(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Proto_alpha
open Alpha_context
open Test_tez
open Test_utils

(**************************************************************************)
(* bootstrap contracts *)
(**************************************************************************)
(* Bootstrap contracts are heavily used in other tests. It is helpful
   to test some properties of these contracts, so we can correctly
   interpret the other tests that use them. *)

let expect_error err = function
  | err0 :: _ when err = err0 -> return_unit
  | _ -> failwith "Unexpected successful result"

let expect_alpha_error err =
  expect_error (Alpha_environment.Ecoproto_error err)

let expect_non_delegatable_contract = function
  | Alpha_environment.Ecoproto_error (Delegate_storage.Non_delegatable_contract _) :: _ ->
      return_unit
  | _ ->
      failwith "Contract is not delegatable and operation should fail."

let expect_no_deletion_pkh pkh = function
  | Alpha_environment.Ecoproto_error (Delegate_storage.No_deletion pkh0) :: _ when pkh0 = pkh ->
      return_unit
  | _ ->
      failwith "Delegate can not be deleted and operation should fail."

(** bootstrap contracts delegate to themselves *)
let bootstrap_manager_is_bootstrap_delegate () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  let bootstrap0 = List.hd bootstrap_contracts in
  Context.Contract.delegate (B b) bootstrap0 >>=? fun delegate0 ->
  Context.Contract.manager (B b) bootstrap0 >>=? fun manager0 ->
  Assert.equal_pkh ~loc:__LOC__ delegate0 manager0.pkh

(** bootstrap contracts cannot change their delegate *)
let bootstrap_delegate_cannot_change ~fee () =
  Context.init 2 >>=? fun (b, bootstrap_contracts) ->
  let bootstrap0 = List.nth bootstrap_contracts 0 in
  let bootstrap1 = List.nth bootstrap_contracts 1 in
  Context.Contract.pkh bootstrap0 >>=? fun pkh1 ->
  Incremental.begin_construction b ~policy:(Block.Excluding [pkh1]) >>=? fun i ->
  Context.Contract.manager (I i) bootstrap1 >>=? fun manager1 ->
  Context.Contract.balance (I i) bootstrap0 >>=? fun balance0 ->
  Context.Contract.delegate (I i) bootstrap0 >>=? fun delegate0 ->
  Op.delegation ~fee (I i) bootstrap0 (Some manager1.pkh) >>=? fun set_delegate ->
  if fee > balance0 then
    Incremental.add_operation i set_delegate >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ -> true
        | _ -> false)
  else
    Incremental.add_operation
      ~expect_failure:expect_non_delegatable_contract i set_delegate >>=? fun i ->
    Incremental.finalize_block i >>=? fun b ->
    (* bootstrap0 still has same delegate *)
    Context.Contract.delegate (B b) bootstrap0 >>=? fun delegate0_after ->
    Assert.equal_pkh ~loc:__LOC__ delegate0 delegate0_after >>=? fun () ->
    (* fee has been debited *)
    Assert.balance_was_debited ~loc:__LOC__ (B b) bootstrap0 balance0 fee


(** bootstrap contracts cannot delete their delegation *)
let bootstrap_delegate_cannot_be_removed ~fee () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  let bootstrap = List.hd bootstrap_contracts in
  Incremental.begin_construction b >>=? fun i ->
  Context.Contract.balance (I i) bootstrap >>=? fun balance ->
  Context.Contract.delegate (I i) bootstrap >>=? fun delegate ->
  Context.Contract.manager (I i) bootstrap >>=? fun manager ->
  Op.delegation ~fee (I i) bootstrap None >>=? fun set_delegate ->
  if fee > balance then
    Incremental.add_operation i set_delegate >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ -> true
        | _ -> false)
  else
    Incremental.add_operation
      ~expect_failure:(expect_no_deletion_pkh manager.pkh) i set_delegate >>=? fun i ->
    (* delegate has not changed *)
    Context.Contract.delegate (I i) bootstrap >>=? fun delegate_after ->
    Assert.equal_pkh ~loc:__LOC__ delegate delegate_after >>=? fun () ->
    (* fee has been debited *)
    Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee

(** bootstrap keys are already registered as delegate keys *)
let bootstrap_manager_already_registered_delegate ~fee () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  Context.Contract.manager (I i) bootstrap >>=? fun manager ->
  let pkh = manager.pkh in
  let impl_contract = Contract.implicit_contract pkh in
  Context.Contract.balance (I i) impl_contract >>=? fun balance ->
  Op.delegation ~fee (I i) impl_contract (Some pkh) >>=? fun sec_reg ->
  if fee > balance then
    begin
      Incremental.add_operation i sec_reg >>= fun err ->
      Assert.proto_error ~loc:__LOC__ err (function
          | Contract_storage.Balance_too_low _ -> true
          | _ -> false)
    end
  else
    begin
      Incremental.add_operation ~expect_failure:(function
          |  Alpha_environment.Ecoproto_error Delegate_storage.Active_delegate :: _ ->
              return_unit
          | _ ->
              failwith "Delegate is already active and operation should fail.")
        i sec_reg >>=? fun i ->
      (* fee has been debited *)
      Assert.balance_was_debited ~loc:__LOC__ (I i) impl_contract balance fee
    end

(** bootstrap manager can be set as delegate of an originated contract
    (through origination operation) *)
let delegate_to_bootstrap_by_origination ~fee () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  Context.Contract.manager (I i) bootstrap >>=? fun manager ->
  Context.Contract.balance (I i) bootstrap >>=? fun balance ->
  (* originate a contract with bootstrap's manager as delegate *)
  Op.origination ~fee ~credit:Tez.zero ~delegate:manager.pkh (I i) bootstrap >>=? fun (op, orig_contract) ->
  Context.get_constants (I i) >>=? fun { parametric = { origination_size ; cost_per_byte ; _ }; _ } -> (* 0.257tz *)
  Tez.(cost_per_byte *? Int64.of_int origination_size) >>?= fun origination_burn ->
  Lwt.return (Tez.(+?) fee origination_burn) >>=? fun total_fee ->
  if fee > balance then
    begin
      Incremental.add_operation i op >>= fun err ->
      Assert.proto_error ~loc:__LOC__ err (function
          | Contract_storage.Balance_too_low _ -> true
          | _ -> false)
    end
  else if total_fee > balance && balance >= fee then
    (* origination did not proceed; fee has been debited *)
    begin
      Incremental.add_operation i ~expect_failure:(function
          |  Alpha_environment.Ecoproto_error Contract.Balance_too_low _ :: _ ->
              return_unit
          | _ ->
              failwith "Not enough balance for origination burn: operation should fail.")
        op >>=? fun i ->
      (* fee was taken *)
      Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee >>=? fun () ->
      (* originated contract has not been created *)
      Context.Contract.balance (I i) orig_contract >>= fun err ->
      Assert.error ~loc:__LOC__ err (function
          | RPC_context.Not_found _ -> true
          | _ -> false)
    end
  else
    (* bootstrap is delegate, fee + origination burn have been debited *)
    begin
      Incremental.add_operation i op >>=? fun i ->
      Context.Contract.delegate (I i) orig_contract >>=? fun delegate ->
      Assert.equal_pkh ~loc:__LOC__ delegate manager.pkh >>=? fun () ->
      Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance total_fee
    end

(** bootstrap manager can be set as delegate of an originated contract
    without initial delegate (through delegation operation) *)
let delegate_to_bootstrap_by_delegation ~fee () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  Context.Contract.manager (I i) bootstrap >>=? fun manager ->
  (* originate a contract with no delegate *)
  Op.origination ~fee:Tez.zero (I i) bootstrap >>=? fun (op, orig_contract) ->
  Incremental.add_operation i op >>=? fun i ->
  Context.Contract.balance (I i) orig_contract >>=? fun orig_balance ->
  (* Format.printf "\nBalance of originated contract: %a\n%!" Tez.pp orig_balance; *)
  (* delegate to bootstrap *)
  Op.delegation ~fee (I i) orig_contract (Some manager.pkh) >>=? fun deleg_op ->
  if fee > orig_balance then
    begin
      Incremental.add_operation i deleg_op >>= fun err ->
      Assert.proto_error ~loc:__LOC__ err (function
          | Contract_storage.Balance_too_low _ -> true
          | _ -> false)
    end
  else
    (* manager is delegate, fee is debited *)
    begin
      Incremental.add_operation i deleg_op >>=? fun i ->
      Context.Contract.delegate (I i) orig_contract >>=? fun delegate ->
      Assert.equal_pkh ~loc:__LOC__ delegate manager.pkh >>=? fun () ->
      Assert.balance_was_debited ~loc:__LOC__ (I i) orig_contract orig_balance fee
    end

(** bootstrap manager can be set as delegate of an originated contract
    with initial delegate (through delegation operation) *)
let delegate_to_bootstrap_by_delegation_switch ~fee () =
  Context.init 2 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap0 = List.hd bootstrap_contracts in
  Context.Contract.manager (I i) bootstrap0 >>=? fun manager0 ->
  let bootstrap1 = List.nth bootstrap_contracts 1 in
  Context.Contract.manager (I i) bootstrap1 >>=? fun manager1 ->
  (* originate a contract with bootstrap1's manager as delegate *)
  Op.origination ~fee:Tez.zero ~credit:Tez.one ~delegate:manager1.pkh (I i) bootstrap0 >>=? fun (op, orig_contract) ->
  Incremental.add_operation i op >>=? fun i ->
  Context.Contract.balance (I i) orig_contract >>=? fun orig_balance ->
  Context.Contract.delegate (I i) orig_contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate manager1.pkh >>=? fun _ ->
  (* switch delegate to bootstrap0 *)
  Op.delegation ~fee (I i) orig_contract (Some manager0.pkh) >>=? fun switch_deleg ->
  if fee > orig_balance then
    begin
      Incremental.add_operation i switch_deleg >>= fun err ->
      Assert.proto_error ~loc:__LOC__ err (function
          | Contract_storage.Balance_too_low _ -> true
          | _ -> false)
    end
  else
    (* manager0 is delegate, fee is debited *)
    begin
      Incremental.add_operation i switch_deleg >>=? fun i ->
      Context.Contract.delegate (I i) orig_contract >>=? fun delegate ->
      Assert.equal_pkh ~loc:__LOC__ delegate manager0.pkh >>=? fun () ->
      Assert.balance_was_debited ~loc:__LOC__ (I i) orig_contract orig_balance fee
    end

let tests_bootstrap_contracts = [
  Test.tztest "bootstrap contracts delegate to themselves" `Quick bootstrap_manager_is_bootstrap_delegate ;
  Test.tztest "bootstrap contracts cannot change their delegate (small fee)" `Quick (bootstrap_delegate_cannot_change ~fee:Tez.one_mutez) ;
  Test.tztest "bootstrap contracts cannot change their delegate (max fee)" `Quick (bootstrap_delegate_cannot_change ~fee:Tez.max_tez) ;
  Test.tztest "bootstrap contracts cannot delete their delegation (small fee)" `Quick (bootstrap_delegate_cannot_be_removed ~fee:Tez.one_mutez) ;
  Test.tztest "bootstrap contracts cannot delete their delegation (max fee)" `Quick (bootstrap_delegate_cannot_be_removed ~fee:Tez.max_tez) ;
  Test.tztest "bootstrap keys are already registered as delegate keys (small fee)" `Quick (bootstrap_manager_already_registered_delegate ~fee:Tez.one_mutez) ;
  Test.tztest "bootstrap keys are already registered as delegate keys (max fee)" `Quick (bootstrap_manager_already_registered_delegate ~fee:Tez.max_tez) ;
  Test.tztest "bootstrap manager can be delegate (init origination, small fee)" `Quick (delegate_to_bootstrap_by_origination ~fee:Tez.one_mutez) ;
  (* balance enough for fee but not for fee + origination burn *)
  Test.tztest "bootstrap manager can be delegate (init origination, edge case)" `Quick (delegate_to_bootstrap_by_origination ~fee:(Tez.of_mutez_exn 3_999_999_743_000L)) ;
  (* fee bigger than bootstrap's initial balance*)
  Test.tztest "bootstrap manager can be delegate (init origination, large fee)" `Quick (delegate_to_bootstrap_by_origination ~fee:(Tez.of_int 10_000_000)) ;
  Test.tztest "bootstrap manager can be delegate (init delegation, small fee)" `Quick (delegate_to_bootstrap_by_delegation ~fee:Tez.one_mutez) ;
  Test.tztest "bootstrap manager can be delegate (init delegation, max fee)" `Quick (delegate_to_bootstrap_by_delegation ~fee:Tez.max_tez) ;
  Test.tztest "bootstrap manager can be delegate (switch delegation, small fee)" `Quick (delegate_to_bootstrap_by_delegation_switch ~fee:Tez.one_mutez) ;
  Test.tztest "bootstrap manager can be delegate (switch delegation, max fee)" `Quick (delegate_to_bootstrap_by_delegation_switch ~fee:Tez.max_tez) ;
]

(**************************************************************************)
(* delegate registration *)
(**************************************************************************)
(* A delegate is a pkh. Delegates must be registered. Registration is
   done via the self-delegation of the implicit contract corresponding
   to the pkh. The implicit contract must be credited when the
   self-delegation is done. Furthermore, trying to register an already
   registered key raises an error.

   In this series of tests, we verify that
   1- unregistered delegate keys cannot be delegated to,
   2- registered keys can be delegated to,
   3- registering an already registered key raises an error.


   We consider three scenarios for setting a delegate:
   - through origination,
   - through delegation when the originated contract has no delegate yet,
   - through delegation when the originated contract already has a delegate.

   We also test that emptying the implicit contract linked to a
   registered delegate key does not unregister the delegate key.
*)

(*
   Valid registration

   Unregistered key:
   - contract not credited and no self-delegation
   - contract credited but no self-delegation
   - contract not credited and self-delegation

Not credited:
- no credit operation
- credit operation of 1μꜩ and then debit operation of 1μꜩ

*)

(** A- unregistered delegate keys cannot be used for delegation *)

(* Two main series of tests: without self-delegation, and with a failed attempt at self-delegation
   1- no self-delegation
     a- no credit
        - no token transfer
        - credit of 1μꜩ and then debit of 1μꜩ
     b- with credit of 1μꜩ.
   For every scenario, we try three different ways of delegating:
   - through origination (init origination)
   - through delegation when no delegate was assigned at origination (init delegation)
   - through delegation when a delegate was assigned at origination (switch delegation).

   2- Self-delegation fails if the contract has no credit. We try the
   two possibilities of 1a for non-credited contracts.
*)

let expect_unregistered_key pkh = function
  | Alpha_environment.Ecoproto_error Roll_storage.Unregistered_delegate pkh0 :: _
    when pkh = pkh0 -> return_unit
  | _ -> failwith "Delegate key is not registered: operation should fail."

(* A1: no self-delegation *)
(* no token transfer, no self-delegation *)
let unregistered_delegate_key_init_origination ~fee () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  (* origination with delegate argument *)
  Op.origination ~fee ~delegate:unregistered_pkh (I i) bootstrap >>=? fun (op, orig_contract) ->
  Context.get_constants (I i) >>=? fun { parametric = { origination_size ; cost_per_byte ; _ }; _ } ->
  Tez.(cost_per_byte *? Int64.of_int origination_size) >>?= fun origination_burn ->
  Lwt.return (Tez.(+?) fee origination_burn) >>=? fun _total_fee -> (* FIXME unused variable *)
  Context.Contract.balance (I i) bootstrap >>=? fun balance ->
  if fee > balance then
    begin
      Incremental.add_operation i op >>= fun err ->
      Assert.proto_error ~loc:__LOC__ err (function
          | Contract_storage.Balance_too_low _ -> true
          | _ -> false)
    end
  else
    (* origination did not proceed; fee has been debited *)
    begin
      Incremental.add_operation
        ~expect_failure:(expect_unregistered_key unregistered_pkh)
        i op >>=? fun i ->
      Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee >>=? fun () ->
      (* originated contract has not been created *)
      Context.Contract.balance (I i) orig_contract >>= fun err ->
      Assert.error ~loc:__LOC__ err (function
          | RPC_context.Not_found _ -> true
          | _ -> false)
    end

let unregistered_delegate_key_init_delegation ~fee () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  Context.Contract.balance (I i) bootstrap >>=? fun _balance -> (* FIXME unused variable *)
  (* origination without delegate argument *)
  let credit = Tez.of_int 10 in
  Op.origination (I i) bootstrap ~credit  >>=? fun (op, orig_contract) ->
  Incremental.add_operation i op >>=? fun i ->
  Op.delegation ~fee (I i) orig_contract (Some unregistered_pkh) >>=? fun delegate_op ->
  if fee > credit then
    begin
      Incremental.add_operation i delegate_op >>= fun err ->
      Assert.proto_error ~loc:__LOC__ err (function
          | Contract_storage.Balance_too_low _ -> true
          | _ -> false)
    end
  else
    (* fee has been debited; no delegate *)
    begin
      Incremental.add_operation i
        ~expect_failure:(expect_unregistered_key unregistered_pkh)
        delegate_op >>=? fun i ->
      Assert.balance_was_debited ~loc:__LOC__ (I i) orig_contract credit fee >>=? fun () ->
      (* originated contract has no delegate *)
      Context.Contract.delegate (I i) orig_contract >>= fun err ->
      Assert.error ~loc:__LOC__ err (function
          | RPC_context.Not_found _ -> true
          | _ -> false)
    end

let unregistered_delegate_key_switch_delegation ~fee () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  (* origination with delegate setting *)
  Context.Contract.manager (I i) bootstrap >>=? fun manager ->
  let credit = Tez.of_int 10 in
  Op.origination (I i) ~delegate:manager.pkh bootstrap ~credit >>=? fun (op, orig_contract) ->
  Incremental.add_operation i op >>=? fun i ->
  Context.Contract.delegate (I i) orig_contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate manager.pkh >>=? fun _ ->
  (* switch delegate through delegation *)
  Op.delegation ~fee (I i) orig_contract (Some unregistered_pkh) >>=? fun delegate_op ->
  if fee > credit then
    begin
      Incremental.add_operation i delegate_op >>= fun err ->
      Assert.proto_error ~loc:__LOC__ err (function
          | Contract_storage.Balance_too_low _ -> true
          | _ -> false)
    end
  else
    (* fee has been debited; no delegate *)
    begin
      Incremental.add_operation i
        ~expect_failure:(expect_unregistered_key unregistered_pkh)
        delegate_op >>=? fun i ->
      Assert.balance_was_debited ~loc:__LOC__ (I i) orig_contract credit fee >>=? fun () ->
      (* originated contract's delegate has not changed *)
      Context.Contract.delegate (I i) orig_contract >>=? fun delegate ->
      Assert.not_equal_pkh ~loc:__LOC__ delegate unregistered_pkh >>=? fun () ->
      Assert.equal_pkh ~loc:__LOC__ delegate manager.pkh
    end

(* credit of some amount, no self-delegation *)
let unregistered_delegate_key_init_origination_credit ~fee ~amount () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  (* credit + check balance *)
  Op.transaction ~fee:Tez.zero (I i) bootstrap impl_contract amount >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount >>=? fun _ ->
  (* origination with delegate argument *)
  Context.Contract.balance (I i) bootstrap >>=? fun balance ->
  Op.origination ~fee ~delegate:unregistered_pkh (I i) bootstrap >>=? fun (op, orig_contract) ->
  if fee > balance then
    begin
      Incremental.add_operation i op >>= fun err ->
      Assert.proto_error ~loc:__LOC__ err (function
          | Contract_storage.Balance_too_low _ -> true
          | _ -> false)
    end
  else (* origination not done, fee taken *)
    begin
      Incremental.add_operation
        ~expect_failure:(expect_unregistered_key unregistered_pkh)
        i op >>=? fun i ->
      Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee >>=? fun () ->
      Context.Contract.balance (I i) orig_contract >>= fun err ->
      Assert.error ~loc:__LOC__ err (function
          | RPC_context.Not_found _ -> true
          | _ -> false)
    end

let unregistered_delegate_key_init_delegation_credit ~fee ~amount () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  (* credit + check balance *)
  Op.transaction ~fee:Tez.zero (I i) bootstrap impl_contract amount >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount >>=? fun _ ->
  (* origination without delegate argument *)
  let credit = Tez.of_int 10 in
  Op.origination ~fee:Tez.zero ~credit (I i) bootstrap >>=? fun (op, contract) ->
  Incremental.add_operation i op >>=? fun i ->
  Op.delegation ~fee (I i) contract (Some unregistered_pkh) >>=? fun delegate_op ->
  if fee > credit then
    Incremental.add_operation i delegate_op >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ -> true
        | _ -> false)
  else
    begin
      (* fee has been taken, no delegate for contract *)
      Incremental.add_operation
        ~expect_failure:(expect_unregistered_key unregistered_pkh)
        i delegate_op >>=? fun i ->
      Assert.balance_was_debited ~loc:__LOC__ (I i) contract credit fee >>=? fun () ->
      Context.Contract.delegate (I i) contract >>= fun err ->
      Assert.error ~loc:__LOC__ err (function
          | RPC_context.Not_found _ -> true
          | _ -> false)
    end

let unregistered_delegate_key_switch_delegation_credit ~fee ~amount () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  (* credit + check balance *)
  Op.transaction ~fee:Tez.zero (I i) bootstrap impl_contract amount >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount >>=? fun _ ->
  (* origination without delegate setting *)
  Context.Contract.manager (I i) bootstrap >>=? fun manager ->
  let credit = Tez.of_int 10 in
  Op.origination (I i) ~fee:Tez.zero ~credit ~delegate:manager.pkh bootstrap >>=? fun (op, contract) ->
  Incremental.add_operation i op >>=? fun i ->
  Context.Contract.delegate (I i) contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate manager.pkh >>=? fun _ ->
  (* switch delegate through delegation *)
  Op.delegation ~fee (I i) contract (Some unregistered_pkh) >>=? fun delegate_op ->
  if fee > credit then
    Incremental.add_operation i delegate_op >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ -> true
        | _ -> false)
  else
    begin
      (* fee has been taken, delegate for contract has not changed *)
      Incremental.add_operation
        ~expect_failure:(expect_unregistered_key unregistered_pkh)
        i delegate_op >>=? fun i ->
      Assert.balance_was_debited ~loc:__LOC__ (I i) contract credit fee >>=? fun () ->
      Context.Contract.delegate (I i) contract >>=? fun delegate ->
      Assert.not_equal_pkh ~loc:__LOC__ delegate unregistered_pkh >>=? fun () ->
      Assert.equal_pkh ~loc:__LOC__ delegate manager.pkh
    end

(* a credit of some amount followed by a debit of the same amount, no self-delegation *)
let unregistered_delegate_key_init_origination_credit_debit ~fee ~amount () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  (* credit + check balance *)
  Op.transaction (I i) bootstrap impl_contract amount >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount >>=? fun _ ->
  (* debit + check balance *)
  Op.transaction (I i) impl_contract bootstrap amount >>=? fun debit_contract ->
  Incremental.add_operation i debit_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.zero >>=? fun _ ->
  (* origination with delegate argument *)
  Context.Contract.balance (I i) bootstrap >>=? fun balance ->
  Op.origination ~fee ~delegate:unregistered_pkh (I i) bootstrap >>=? fun (op, orig_contract) ->
  if fee > balance then
    begin
      Incremental.add_operation i op  >>= fun err ->
      Assert.proto_error ~loc:__LOC__ err (function
          | Contract_storage.Balance_too_low _ -> true
          | _ -> false)
    end
  else (* fee taken, origination not processed *)
    begin
      Incremental.add_operation
        ~expect_failure:(expect_unregistered_key unregistered_pkh)
        i op >>=? fun i ->
      Assert.balance_was_debited ~loc:__LOC__ (I i) bootstrap balance fee >>=? fun () ->
      Context.Contract.balance (I i) orig_contract >>= fun err ->
      Assert.error ~loc:__LOC__ err (function
          | RPC_context.Not_found _ -> true
          | _ -> false)
    end

let unregistered_delegate_key_init_delegation_credit_debit ~amount ~fee () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  (* credit + check balance *)
  Op.transaction ~fee:Tez.zero (I i) bootstrap impl_contract amount >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount >>=? fun _ ->
  (* debit + check balance *)
  Op.transaction ~fee:Tez.zero (I i) impl_contract bootstrap amount >>=? fun debit_contract ->
  Incremental.add_operation i debit_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.zero >>=? fun _ ->
  (* origination without delegate argument *)
  let credit = Tez.of_int 10 in
  Op.origination ~fee:Tez.zero (I i) ~credit bootstrap >>=? fun (op, contract) ->
  Incremental.add_operation i op >>=? fun i ->
  (* set a delegate with delegation operation *)
  Op.delegation ~fee (I i) contract (Some unregistered_pkh) >>=? fun delegate_op ->
  if fee > credit then
    begin
      Incremental.add_operation i delegate_op >>= fun err ->
      Assert.proto_error ~loc:__LOC__ err (function
          | Contract_storage.Balance_too_low _ -> true
          | _ -> false)
    end
  else
    begin
      (* fee has been taken, no delegate for contract *)
      Incremental.add_operation
        ~expect_failure:(expect_unregistered_key unregistered_pkh)
        i delegate_op >>=? fun i ->
      Assert.balance_was_debited ~loc:__LOC__ (I i) contract credit fee >>=? fun () ->
      Context.Contract.delegate (I i) contract >>= fun err ->
      Assert.error ~loc:__LOC__ err (function
          | RPC_context.Not_found _ -> true
          | _ -> false)
    end

let unregistered_delegate_key_switch_delegation_credit_debit ~fee ~amount () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let unregistered_account = Account.new_account () in
  let unregistered_pkh = Account.(unregistered_account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  (* credit + check balance *)
  Op.transaction ~fee:Tez.zero (I i) bootstrap impl_contract amount >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount >>=? fun _ ->
  (* debit + check balance *)
  Op.transaction (I i) impl_contract bootstrap amount >>=? fun debit_contract ->
  Incremental.add_operation i debit_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.zero >>=? fun _ ->
  (* origination with delegate setting *)
  Context.Contract.manager (I i) bootstrap >>=? fun manager ->
  let credit = Tez.of_int 10 in
  Op.origination (I i) ~fee:Tez.zero ~credit ~delegate:manager.pkh bootstrap >>=? fun (op, contract) ->
  Incremental.add_operation i op >>=? fun i ->
  Context.Contract.delegate (I i) contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate manager.pkh >>=? fun _ ->
  (* switch delegate through delegation *)
  Op.delegation (I i) ~fee contract (Some unregistered_pkh) >>=? fun delegate_op ->
  if fee > credit then
    Incremental.add_operation i delegate_op >>= fun err ->
    Assert.proto_error ~loc:__LOC__ err (function
        | Contract_storage.Balance_too_low _ -> true
        | _ -> false)
  else
    begin
      (* fee has been taken, delegate for contract has not changed *)
      Incremental.add_operation
        ~expect_failure:(expect_unregistered_key unregistered_pkh)
        i delegate_op >>=? fun i ->
      Assert.balance_was_debited ~loc:__LOC__ (I i) contract credit fee >>=? fun () ->
      Context.Contract.delegate (I i) contract >>=? fun delegate ->
      Assert.not_equal_pkh ~loc:__LOC__ delegate unregistered_pkh >>=? fun () ->
      Assert.equal_pkh ~loc:__LOC__ delegate manager.pkh
    end

(* A2- self-delegation to an empty contract fails *)
let failed_self_delegation_no_transaction () =
  Context.init 1 >>=? fun (b, _) ->
  Incremental.begin_construction b >>=? fun i ->
  let account = Account.new_account () in
  let unregistered_pkh = Account.(account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  (* check balance *)
  Context.Contract.balance (I i) impl_contract >>=? fun balance ->
  Assert.equal_tez ~loc:__LOC__ Tez.zero balance >>=? fun _ ->
  (* self delegation fails *)
  Op.delegation (I i) impl_contract (Some unregistered_pkh) >>=? fun self_delegation ->
  Incremental.add_operation i self_delegation >>= fun err ->
  Assert.proto_error ~loc:__LOC__ err (function
      | Contract_storage.Empty_implicit_contract pkh ->
          if pkh = unregistered_pkh then true else false
      | _ -> false)

let failed_self_delegation_emptied_implicit_contract amount () =
  (* create an implicit contract *)
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let account = Account.new_account () in
  let unregistered_pkh = Account.(account.pkh) in
  let impl_contract = Contract.implicit_contract unregistered_pkh in
  (*  credit implicit contract and check balance *)
  Op.transaction (I i) bootstrap impl_contract amount >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount >>=? fun _ ->
  (* empty implicit contract and check balance *)
  Op.transaction (I i) impl_contract bootstrap amount >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.zero >>=? fun _ ->
  (* self delegation fails *)
  Op.delegation (I i) impl_contract (Some unregistered_pkh) >>=? fun self_delegation ->
  Incremental.add_operation i self_delegation >>= fun err ->
  Assert.proto_error ~loc:__LOC__ err (function
      | Contract_storage.Empty_implicit_contract pkh ->
          if pkh = unregistered_pkh then true else false
      | _ -> false)

(** B- valid registration:
    - credit implicit contract with some ꜩ + verification of balance
    - self delegation + verification
    - empty contract + verification of balance + verification of not being erased / self-delegation
    - originate contract w implicit contract as delegate + verification of delegation *)
let valid_delegate_registration_init_origination_credit amount () =
  (* create an implicit contract *)
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let delegate_account = Account.new_account () in
  let delegate_pkh = Account.(delegate_account.pkh) in
  let impl_contract = Contract.implicit_contract delegate_pkh in
  (* credit > 0ꜩ + check balance *)
  Op.transaction (I i) bootstrap impl_contract amount >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount >>=? fun _ ->
  (* self delegation + verification *)
  Op.delegation (I i) impl_contract (Some delegate_pkh) >>=? fun self_delegation ->
  Incremental.add_operation i self_delegation >>=? fun i ->
  Context.Contract.delegate (I i) impl_contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate delegate_pkh >>=? fun _ ->
  (* originating a contract with the newly registered delegate account as delegate *)
  Op.origination ~delegate:delegate_account.pkh (I i) bootstrap >>=? fun (op, orig_contract) ->
  Incremental.add_operation i op >>=? fun i ->
  Context.Contract.delegate (I i) orig_contract >>=? fun orig_delegate ->
  Assert.equal_pkh ~loc:__LOC__ orig_delegate delegate_pkh

let valid_delegate_registration_init_delegation_credit amount () =
  (* create an implicit contract *)
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let delegate_account = Account.new_account () in
  let delegate_pkh = Account.(delegate_account.pkh) in
  let impl_contract = Contract.implicit_contract delegate_pkh in
  (* credit > 0ꜩ + check balance *)
  Op.transaction (I i) bootstrap impl_contract amount >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount >>=? fun _ ->
  (* self delegation + verification *)
  Op.delegation (I i) impl_contract (Some delegate_pkh) >>=? fun self_delegation ->
  Incremental.add_operation i self_delegation >>=? fun i ->
  Context.Contract.delegate (I i) impl_contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate delegate_pkh >>=? fun _ ->
  (* originating a contract with no delegate *)
  Op.origination (I i) bootstrap >>=? fun (op, orig_contract) ->
  Incremental.add_operation i op >>=? fun i ->
  (* check no delegate for orig contract *)
  Context.Contract.delegate (I i) orig_contract >>= fun err ->
  Assert.error ~loc:__LOC__ err (function
      | RPC_context.Not_found _ -> true
      | _ -> false) >>=? fun _ ->
  (* delegation to the newly registered key *)
  Op.delegation (I i) orig_contract (Some delegate_account.pkh) >>=? fun delegation ->
  Incremental.add_operation i delegation >>=? fun i ->
  (* check delegation *)
  Context.Contract.delegate (I i) orig_contract >>=? fun orig_delegate ->
  Assert.equal_pkh ~loc:__LOC__ orig_delegate delegate_pkh

let valid_delegate_registration_switch_delegation_credit amount () =
  (* create an implicit contract *)
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let delegate_account = Account.new_account () in
  let delegate_pkh = Account.(delegate_account.pkh) in
  let impl_contract = Contract.implicit_contract delegate_pkh in
  (* credit > 0ꜩ + check balance *)
  Op.transaction (I i) bootstrap impl_contract amount >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount >>=? fun _ ->
  (* self delegation + verification *)
  Op.delegation (I i) impl_contract (Some delegate_pkh) >>=? fun self_delegation ->
  Incremental.add_operation i self_delegation >>=? fun i ->
  Context.Contract.delegate (I i) impl_contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate delegate_pkh >>=? fun _ ->
  (* originating a contract with bootstrap's account as delegate *)
  Context.Contract.manager (I i) bootstrap >>=? fun bootstrap_manager ->
  Op.origination (I i) ~delegate:bootstrap_manager.pkh bootstrap >>=? fun (op, orig_contract) ->
  Incremental.add_operation i op >>=? fun i ->
  (* test delegate of new contract is bootstrap *)
  Context.Contract.delegate (I i) orig_contract >>=? fun orig_delegate ->
  Assert.equal_pkh ~loc:__LOC__ orig_delegate bootstrap_manager.pkh  >>=? fun _ ->
  (* delegation with newly registered key *)
  Op.delegation (I i) orig_contract (Some delegate_account.pkh) >>=? fun delegation ->
  Incremental.add_operation i delegation >>=? fun i ->
  Context.Contract.delegate (I i) orig_contract >>=? fun orig_delegate ->
  Assert.equal_pkh ~loc:__LOC__ orig_delegate delegate_pkh

let valid_delegate_registration_init_origination_credit_debit amount () =
  (* create an implicit contract *)
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let delegate_account = Account.new_account () in
  let delegate_pkh = Account.(delegate_account.pkh) in
  let impl_contract = Contract.implicit_contract delegate_pkh in
  (* credit > 0ꜩ+ check balance *)
  Op.transaction (I i) bootstrap impl_contract amount >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount >>=? fun _ ->
  (* self delegation + verification *)
  Op.delegation (I i) impl_contract (Some delegate_pkh) >>=? fun self_delegation ->
  Incremental.add_operation i self_delegation >>=? fun i ->
  Context.Contract.delegate (I i) impl_contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate_pkh delegate >>=? fun _ ->
  (* empty implicit contracts are usually deleted but they are kept if
     they were registered as delegates. we empty the contract in
     order to verify this. *)
  Op.transaction (I i) impl_contract bootstrap amount >>=? fun empty_contract ->
  Incremental.add_operation i empty_contract >>=? fun i ->
  (* impl_contract is empty *)
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.zero >>=? fun _ ->
  (* verify self-delegation after contract is emptied *)
  Context.Contract.delegate (I i) impl_contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate_pkh delegate >>=? fun _ ->
  (* originating a contract with the newly registered delegate account as delegate *)
  Op.origination ~delegate:delegate_account.pkh (I i) bootstrap >>=? fun (op, orig_contract) ->
  Incremental.add_operation i op >>=? fun i ->
  Context.Contract.delegate (I i) orig_contract >>=? fun orig_delegate ->
  Assert.equal_pkh ~loc:__LOC__ orig_delegate delegate_pkh

let valid_delegate_registration_init_delegation_credit_debit amount () =
  (* create an implicit contract *)
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let delegate_account = Account.new_account () in
  let delegate_pkh = Account.(delegate_account.pkh) in
  let impl_contract = Contract.implicit_contract delegate_pkh in
  (* credit > 0ꜩ+ check balance *)
  Op.transaction (I i) bootstrap impl_contract amount >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount >>=? fun _ ->
  (* self delegation + verification *)
  Op.delegation (I i) impl_contract (Some delegate_pkh) >>=? fun self_delegation ->
  Incremental.add_operation i self_delegation >>=? fun i ->
  Context.Contract.delegate (I i) impl_contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate_pkh delegate >>=? fun _ ->
  (* empty implicit contracts are usually deleted but they are kept if
     they were registered as delegates. we empty the contract in
     order to verify this. *)
  Op.transaction (I i) impl_contract bootstrap amount >>=? fun empty_contract ->
  Incremental.add_operation i empty_contract >>=? fun i ->
  (* impl_contract is empty *)
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.zero >>=? fun _ ->
  (* verify self-delegation after contract is emptied *)
  Context.Contract.delegate (I i) impl_contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate_pkh delegate >>=? fun _ ->
  (* originating a contract with no delegate *)
  Op.origination (I i) bootstrap >>=? fun (op, orig_contract) ->
  Incremental.add_operation i op >>=? fun i ->
  (* check no delegate for orig contract *)
  Context.Contract.delegate (I i) orig_contract >>= fun err ->
  Assert.error ~loc:__LOC__ err (function
      | RPC_context.Not_found _ -> true
      | _ -> false) >>=? fun _ ->
  (* delegation to the newly registered key *)
  Op.delegation (I i) orig_contract (Some delegate_account.pkh) >>=? fun delegation ->
  Incremental.add_operation i delegation >>=? fun i ->
  (* check delegation *)
  Context.Contract.delegate (I i) orig_contract >>=? fun orig_delegate ->
  Assert.equal_pkh ~loc:__LOC__ orig_delegate delegate_pkh

let valid_delegate_registration_switch_delegation_credit_debit amount () =
  (* create an implicit contract *)
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let delegate_account = Account.new_account () in
  let delegate_pkh = Account.(delegate_account.pkh) in
  let impl_contract = Contract.implicit_contract delegate_pkh in
  (* credit > 0ꜩ + check balance *)
  Op.transaction (I i) bootstrap impl_contract amount >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract amount >>=? fun _ ->
  (* self delegation + verification *)
  Op.delegation (I i) impl_contract (Some delegate_pkh) >>=? fun self_delegation ->
  Incremental.add_operation i self_delegation >>=? fun i ->
  Context.Contract.delegate (I i) impl_contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate_pkh delegate >>=? fun _ ->
  (* empty implicit contracts are usually deleted but they are kept if
     they were registered as delegates. we empty the contract in
     order to verify this. *)
  Op.transaction (I i) impl_contract bootstrap amount >>=? fun empty_contract ->
  Incremental.add_operation i empty_contract >>=? fun i ->
  (* impl_contract is empty *)
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.zero >>=? fun _ ->
  (* originating a contract with bootstrap's account as delegate *)
  Context.Contract.manager (I i) bootstrap >>=? fun bootstrap_manager ->
  Op.origination (I i) ~delegate:bootstrap_manager.pkh bootstrap >>=? fun (op, orig_contract) ->
  Incremental.add_operation i op >>=? fun i ->
  (* test delegate of new contract is bootstrap *)
  Context.Contract.delegate (I i) orig_contract >>=? fun orig_delegate ->
  Assert.equal_pkh ~loc:__LOC__ orig_delegate bootstrap_manager.pkh  >>=? fun _ ->
  (* delegation with newly registered key *)
  Op.delegation (I i) orig_contract (Some delegate_account.pkh) >>=? fun delegation ->
  Incremental.add_operation i delegation >>=? fun i ->
  Context.Contract.delegate (I i) orig_contract >>=? fun orig_delegate ->
  Assert.equal_pkh ~loc:__LOC__ orig_delegate delegate_pkh

(** C- a second self-delegation should raise an `Active_delegate` error *)
(* with implicit contract with some credit *)
let double_registration () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let account = Account.new_account () in
  let pkh = Account.(account.pkh) in
  let impl_contract = Contract.implicit_contract pkh in
  (* credit 1μꜩ+ check balance *)
  Op.transaction (I i) bootstrap impl_contract (Tez.one_mutez) >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.one_mutez >>=? fun _ ->
  (* self-delegation *)
  Op.delegation (I i) impl_contract (Some pkh) >>=? fun self_delegation ->
  Incremental.add_operation i self_delegation >>=? fun i ->
  (* second self-delegation *)
  Op.delegation (I i) impl_contract (Some pkh) >>=? fun second_registration ->
  Incremental.add_operation i second_registration >>= fun err ->

  Assert.proto_error ~loc:__LOC__ err (function
      | Delegate_storage.Active_delegate -> true
      | _ -> false)

(* with implicit contract emptied after first self-delegation  *)
let double_registration_when_empty () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let account = Account.new_account () in
  let pkh = Account.(account.pkh) in
  let impl_contract = Contract.implicit_contract pkh in
  (* credit 1μꜩ+ check balance *)
  Op.transaction (I i) bootstrap impl_contract Tez.one_mutez >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.one_mutez >>=? fun _ ->
  (* self delegation *)
  Op.delegation (I i) impl_contract (Some pkh) >>=? fun self_delegation ->
  Incremental.add_operation i self_delegation >>=? fun i ->
  (* empty the delegate account *)
  Op.transaction (I i) impl_contract bootstrap Tez.one_mutez >>=? fun empty_contract ->
  Incremental.add_operation i empty_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.zero >>=? fun _ ->
  (* second self-delegation *)
  Op.delegation (I i) impl_contract (Some pkh) >>=? fun second_registration ->
  Incremental.add_operation i second_registration >>= fun err ->

  Assert.proto_error ~loc:__LOC__ err (function
      | Delegate_storage.Active_delegate -> true
      | _ -> false)

(* with implicit contract emptied then recredited after first self-delegation  *)
let double_registration_when_recredited () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let account = Account.new_account () in
  let pkh = Account.(account.pkh) in
  let impl_contract = Contract.implicit_contract pkh in
  (* credit 1μꜩ+ check balance *)
  Op.transaction (I i) bootstrap impl_contract Tez.one_mutez >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.one_mutez >>=? fun _ ->
  (* self delegation *)
  Op.delegation (I i) impl_contract (Some pkh) >>=? fun self_delegation ->
  Incremental.add_operation i self_delegation >>=? fun i ->
  (* empty the delegate account *)
  Op.transaction (I i) impl_contract bootstrap Tez.one_mutez >>=? fun empty_contract ->
  Incremental.add_operation i empty_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.zero >>=? fun _ ->
  (* credit 1μꜩ+ check balance *)
  Op.transaction (I i) bootstrap impl_contract Tez.one_mutez >>=? fun create_contract ->
  Incremental.add_operation i create_contract >>=? fun i ->
  Assert.balance_is ~loc:__LOC__ (I i) impl_contract Tez.one_mutez >>=? fun _ ->
  (* second self-delegation *)
  Op.delegation (I i) impl_contract (Some pkh) >>=? fun second_registration ->
  Incremental.add_operation i second_registration >>= fun err ->

  Assert.proto_error ~loc:__LOC__ err (function
      | Delegate_storage.Active_delegate -> true
      | _ -> false)

(* originate and self-delegation on unrevealed contract *)
let unregistered_and_unrevealed_self_delegate_key_init_origination ~fee () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let { Account.pkh ; _ }  = Account.new_account () in
  let contract = Alpha_context.Contract.implicit_contract pkh in
  Op.transaction (I i) bootstrap contract (Tez.of_int 10) >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  (* origination with delegate argument *)
  Op.origination ~fee ~delegate:pkh (I i) contract >>=? fun (op, orig_contract) ->
  Context.Contract.balance (I i) contract >>=? fun balance ->
  if fee > balance then
    begin
      Incremental.add_operation i op >>= fun err ->
      Assert.proto_error ~loc:__LOC__ err (function
          | Contract_storage.Balance_too_low _ -> true
          | _ -> false)
    end
  else
    (* origination did not proceed; fee has been debited *)
    begin
      Incremental.add_operation
        ~expect_failure:(expect_unregistered_key pkh)
        i op >>=? fun i ->
      Assert.balance_was_debited ~loc:__LOC__ (I i) contract balance fee >>=? fun () ->
      (* originated contract has not been created *)
      Context.Contract.balance (I i) orig_contract >>= fun err ->
      Assert.error ~loc:__LOC__ err (function
          | RPC_context.Not_found _ -> true
          | _ -> false)
    end

(* originate and self-delegation on revelead but not registered contract *)
let unregistered_and_revealed_self_delegate_key_init_origination ~fee () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let { Account.pkh ; pk ; _ }  = Account.new_account () in
  let contract = Alpha_context.Contract.implicit_contract pkh in
  Op.transaction (I i) bootstrap contract (Tez.of_int 10) >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  Op.revelation (I i) pk >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  (* origination with delegate argument *)
  Op.origination ~fee ~delegate:pkh (I i) contract >>=? fun (op, orig_contract) ->
  Context.Contract.balance (I i) contract >>=? fun balance ->
  if fee > balance then
    begin
      Incremental.add_operation i op >>= fun err ->
      Assert.proto_error ~loc:__LOC__ err (function
          | Contract_storage.Balance_too_low _ -> true
          | _ -> false)
    end
  else
    (* origination did not proceed; fee has been debited *)
    begin
      Incremental.add_operation
        ~expect_failure:(expect_unregistered_key pkh)
        i op >>=? fun i ->
      Assert.balance_was_debited ~loc:__LOC__ (I i) contract balance fee >>=? fun () ->
      (* originated contract has not been created *)
      Context.Contract.balance (I i) orig_contract >>= fun err ->
      Assert.error ~loc:__LOC__ err (function
          | RPC_context.Not_found _ -> true
          | _ -> false)
    end

(* originate and self-delegation on revealed and registered contract *)
let registered_self_delegate_key_init_origination () =
  Context.init 1 >>=? fun (b, bootstrap_contracts) ->
  Incremental.begin_construction b >>=? fun i ->
  let bootstrap = List.hd bootstrap_contracts in
  let { Account.pkh ; pk ; _ }  = Account.new_account () in
  let contract = Alpha_context.Contract.implicit_contract pkh in
  Op.transaction (I i) bootstrap contract (Tez.of_int 10) >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  Op.revelation (I i) pk >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  Op.delegation (I i) contract (Some pkh) >>=? fun op ->
  Incremental.add_operation i op >>=? fun i ->
  Context.Contract.balance (I i) contract >>=? fun balance ->
  Context.get_constants (I i) >>=? fun { parametric = { origination_size ; cost_per_byte ; _ } ; _ } ->
  Tez.(cost_per_byte *? Int64.of_int origination_size) >>?= fun origination_burn ->
  (* origination with delegate argument *)
  Op.origination ~delegate:pkh ~credit:Tez.one (I i) contract >>=? fun (op, orig_contract) ->
  Tez.(origination_burn +? Tez.one) >>?= fun total_cost ->
  Incremental.add_operation i op >>=? fun i ->
  Assert.balance_was_debited ~loc:__LOC__ (I i) contract balance total_cost >>=? fun () ->
  Assert.balance_is ~loc:__LOC__ (I i) orig_contract Tez.one >>=? fun () ->
  return_unit

let tests_delegate_registration =
  [
    (*** unregistered delegate key: no self-delegation ***)
    (* no token transfer, no self-delegation *)
    Test.tztest "unregistered delegate key (origination, small fee)"
      `Quick (unregistered_delegate_key_init_origination ~fee:Tez.one_mutez);
    Test.tztest "unregistered delegate key (origination, edge case fee)"
      `Quick (unregistered_delegate_key_init_origination ~fee:(Tez.of_int 3_999_488));
    Test.tztest "unregistered delegate key (origination, large fee)"
      `Quick (unregistered_delegate_key_init_origination ~fee:(Tez.of_int 10_000_000));

    Test.tztest "unregistered delegate key (init with delegation, small fee)"
      `Quick (unregistered_delegate_key_init_delegation ~fee:Tez.one_mutez);
    Test.tztest "unregistered delegate key (init with delegation, max fee)"
      `Quick (unregistered_delegate_key_init_delegation ~fee:Tez.max_tez);

    Test.tztest "unregistered delegate key (switch with delegation, small fee)"
      `Quick (unregistered_delegate_key_switch_delegation ~fee:Tez.one_mutez) ;
    Test.tztest "unregistered delegate key (switch with delegation, max fee)"
      `Quick (unregistered_delegate_key_switch_delegation ~fee:Tez.max_tez) ;

    (* credit/debit 1μꜩ, no self-delegation *)
    Test.tztest "unregistered delegate key - credit/debit 1μꜩ (origination, small fee)"
      `Quick (unregistered_delegate_key_init_origination_credit_debit ~fee:Tez.one_mutez ~amount:Tez.one_mutez) ;
    Test.tztest "unregistered delegate key - credit/debit 1μꜩ (origination, large fee)"
      `Quick (unregistered_delegate_key_init_origination_credit_debit ~fee:Tez.max_tez ~amount:Tez.one_mutez) ;

    Test.tztest "unregistered delegate key - credit/debit 1μꜩ (init with delegation, small fee)"
      `Quick (unregistered_delegate_key_init_delegation_credit_debit ~amount:Tez.one_mutez ~fee:Tez.one_mutez) ;
    Test.tztest "unregistered delegate key - credit/debit 1μꜩ (init with delegation, large fee)"
      `Quick (unregistered_delegate_key_init_delegation_credit_debit ~amount:Tez.one_mutez ~fee:Tez.max_tez) ;

    Test.tztest "unregistered delegate key - credit/debit 1μꜩ (switch with delegation, small fee)"
      `Quick (unregistered_delegate_key_switch_delegation_credit_debit ~amount:Tez.one_mutez ~fee:Tez.one_mutez) ;
    Test.tztest "unregistered delegate key - credit/debit 1μꜩ (switch with delegation, large fee)"
      `Quick (unregistered_delegate_key_switch_delegation_credit_debit ~amount:Tez.one_mutez ~fee:Tez.max_tez) ;

    (* credit 1μꜩ, no self-delegation *)
    Test.tztest "unregistered delegate key - credit 1μꜩ (origination, small fee)"
      `Quick (unregistered_delegate_key_init_origination_credit ~fee:Tez.one_mutez ~amount:Tez.one_mutez) ;
    Test.tztest "unregistered delegate key - credit 1μꜩ (origination, edge case fee)"
      `Quick (unregistered_delegate_key_init_origination_credit ~fee:(Tez.of_int 3_999_488) ~amount:Tez.one_mutez) ;
    Test.tztest "unregistered delegate key - credit 1μꜩ (origination, large fee)"
      `Quick (unregistered_delegate_key_init_origination_credit ~fee:(Tez.of_int 10_000_000) ~amount:Tez.one_mutez) ;

    Test.tztest "unregistered delegate key - credit 1μꜩ (init with delegation, small fee)"
      `Quick (unregistered_delegate_key_init_delegation_credit ~amount:Tez.one_mutez ~fee:Tez.one_mutez) ;
    Test.tztest "unregistered delegate key - credit 1μꜩ (init with delegation, large fee)"
      `Quick (unregistered_delegate_key_init_delegation_credit ~amount:Tez.one_mutez ~fee:Tez.max_tez) ;

    Test.tztest "unregistered delegate key - credit 1μꜩ (switch with delegation, small fee)"
      `Quick (unregistered_delegate_key_switch_delegation_credit ~amount:Tez.one_mutez ~fee:Tez.one_mutez) ;
    Test.tztest "unregistered delegate key - credit 1μꜩ (switch with delegation, large fee)"
      `Quick (unregistered_delegate_key_switch_delegation_credit ~amount:Tez.one_mutez ~fee:Tez.max_tez) ;

    (* origination with self_delegation on unrevealed and unregistered contract *)
    Test.tztest "unregistered and unrevealed self-delegation (origination, small fee)"
      `Quick (unregistered_and_unrevealed_self_delegate_key_init_origination ~fee:Tez.one_mutez) ;
    Test.tztest "unregistered and unrevealed self-delegation (origination, large fee)"
      `Quick (unregistered_and_unrevealed_self_delegate_key_init_origination ~fee:Tez.max_tez) ;

    (* origination with self_delegation on unregistered contract *)
    Test.tztest "unregistered and revealed self-delegation (origination, small fee)"
      `Quick (unregistered_and_revealed_self_delegate_key_init_origination ~fee:Tez.one_mutez) ;
    Test.tztest "unregistered and revealed self-delegation (origination, large fee)"
      `Quick (unregistered_and_revealed_self_delegate_key_init_origination ~fee:Tez.max_tez) ;

    (* origination with self_delegation on registered contract *)
    Test.tztest "registered and revelead self-delegation (origination)"
      `Quick registered_self_delegate_key_init_origination ;

    (*** unregistered delegate key: failed self-delegation ***)
    (* no token transfer, self-delegation *)
    Test.tztest "failed self-delegation: no transaction" `Quick failed_self_delegation_no_transaction ;
    (* credit 1μtz, debit 1μtz, self-delegation *)
    Test.tztest "failed self-delegation: credit & debit 1μꜩ" `Quick (failed_self_delegation_emptied_implicit_contract Tez.one_mutez) ;

    (*** valid registration ***)
    (* valid registration: credit 1 μꜩ, self delegation *)
    Test.tztest "valid delegate registration: credit 1μꜩ, self delegation (origination)"
      `Quick (valid_delegate_registration_init_origination_credit Tez.one_mutez) ;
    Test.tztest "valid delegate registration: credit 1μꜩ, self delegation (init with delegation)"
      `Quick (valid_delegate_registration_init_delegation_credit Tez.one_mutez) ;
    Test.tztest "valid delegate registration: credit 1μꜩ, self delegation (switch with delegation)"
      `Quick (valid_delegate_registration_switch_delegation_credit Tez.one_mutez) ;
    (* valid registration: credit 1 μꜩ, self delegation, debit 1μꜩ *)
    Test.tztest "valid delegate registration: credit 1μꜩ, self delegation, debit 1μꜩ (origination)"
      `Quick (valid_delegate_registration_init_origination_credit_debit Tez.one_mutez) ;
    Test.tztest "valid delegate registration: credit 1μꜩ, self delegation, debit 1μꜩ (init with delegation)"
      `Quick (valid_delegate_registration_init_delegation_credit_debit Tez.one_mutez) ;
    Test.tztest "valid delegate registration: credit 1μꜩ, self delegation, debit 1μꜩ (switch with delegation)"
      `Quick (valid_delegate_registration_switch_delegation_credit_debit Tez.one_mutez) ;

    (*** double registration ***)
    Test.tztest "double registration" `Quick double_registration ;
    Test.tztest "double registration when delegate account is emptied" `Quick double_registration_when_empty ;
    Test.tztest "double registration when delegate account is emptied and then recredited" `Quick double_registration_when_recredited ;
  ]



(******************************************************************************)
(* Main                                                                       *)
(******************************************************************************)

let tests =
  tests_bootstrap_contracts  @
  tests_delegate_registration
