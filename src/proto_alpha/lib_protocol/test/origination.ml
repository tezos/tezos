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
open Test_utils
open Test_tez

let ten_tez = Tez.of_int 10

(** [register_origination fee credit spendable delegatable] takes four
    optional parameter: fee for the fee need to be paid if set to
    create an originated contract; credit is the amount of tez that
    send to this originated contract; spendable default is set to true
    meaning that this contract is spendable; delegatable default is
    set to true meaning that this contract is able to delegate. *)
let register_origination ?(fee=Tez.zero) ?(credit=Tez.zero) ?spendable ?delegatable () =
  Context.init 1 >>=? fun (b, contracts) ->
  let source = List.hd contracts in
  Context.Contract.balance (B b) source >>=? fun source_balance ->
  Op.origination (B b) source ~fee ~credit ?spendable ?delegatable
  >>=? fun (operation, originated) ->
  Block.bake ~operation b >>=? fun b ->
  (* fee + credit + block security deposit were debited from source *)
  Context.get_constants (B b) >>=? fun {parametric = { origination_size ;
                                                       cost_per_byte ;
                                                       block_security_deposit ; _ }; _ } ->
  Tez.(cost_per_byte *? Int64.of_int origination_size) >>?= fun origination_burn ->
  Lwt.return (
    Tez.(+?) credit block_security_deposit >>?
    Tez.(+?) fee >>?
    Tez.(+?) origination_burn ) >>=? fun total_fee ->
  Assert.balance_was_debited ~loc:__LOC__ (B b) source source_balance total_fee >>=? fun () ->
  (* originated contract has been credited *)
  Assert.balance_was_credited ~loc:__LOC__ (B b) originated Tez.zero credit >>=? fun () ->
  (* TODO spendable or not and delegatable or not if relevant for some
     test. Not the case at the moment, cf. uses of
     register_origination *)
  return (b, source, originated)


(* [test_origination_balances fee credit spendable delegatable]
   takes four optional parameter: fee is the fee that pay if require to create an originated contract; credit is the amount of tez that will send to this contract; spendable default is set to true meaning that this contract is spendable; delegatable default is set to true meaning that this contract is able to delegate.
   This function will create a contract, get the balance of this contract, call the origination operation to create a new originated contract from this contract with all the possible fees; and check the balance before/after originated operation valid.
   - the source contract has payed all the fees
   - the originated has been credited correctly *)
let test_origination_balances ~loc ?(fee=Tez.zero) ?(credit=Tez.zero)
    ?spendable ?delegatable () =
  Context.init 1 >>=? fun (b, contracts) ->
  let contract = List.hd contracts in
  Context.Contract.balance (B b) contract >>=? fun balance ->
  Op.origination (B b) contract ~fee ~credit ?spendable ?delegatable >>=? fun (operation, new_contract) ->
  (* The possible fees are: a given credit, an origination burn fee
     (constants_repr.default.origination_burn = 257 mtez),
     a fee that is paid when creating an originate contract.

     We also take into account a block security deposit. Note that it
     is not related to origination but to the baking done in the
     tests.*)
  Context.get_constants (B b) >>=? fun
    { parametric =
        { origination_size ;
          cost_per_byte ;
          block_security_deposit
        ; _ }
    ; _ } ->
  Tez.(cost_per_byte *? Int64.of_int origination_size) >>?= fun origination_burn ->
  Lwt.return (
    Tez.(+?) credit block_security_deposit >>?
    Tez.(+?) fee >>?
    Tez.(+?) origination_burn ) >>=? fun total_fee ->
  Block.bake ~operation b >>=? fun b ->
  (* check that after the block has been baked the source contract
     was debited all the fees *)
  Assert.balance_was_debited ~loc (B b) contract balance total_fee
  >>=? fun _ ->
  (* check the balance of the originate contract is equal to credit *)
  Assert.balance_is ~loc (B b) new_contract credit

(** [transfer_and_check_balances b source dest amount] takes a block,
    a source contract, a destination and the amount that one wants to send
    (with no fee) and check the source and destination balances. *)
let transfer_and_check_balances b source dest amount =
  Context.Contract.balance (B b) source >>=? fun balance ->
  Op.transaction (B b) source dest amount >>=? fun operation ->
  Block.bake ~operation b >>=? fun b ->
  Assert.balance_was_debited ~loc:__LOC__ (B b) source balance amount >>=? fun _ ->
  return b

(******************************************************)
(** Tests *)
(******************************************************)

(** compute half of the balance and divided it by nth times *)

let two_nth_of_balance incr contract nth =
  Context.Contract.balance (I incr) contract >>=? fun balance ->
  Tez.(/?) balance nth >>?= fun res ->
  Tez.( *?) res 2L >>?= fun balance ->
  return balance

(*******************)
(** Basic test *)
(*******************)

let balances_simple () = test_origination_balances ~loc:__LOC__ ()

let balances_credit () =
  test_origination_balances ~loc:__LOC__ ~credit:ten_tez ()

let balances_credit_fee () =
  test_origination_balances ~loc:__LOC__ ~credit:(Tez.of_int 2) ~fee:ten_tez ()

let balances_credit_unspendable () =
  test_origination_balances ~loc:__LOC__ ~credit:Tez.one ~spendable:false ()

let balances_undelegatable () =
  test_origination_balances ~loc:__LOC__ ~delegatable:false ()

(*******************)
(** create an originate contract with a credit, then use this contract to
    transfer some tez back into the source contract, change the delegate
    contract to the endorser account *)
(*******************)

let regular () =
  register_origination ~credit:ten_tez () >>=? fun (b, contract, new_contract) ->
  transfer_and_check_balances b new_contract contract Tez.one_cent >>=? fun _ ->

  (* Delegatable *)
  Context.get_endorser (B b) >>=? fun (account, _slots) ->
  Op.delegation (B b) new_contract (Some account) >>=? fun operation ->
  Block.bake ~operation b >>=? fun _ ->
  return_unit

(*******************)
(** ask source contract to pay a fee when originating a contract *)
(*******************)

let pay_fee () =
  register_origination ~credit:(Tez.of_int 2) ~fee:ten_tez () >>=? fun (b, contract, new_contract) ->
  transfer_and_check_balances b new_contract contract (Tez.of_int 2) >>=? fun _ ->
  return_unit

(******************************************************)
(** Errors *)
(******************************************************)

(*******************)
(** The originate contract is marked as unspendable. Then ask this
    contract to transfer, it will raise an error *)
(*******************)

let unspendable () =
  register_origination ~credit:Tez.one ~spendable:false () >>=? fun (b, contract, new_contract) ->
  Op.transaction (B b) new_contract contract Tez.one_cent >>=? fun operation ->
  Block.bake ~operation b >>= fun e ->
  let unspendable = function
    | Proto_alpha.Contract_storage.Unspendable_contract _ -> true
    | _ -> false in
  Assert.proto_error ~loc:__LOC__ e unspendable

(*******************)
(** The originate contract is marked as undelegatable. Then do the delegation
    for this contract, it will raise an error *)
(*******************)

let undelegatable fee () =
  register_origination ~delegatable:false () >>=? fun (b, _, new_contract) ->
  Context.get_endorser (B b) >>=? fun (account, _slots) ->
  Incremental.begin_construction b >>=? fun i ->
  Context.Contract.balance (I i) new_contract >>=? fun balance ->
  Context.Contract.delegate_opt (I i) new_contract >>=? fun delegate_opt ->
  assert (delegate_opt = None) ;
  Op.delegation ~fee (I i) new_contract (Some account) >>=? fun operation ->
  if fee > balance then
    (* fees cannot be paid *)
    begin
      Incremental.add_operation i operation >>= fun res ->
      let not_enough_money = function
        | Proto_alpha.Contract_storage.Balance_too_low _ -> true
        | _ -> false
      in
      Assert.proto_error ~loc:__LOC__ res not_enough_money
    end
  else
    (* delegation is processed ; but delegate does not change *)
    begin
      let expect_failure = function
        | Alpha_environment.Ecoproto_error (Delegate_storage.Non_delegatable_contract _) :: _ ->
            return_unit
        | _ ->
            failwith "The contract is not delegatable, it fails!"
      in
      Incremental.add_operation ~expect_failure i operation >>=? fun i ->
      (* still no delegate *)
      Context.Contract.delegate_opt (I i) new_contract >>=? fun new_delegate_opt ->
      assert (new_delegate_opt = None) ;
      (* new contract loses the fee *)
      Assert.balance_was_debited ~loc:__LOC__ (I i) new_contract balance fee
    end

(*******************)
(** the credit is zero tez *)
(*******************)

let credit fee () =
  register_origination ~credit:Tez.zero () >>=? fun (b, contract, new_contract) ->
  Incremental.begin_construction b >>=? fun i ->
  Context.Contract.balance (I i) contract >>=? fun balance ->
  Context.Contract.balance (I i) new_contract >>=? fun new_balance ->
  (* the source contract does not have enough tez to transfer *)
  Op.transaction ~fee (I i) new_contract contract Tez.one_cent >>=? fun operation ->
  if fee > new_balance then
    begin
      Incremental.add_operation i operation >>= fun res ->
      let not_enough_money = function
        | Proto_alpha.Contract_storage.Balance_too_low _ -> true
        | _ -> false
      in
      Assert.proto_error ~loc:__LOC__ res not_enough_money
    end
  else
    begin
      let not_enough_money = function
        | Alpha_environment.Ecoproto_error (Proto_alpha.Contract_storage.Balance_too_low _) :: _ ->
            return_unit
        | _ -> failwith "The contract does not have enough money, it fails!"
      in
      Incremental.add_operation ~expect_failure:not_enough_money i operation >>=? fun i ->
      (* new contract loses the fee *)
      Assert.balance_was_debited ~loc:__LOC__ (I i) new_contract new_balance fee >>=? fun () ->
      (* contract is not credited *)
      Assert.balance_was_credited ~loc:__LOC__ (I i) contract balance Tez.zero
    end

(*******************)
(** same as register_origination but for an incremental *)
(*******************)

let register_origination_inc ~credit () =
  Context.init 1 >>=? fun (b, contracts) ->
  let source_contract = List.hd contracts in
  Incremental.begin_construction b >>=? fun inc ->
  Op.origination (I inc)
    ~storage_limit:(Z.of_int Constants_repr.default.origination_size)
    ~credit source_contract >>=? fun (operation, new_contract) ->
  Incremental.add_operation inc operation >>=? fun inc ->
  return (inc, source_contract, new_contract)

(*******************)
(** Using the originate contract to create another
    originate contract *)
(*******************)

let origination_contract_from_origination_contract_not_enough_fund fee () =
  let amount = Tez.one in
  register_origination_inc ~credit:amount () >>=? fun (inc, _, contract) ->
  (* contract's balance is not enough to afford origination burn  *)
  Op.origination ~fee (I inc) ~credit:amount contract >>=? fun (operation, orig_contract) ->
  let expect_failure = function
    | Alpha_environment.Ecoproto_error (Alpha_context.Fees.Cannot_pay_storage_fee) :: _ ->
        return_unit
    | e ->
        failwith "The contract has not enough funds, it fails! %a"
          Error_monad.pp_print_error e
  in
  Incremental.add_operation ~expect_failure inc operation >>=? fun inc ->
  Context.Contract.balance (I inc) contract >>=? fun balance_aft ->
  (* contract was debited of [fee] but not of origination burn *)
  Assert.balance_was_debited ~loc:__LOC__ (I inc) contract balance_aft fee >>=? fun () ->
  (* orig_contract does not exist *)
  Context.Contract.balance (I inc) orig_contract >>= fun res ->
  Assert.error ~loc:__LOC__ res begin function
    | RPC_context.Not_found _ -> true
    | _ -> false
  end
(*******************)
(** create an originate contract where the contract
    does not have enough tez to pay for the fee *)
(*******************)

let not_tez_in_contract_to_pay_fee () =
  Context.init 2 >>=? fun (b, contracts) ->
  let contract_1 = List.nth contracts 0 in
  let contract_2 = List.nth contracts 1 in
  Incremental.begin_construction b >>=? fun inc ->
  (* transfer everything but one tez from 1 to 2 and check balance of 1 *)
  Context.Contract.balance (I inc) contract_1 >>=? fun balance ->
  Lwt.return @@ Tez.(-?) balance Tez.one >>=? fun amount ->
  Op.transaction (I inc) contract_1 contract_2 amount >>=? fun operation ->
  Incremental.add_operation inc operation >>=? fun inc ->
  Assert.balance_was_debited ~loc:__LOC__ (I inc) contract_1 balance amount
  >>=? fun _ ->
  (* use this source contract to create an originate contract where it requires
     to pay a fee and add an amount of credit into this new contract *)
  Op.origination (I inc) ~fee:ten_tez ~credit:Tez.one contract_1 >>=? fun (op, _) ->
  Incremental.add_operation inc op >>= fun inc ->
  Assert.proto_error ~loc:__LOC__ inc begin function
    | Contract_storage.Balance_too_low _ -> true
    | _ -> false
  end

(***************************************************)
(* set the endorser of the block as manager/delegate of the originated
   account *)
(***************************************************)

let register_contract_get_endorser () =
  Context.init 1 >>=? fun (b, contracts) ->
  let contract = List.hd contracts in
  Incremental.begin_construction b >>=? fun inc ->
  Context.get_endorser (I inc) >>=? fun (account_endorser, _slots) ->
  return (inc, contract, account_endorser)

let set_manager () =
  register_contract_get_endorser () >>=? fun (inc, contract, account_endorser) ->
  Op.origination ~manager:account_endorser (I inc) ~credit:Tez.one contract >>=? fun (op, orig_contract) ->
  Incremental.add_operation inc op >>=? fun inc ->
  Incremental.finalize_block inc >>=? fun b ->
  (* the manager is indeed the endorser *)
  Context.Contract.manager (B b) orig_contract >>=? fun manager ->
  Assert.equal_pkh ~loc:__LOC__ manager.pkh account_endorser

let set_delegate () =
  register_contract_get_endorser () >>=? fun (inc, contract, account_endorser) ->
  Op.origination ~delegate:account_endorser (I inc) ~credit:Tez.one contract >>=? fun (op, orig_contract) ->
  Incremental.add_operation inc op >>=? fun inc ->
  Incremental.finalize_block inc >>=? fun b ->
  (* the delegate is indeed the endorser *)
  Context.Contract.delegate (B b) orig_contract >>=? fun delegate ->
  Assert.equal_pkh ~loc:__LOC__ delegate account_endorser

(*******************)
(** create multiple originated contracts and
    ask contract to pay the fee *)
(*******************)

let n_originations n ?credit ?fee ?spendable ?delegatable () =
  fold_left_s (fun new_contracts _ ->
      register_origination ?fee ?credit ?spendable ?delegatable () >>=? fun (_b, _source, new_contract) ->

      let contracts = new_contract :: new_contracts in
      return contracts
    ) [] (1 -- n)

let multiple_originations () =
  n_originations 100 ~credit:(Tez.of_int 2) ~fee:ten_tez () >>=? fun contracts ->
  Assert.equal_int ~loc:__LOC__ (List.length contracts) 100

(*******************)
(** cannot originate two contracts with the same context's counter *)
(*******************)

let counter () =
  Context.init 1 >>=? fun (b, contracts) ->
  let contract = List.hd contracts in
  Incremental.begin_construction b >>=? fun inc ->
  Op.origination (I inc) ~credit:Tez.one contract >>=? fun (op1, _) ->
  Op.origination (I inc) ~credit:Tez.one contract >>=? fun (op2, _) ->
  Incremental.add_operation inc op1 >>=? fun inc ->
  Incremental.add_operation inc op2 >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Contract_storage.Counter_in_the_past _ -> true
    | _ -> false
  end

(*******************)
(* create an originate contract from an originate contract *)
(*******************)

let origination_contract_from_origination_contract () =
  register_origination_inc ~credit:ten_tez () >>=? fun (inc, _source_contract, new_contract) ->
  let credit = Tez.one in
  Op.origination (I inc) ~credit new_contract >>=? fun (op2, orig_contract) ->
  Incremental.add_operation inc op2 >>=? fun inc ->
  Incremental.finalize_block inc >>=? fun b ->
  (* operation has been processed:
     originated contract exists and has been credited with the right amount *)
  Context.Contract.balance (B b) orig_contract >>=? fun credit0 ->
  Assert.equal_tez ~loc:__LOC__ credit0 credit

(******************************************************)

let tests = [
  Test.tztest "balances_simple" `Quick balances_simple ;
  Test.tztest "balances_credit" `Quick balances_credit ;
  Test.tztest "balances_credit_fee" `Quick balances_credit_fee ;
  Test.tztest "balances_credit_unspendable" `Quick balances_credit_unspendable ;
  Test.tztest "balances_undelegatable" `Quick balances_undelegatable ;

  Test.tztest "regular" `Quick regular ;
  Test.tztest "pay_fee" `Quick pay_fee;

  Test.tztest "unspendable" `Quick unspendable ;
  Test.tztest "undelegatable (no fee)" `Quick (undelegatable Tez.zero);
  Test.tztest "undelegatable (with fee)" `Quick (undelegatable Tez.one);
  Test.tztest "credit" `Quick (credit Tez.one) ;
  Test.tztest "create origination from origination not enough fund" `Quick  (origination_contract_from_origination_contract_not_enough_fund Tez.zero);
  Test.tztest "not enough tez in contract to pay fee" `Quick not_tez_in_contract_to_pay_fee;

  Test.tztest "set manager" `Quick set_manager;
  Test.tztest "set delegate" `Quick set_delegate;

  Test.tztest "multiple originations" `Quick multiple_originations;

  Test.tztest "counter" `Quick counter;

  Test.tztest "create origination from origination" `Quick
    origination_contract_from_origination_contract;
]
