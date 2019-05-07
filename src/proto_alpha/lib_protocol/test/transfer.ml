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
open Test_utils
open Test_tez

(*********************************************************************)
(* Utility functions                                                 *)
(*********************************************************************)

(**
   [transfer_and_check_balances b fee src dst amount]
   this function takes a block, an optional parameter fee if fee does not
   given it will be set to zero tez, a source contract, a destination contract
   and the amount that one wants to transfer.

   1- Transfer the amount of tez (w/wo fee) from a source contract to a
       destination contract.

    2- Check the equivalent of the balance of the source/destination
       contract before and after transfer is valided.

   This function returns a pair:
   - A block that added a valid operation
   - a valid operation
*)
let transfer_and_check_balances ?(with_burn = false) ~loc b ?(fee=Tez.zero) ?expect_failure src dst amount =
  Tez.(+?) fee amount >>?= fun amount_fee ->
  Context.Contract.balance (I b) src >>=? fun bal_src ->
  Context.Contract.balance (I b) dst >>=? fun bal_dst ->
  Op.transaction (I b) ~fee src dst amount >>=? fun op ->
  Incremental.add_operation ?expect_failure b op >>=? fun b ->
  Context.get_constants (I b) >>=? fun { parametric = { origination_size ; cost_per_byte ; _ } ; _ } ->
  Tez.(cost_per_byte *? Int64.of_int origination_size) >>?= fun origination_burn ->
  let amount_fee_maybe_burn =
    if with_burn then
      match Tez.(amount_fee +? origination_burn) with
      | Ok r -> r
      | Error _ -> assert false
    else
      amount_fee in
  Assert.balance_was_debited  ~loc (I b) src bal_src amount_fee_maybe_burn >>=? fun () ->
  Assert.balance_was_credited ~loc (I b) dst bal_dst amount >>=? fun () ->
  return (b, op)

(**
   [transfer_to_itself_and_check_balances b fee contract amount]
   this function takes a block, an optional parameter fee,
   a contract that is a source and a destination contract,
   and an amount of tez that one wants to transfer.

   1- Transfer the amount of tez (w/wo transfer fee) from/to a contract itself.

   2- Check the equivalent of the balance of the contract before
       and after transfer.

   This function returns a pair:
   - a block that added the valid transaction
   - an valid transaction
*)
let transfer_to_itself_and_check_balances ~loc b ?(fee=Tez.zero) contract amount =
  Context.Contract.balance (I b) contract >>=? fun bal ->
  Op.transaction (I b) ~fee contract contract amount >>=? fun op ->
  Incremental.add_operation b op >>=? fun b ->
  Assert.balance_was_debited ~loc (I b) contract bal fee >>=? fun () ->
  return (b, op)

(**
   [n_transactions n b fee source dest amount]
   this function takes a number of "n" that one wish to transfer,
   a block, an optional parameter fee, a source contract,
   a destination contract and an amount one wants to transfer.

   This function will do a transaction from a source contract to
   a destination contract with the amount "n" times.
*)
let n_transactions n b ?fee source dest amount =
  fold_left_s (fun b _ ->
      transfer_and_check_balances ~loc:__LOC__ b ?fee source dest amount >>=? fun (b,_) ->
      return b)
    b (1 -- n)

let ten_tez = Tez.of_int 10

(*********************************************************************)
(* Tests                                                             *)
(*********************************************************************)

let register_two_contracts () =
  Context.init 2 >>=? fun (b, contracts) ->
  let contract_1 = List.nth contracts 0 in
  let contract_2 = List.nth contracts 1 in
  return (b, contract_1, contract_2)


(** compute half of the balance and divided by nth
    times *)

let two_nth_of_balance incr contract nth =
  Context.Contract.balance (I incr) contract >>=? fun balance ->
  Tez.(/?) balance nth >>?= fun res ->
  Tez.( *?) res 2L >>?= fun balance ->
  return balance

(********************)
(** Single transfer *)
(********************)

let single_transfer ?fee ?expect_failure amount =
  register_two_contracts () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun b ->
  transfer_and_check_balances ~loc:__LOC__ ?fee ?expect_failure
    b contract_1 contract_2 amount >>=? fun (b,_) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(** single transfer without fee *)
let block_with_a_single_transfer () =
  single_transfer Tez.one

(** single transfer with fee *)
let block_with_a_single_transfer_with_fee () =
  single_transfer ~fee:Tez.one Tez.one

(** single transfer without fee *)

let transfer_zero_tez () =
  single_transfer ~expect_failure:(
    function
    | Alpha_environment.Ecoproto_error (Contract_storage.Empty_transaction _) :: _ ->
        return_unit
    | _ ->
        failwith "Empty transaction should fail")
    Tez.zero

(********************)
(** Transfer zero tez from an originated/implicit contract *)
(********************)

let transfer_zero_originated () =
  register_two_contracts () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun i ->
  (* originated the first contract *)
  Op.origination (I i) contract_1 >>=? fun (operation, orig_contract_1) ->
  Incremental.add_operation i operation >>=? fun i ->
  Context.Contract.balance (I i) orig_contract_1 >>=? fun balance_1 ->
  (* transfer all the tez inside the originated contract *)
  transfer_and_check_balances ~loc:__LOC__ i
    orig_contract_1 contract_2 balance_1 >>=? fun (i, _) ->
  Op.transaction (I i) orig_contract_1 contract_2 Tez.zero >>=? fun op ->
  Incremental.add_operation i op >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Contract_storage.Empty_transaction _ -> true
    | _ -> false
  end

let transfer_zero_implicit () =
  Context.init 1 >>=? fun (b, contracts) ->
  let dest = List.nth contracts 0 in
  let account = Account.new_account () in
  Incremental.begin_construction b >>=? fun i ->
  let src = Contract.implicit_contract account.Account.pkh in
  Op.transaction (I i) src dest Tez.zero >>=? fun op ->
  Incremental.add_operation i op >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Contract_storage.Empty_implicit_contract _ -> true
    | _ -> false
  end

(********************)
(** Transfer to originted contract *)
(********************)

let transfer_to_originate_with_fee () =
  Context.init 1 >>=? fun (b, contracts) ->
  let contract = List.nth contracts 0 in
  Incremental.begin_construction b >>=? fun b ->
  two_nth_of_balance b contract 10L >>=? fun fee ->
  (* originated contract, paying a fee to originated this contract *)
  Op.origination (I b) ~fee:ten_tez contract >>=? fun (operation, new_contract) ->
  Incremental.add_operation b operation >>=? fun b ->
  two_nth_of_balance b contract 3L >>=? fun amount ->
  transfer_and_check_balances ~loc:__LOC__ b ~fee:fee contract
    new_contract amount >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(********************)
(** Transfer from balance *)
(********************)

let transfer_amount_of_contract_balance () =
  register_two_contracts () >>=? fun (b, contract_1, contract_2) ->
  Context.Contract.pkh contract_1 >>=? fun pkh1 ->
  (* given that contract_1 no longer has a sufficient balance to bake,
     make sure it cannot be chosen as baker *)
  Incremental.begin_construction b ~policy:(Block.Excluding [pkh1]) >>=? fun b ->
  (* get the balance of the source contract *)
  Context.Contract.balance (I b) contract_1 >>=? fun balance ->
  (* transfer all the tez inside contract 1 *)
  transfer_and_check_balances ~loc:__LOC__
    b contract_1 contract_2 balance >>=? fun (b,_) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(********************)
(** Transfer to itself *)
(********************)

let transfers_to_self () =
  Context.init 1 >>=? fun (b, contracts) ->
  let contract = List.nth contracts 0 in
  Incremental.begin_construction b >>=? fun b ->
  two_nth_of_balance b contract 3L >>=? fun amount ->
  transfer_to_itself_and_check_balances ~loc:__LOC__ b contract amount
  >>=? fun (b, _) ->
  two_nth_of_balance b contract 5L >>=? fun fee ->
  transfer_to_itself_and_check_balances ~loc:__LOC__ b ~fee:fee contract ten_tez
  >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(********************)
(** Forgot to add the valid transaction into the block *)
(********************)

let missing_transaction () =
  register_two_contracts () >>=? fun (b, contract_1, contract_2) ->
  (* given that contract_1 no longer has a sufficient balance to bake,
     make sure it cannot be chosen as baker *)
  Context.Contract.pkh contract_1 >>=? fun pkh1 ->
  Incremental.begin_construction b ~policy:(Block.Excluding [pkh1]) >>=? fun b ->
  two_nth_of_balance b contract_1 6L >>=? fun amount ->
  (* do the transfer 3 times from source contract to destination contract *)
  n_transactions 3 b contract_1 contract_2 amount >>=? fun b ->
  (* do the fourth transfer from source contract to destination contract *)
  Op.transaction (I b) contract_1 contract_2 amount >>=? fun _ ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(********************)
(** These following tests are for different kind of contracts:
    - implicit to implicit
    - implicit to originated
    - originated to implicit
    - originted to originted *)
(********************)

(** Implicit to Implicit *)

let transfer_from_implicit_to_implicit_contract () =
  Context.init 1 >>=? fun (b, contracts) ->
  let bootstrap_contract = List.nth contracts 0 in
  let account_a = Account.new_account () in
  let account_b = Account.new_account () in
  Incremental.begin_construction b >>=? fun b ->
  let src = Contract.implicit_contract account_a.Account.pkh in
  two_nth_of_balance b bootstrap_contract 3L >>=? fun amount1 ->
  two_nth_of_balance b bootstrap_contract 10L >>=? fun fee1 ->
  transfer_and_check_balances ~with_burn:true ~loc:__LOC__ ~fee:fee1 b
    bootstrap_contract src amount1 >>=? fun (b, _) ->
  (* create an implicit contract as a destination contract *)
  let dest = Contract.implicit_contract account_b.pkh in
  two_nth_of_balance b bootstrap_contract 4L >>=? fun amount2 ->
  two_nth_of_balance b bootstrap_contract 10L >>=? fun fee2 ->
  (* transfer from implicit contract to another implicit contract *)
  transfer_and_check_balances ~with_burn:true ~loc:__LOC__ ~fee:fee2 b
    src dest amount2 >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(** Implicit to originated *)

let transfer_from_implicit_to_originated_contract () =
  Context.init 1 >>=? fun (b, contracts) ->
  let bootstrap_contract = List.nth contracts 0 in
  let contract = List.nth contracts 0 in
  let account = Account.new_account () in
  let src = Contract.implicit_contract account.Account.pkh in
  Incremental.begin_construction b >>=? fun b ->
  two_nth_of_balance b bootstrap_contract 3L >>=? fun amount1 ->
  (* transfer the money to implicit contract *)
  transfer_and_check_balances ~with_burn:true ~loc:__LOC__ b bootstrap_contract src amount1
  >>=? fun (b, _) ->
  (* originated contract *)
  Op.origination (I b) contract >>=? fun (operation, new_contract) ->
  Incremental.add_operation b operation >>=? fun b ->
  two_nth_of_balance b bootstrap_contract 4L >>=? fun amount2 ->
  (* transfer from implicit contract to originated contract *)
  transfer_and_check_balances ~loc:__LOC__ b src new_contract amount2
  >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(** Originted to originted *)

let transfer_from_originated_to_originated () =
  register_two_contracts () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun b ->
  (* originated contract 1 *)
  Op.origination (I b) contract_1 >>=? fun (operation, orig_contract_1) ->
  Incremental.add_operation b operation >>=? fun b ->
  (* originated contract 2 *)
  Op.origination (I b) contract_2 >>=? fun (operation, orig_contract_2) ->
  Incremental.add_operation b operation >>=? fun b ->
  (* transfer from originated contract 1 to originated contract 2 *)
  transfer_and_check_balances ~loc:__LOC__ b
    orig_contract_1 orig_contract_2 Alpha_context.Tez.one >>=? fun (b,_) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(** Originted to impicit *)

let transfer_from_originated_to_implicit () =
  Context.init 1 >>=? fun (b, contracts) ->
  let contract_1 = List.nth contracts 0 in
  let account = Account.new_account () in
  let src = Contract.implicit_contract account.pkh in
  Incremental.begin_construction b >>=? fun b ->
  (* originated contract 1*)
  Op.origination (I b) contract_1 >>=? fun (operation, new_contract) ->
  Incremental.add_operation b operation >>=? fun b ->
  (* transfer from originated contract to implicit contract *)
  transfer_and_check_balances ~with_burn:true ~loc:__LOC__ b new_contract src Alpha_context.Tez.one_mutez
  >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(********************)
(** Slow tests case *)
(********************)

let multiple_transfer n ?fee amount =
  register_two_contracts () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun b ->
  n_transactions n b ?fee contract_1 contract_2 amount >>=? fun b ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(** 1- Create a block with two contracts;
    2- Apply 100 transfers. *)
let block_with_multiple_transfers () =
  multiple_transfer 99 (Tez.of_int 1000)

(** 1- Create a block with two contracts;
    2- Apply 100 transfers with 10tz fee. *)
let block_with_multiple_transfers_pay_fee () =
  multiple_transfer 10 ~fee:ten_tez (Tez.of_int 1000)

(** 1- Create a block with 8 contracts;
    2- Apply multiple transfers without fees;
    3- Apply multiple transfers with fees. *)
(* TODO : increase the number of operations and add a `Slow tag to it in `tests` *)
let block_with_multiple_transfers_with_without_fee () =
  Context.init 8 >>=? fun (b, contracts) ->
  let contracts = Array.of_list contracts in
  Incremental.begin_construction b >>=? fun b ->
  let hundred = Tez.of_int 100 in
  let ten = Tez.of_int 10 in
  let twenty = Tez.of_int 20 in
  n_transactions 10 b contracts.(0) contracts.(1) Tez.one >>=? fun b ->
  n_transactions 30 b contracts.(1) contracts.(2) hundred >>=? fun b ->
  n_transactions 30 b contracts.(1) contracts.(3) hundred >>=? fun b ->
  n_transactions 30 b contracts.(4) contracts.(3) hundred >>=? fun b ->
  n_transactions 20 b contracts.(0) contracts.(1) hundred >>=? fun b ->
  n_transactions 10 b contracts.(1) contracts.(3) hundred >>=? fun b ->
  n_transactions 10 b contracts.(1) contracts.(3) hundred >>=? fun b ->

  n_transactions 20  ~fee:ten b contracts.(3) contracts.(4) ten >>=? fun b ->
  n_transactions 10  ~fee:twenty b contracts.(4) contracts.(5) ten >>=? fun b ->
  n_transactions 70  ~fee:twenty b contracts.(6) contracts.(0) twenty >>=? fun b ->
  n_transactions 550 ~fee:twenty b contracts.(6) contracts.(4) twenty >>=? fun b ->
  n_transactions 50  ~fee:ten b contracts.(7) contracts.(5) twenty >>=? fun b ->
  n_transactions 30  ~fee:ten b contracts.(0) contracts.(7) hundred >>=? fun b ->
  n_transactions 20  ~fee:ten b contracts.(1) contracts.(0) twenty >>=? fun b ->

  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(********************)
(** Build a chain that has 10 blocks. *)
(********************)

let build_a_chain () =
  register_two_contracts () >>=? fun (b, contract_1, contract_2) ->
  let ten = Tez.of_int 10 in
  fold_left_s (fun b _ ->
      Incremental.begin_construction b >>=? fun b ->
      transfer_and_check_balances ~loc:__LOC__ b contract_1 contract_2 ten
      >>=? fun (b, _) ->
      Incremental.finalize_block b
    ) b (1 -- 10) >>=? fun _ ->
  return_unit

(*********************************************************************)
(* Expected error test cases                                         *)
(*********************************************************************)

(********************)
(** transfer zero tez is forbidden in implicit contract *)
(********************)

let empty_implicit () =
  Context.init 1 >>=? fun (b, contracts) ->
  let dest = List.nth contracts 0 in
  let account = Account.new_account () in
  Incremental.begin_construction b >>=? fun incr ->
  let src = Contract.implicit_contract account.Account.pkh in
  two_nth_of_balance incr dest 3L >>=? fun amount ->
  (* transfer zero tez from an implicit contract *)
  Op.transaction (I incr) src dest amount >>=? fun op ->
  Incremental.add_operation incr op >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Contract_storage.Empty_implicit_contract _ -> true
    | _ -> false
  end

(********************)
(** Balance is too low to transfer *)
(********************)

let balance_too_low fee () =
  register_two_contracts () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun i ->
  Context.Contract.balance (I i) contract_1 >>=? fun balance1 ->
  Context.Contract.balance (I i) contract_2 >>=? fun balance2 ->
  (* transfer the amount of tez that is bigger than the balance in the source contract *)
  Op.transaction ~fee (I i) contract_1 contract_2 Tez.max_tez >>=? fun op ->
  let expect_failure = function
    | Alpha_environment.Ecoproto_error (Contract_storage.Balance_too_low _) :: _ ->
        return_unit
    | _ ->
        failwith "balance too low should fail"
  in
  (* the fee is higher than the balance then raise an error "Balance_too_low" *)
  if fee > balance1 then begin
    Incremental.add_operation ~expect_failure i op >>= fun _res ->
    return_unit
  end
  (* the fee is smaller than the balance, then the transfer is accepted
     but it is not processed, and fees are taken *)
  else begin
    Incremental.add_operation ~expect_failure i op >>=? fun i ->
    (* contract_1 loses the fees *)
    Assert.balance_was_debited ~loc:__LOC__ (I i) contract_1 balance1 fee >>=? fun () ->
    (* contract_2 is not credited *)
    Assert.balance_was_credited ~loc:__LOC__ (I i) contract_2 balance2 Tez.zero
  end

(** 1- Create a block, and three contracts;
    2- Add a transfer that at the end the balance of a contract is
       zero into this block;
    3- Add another transfer that send tez from a zero balance contract;
    4- Catch the expected error: Balance_too_low. *)
let balance_too_low_two_transfers fee () =
  Context.init 3 >>=? fun (b, contracts) ->
  let contract_1 = List.nth contracts 0 in
  let contract_2 = List.nth contracts 1 in
  let contract_3 = List.nth contracts 2 in
  Incremental.begin_construction b >>=? fun i ->
  Context.Contract.balance (I i) contract_1 >>=? fun balance ->
  Tez.(/?) balance  3L >>?= fun res ->
  Tez.( *?) res 2L >>?= fun two_third_of_balance ->
  transfer_and_check_balances ~loc:__LOC__ i
    contract_1 contract_2 two_third_of_balance >>=? fun (i, _) ->
  Context.Contract.balance (I i) contract_1 >>=? fun balance1 ->
  Context.Contract.balance (I i) contract_3 >>=? fun balance3 ->
  Op.transaction ~fee (I i) contract_1 contract_3
    two_third_of_balance >>=? fun operation ->
  let expect_failure = function
    | Alpha_environment.Ecoproto_error (Contract_storage.Balance_too_low _) :: _ ->
        return_unit
    | _ ->
        failwith "balance too low should fail"
  in
  Incremental.add_operation ~expect_failure i operation >>=? fun i ->
  (* contract_1 loses the fees *)
  Assert.balance_was_debited ~loc:__LOC__ (I i) contract_1 balance1 fee >>=? fun () ->
  (* contract_3 is not credited *)
  Assert.balance_was_credited ~loc:__LOC__ (I i) contract_3 balance3 Tez.zero

(********************)
(** The counter is already used for the previous operation *)
(********************)

let invalid_counter () =
  register_two_contracts () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun b ->
  Op.transaction (I b) contract_1 contract_2
    Tez.one >>=? fun op1 ->
  Op.transaction (I b) contract_1 contract_2
    Tez.one >>=? fun op2 ->
  Incremental.add_operation b op1 >>=? fun b ->
  Incremental.add_operation b op2 >>= fun b ->
  Assert.proto_error ~loc:__LOC__ b begin function
    | Contract_storage.Counter_in_the_past _ -> true
    | _ -> false
  end

(* same as before but different way to perform this error *)

let add_the_same_operation_twice () =
  register_two_contracts () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun b ->
  transfer_and_check_balances ~loc:__LOC__ b contract_1 contract_2 ten_tez
  >>=? fun (b, op_transfer) ->
  Op.transaction (I b) contract_1 contract_2 ten_tez >>=? fun _ ->
  Incremental.add_operation b op_transfer >>= fun b ->
  Assert.proto_error ~loc:__LOC__ b begin function
    | Contract_storage.Counter_in_the_past _ -> true
    | _ -> false
  end

(********************)
(** Do the transfer from an "unspendable" contract *)
(********************)

let unspendable_contract () =
  register_two_contracts () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun b ->
  Op.origination ~spendable:false (I b) contract_1 >>=? fun (operation, unspendable_contract) ->
  Incremental.add_operation b operation >>=? fun b ->
  Op.transaction (I b) unspendable_contract contract_2 Alpha_context.Tez.one_cent >>=? fun operation ->
  Incremental.add_operation b operation >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Contract_storage.Unspendable_contract _ -> true
    | _ -> false
  end

(********************)
(** check ownership *)
(********************)

let ownership_sender () =
  register_two_contracts () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun b ->
  (* get the manager of the contract_1 as a sender *)
  Context.Contract.manager (I b) contract_1 >>=? fun manager ->
  (* create an implicit_contract *)
  let imcontract_1 = Alpha_context.Contract.implicit_contract manager.pkh in
  transfer_and_check_balances ~loc:__LOC__ b imcontract_1 contract_2 Tez.one
  >>=? fun (b,_) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(*********************************************************************)
(** Random transfer *)

(** Return a pair of minimum and maximum random number *)
let random_range (min, max) =
  let interv = max - min + 1 in
  let init =
    Random.self_init ();
    (Random.int interv) + min
  in init

(** Return a random contract *)
let random_contract contract_array =
  let i = Random.int (Array.length contract_array) in
  contract_array.(i)

(** Transfer by randomly choose amount 10 contracts, and randomly
    choose the amount in the source contract *)
let random_transfer () =
  Context.init 10 >>=? fun (b, contracts) ->
  let contracts = Array.of_list contracts in
  let source = random_contract contracts in
  let dest = random_contract contracts in
  Context.Contract.pkh source >>=? fun source_pkh ->
  (* given that source may not have a sufficient balance for the transfer + to bake, 
     make sure it cannot be chosen as baker *)
  Incremental.begin_construction b ~policy:(Block.Excluding [source_pkh]) >>=? fun b ->
  Context.Contract.balance (I b) source >>=? fun amount ->
  begin
    if source = dest
    then
      transfer_to_itself_and_check_balances ~loc:__LOC__ b source amount
    else
      transfer_and_check_balances ~loc:__LOC__ b source dest amount
  end >>=? fun (b,_) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(** Transfer random transactions *)
let random_multi_transactions () =
  let n = random_range (1, 100) in
  multiple_transfer n (Tez.of_int 100)

(*********************************************************************)

let tests = [
  (* single transfer *)
  Test.tztest "single transfer" `Quick block_with_a_single_transfer ;
  Test.tztest "single transfer with fee" `Quick block_with_a_single_transfer_with_fee ;

  (* transfer zero tez *)
  Test.tztest "single transfer zero tez" `Quick transfer_zero_tez ;
  Test.tztest "transfer zero tez from originated contract" `Quick transfer_zero_originated;
  Test.tztest "transfer zero tez from implicit contract" `Quick transfer_zero_implicit;

  (* transfer to originated contract *)
  Test.tztest "transfer to originated contract paying transaction fee" `Quick transfer_to_originate_with_fee ;

  (* transfer by the balance of contract *)
  Test.tztest "transfer the amount from source contract balance" `Quick transfer_amount_of_contract_balance ;

  (* transfer to itself *)
  Test.tztest "transfers to itself" `Quick transfers_to_self ;

  (* missing operation *)

  Test.tztest "missing transaction" `Quick missing_transaction ;

  (* transfer from/to implicit/originted contracts*)
  Test.tztest "transfer from an implicit to implicit contract " `Quick transfer_from_implicit_to_implicit_contract ;
  Test.tztest "transfer from an implicit to an originated contract" `Quick transfer_from_implicit_to_originated_contract ;
  Test.tztest "transfer from an originated to an originated contract" `Quick transfer_from_originated_to_originated ;
  Test.tztest "transfer from an originated to an implicit contract" `Quick transfer_from_originated_to_implicit ;

  (* Slow tests *)
  Test.tztest "block with multiple transfers" `Slow block_with_multiple_transfers ;
  (* TODO increase the number of transaction times *)
  Test.tztest "block with multiple transfer paying fee" `Slow block_with_multiple_transfers_pay_fee ;
  Test.tztest "block with multiple transfer without paying fee" `Slow block_with_multiple_transfers_with_without_fee ;

  (* build the chain *)
  Test.tztest "build a chain" `Quick build_a_chain ;

  (* Erroneous *)
  Test.tztest "empty implicit" `Quick empty_implicit;
  Test.tztest "balance too low - transfer zero" `Quick (balance_too_low Tez.zero);
  Test.tztest "balance too low" `Quick (balance_too_low Tez.one);
  Test.tztest "balance too low (max fee)" `Quick (balance_too_low Tez.max_tez);
  Test.tztest "balance too low with two transfers - transfer zero" `Quick (balance_too_low_two_transfers Tez.zero);
  Test.tztest "balance too low with two transfers" `Quick (balance_too_low_two_transfers Tez.one);
  Test.tztest "invalid_counter" `Quick invalid_counter ;
  Test.tztest "add the same operation twice" `Quick  add_the_same_operation_twice ;
  Test.tztest "unspendable contract" `Quick unspendable_contract ;

  Test.tztest "ownership sender" `Quick  ownership_sender ;
  (* Random tests *)
  Test.tztest "random transfer" `Quick random_transfer ;
  Test.tztest "random multi transfer" `Quick  random_multi_transactions ;
]
