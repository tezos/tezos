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
open Proto_001_PtCJ7pwo
open Alpha_context
open Test_utils
open Test_tez

(*********************************************************************)
(* Utility functions                                                 *)
(*********************************************************************)

(** 1- Transfer the amount of tez from a source contract to a
       destination contract with or without the fee of transfer.
    2- Check the equivalent of the balance of the source/destination
       contract before and after the transfer *)
let transfer_and_check_balances ~loc b ?(fee=Tez.zero) ?expect_failure src dst amount =
  Tez.(+?) fee amount >>?= fun amount_fee ->
  Context.Contract.balance (I b) src >>=? fun bal_src ->
  Context.Contract.balance (I b) dst >>=? fun bal_dst ->
  Op.transaction (I b) ~fee src dst amount >>=? fun op ->
  Incremental.add_operation ?expect_failure b op >>=? fun b ->
  Assert.balance_was_debited  ~loc (I b) src bal_src amount_fee >>=? fun () ->
  Assert.balance_was_credited ~loc (I b) dst bal_dst amount >>=? fun () ->
  return (b, op)

(** 1- Transfer the amount of tez from/to a contract itself, with or
       without fee of transfer.
    2- Check the equivalent of the balance of the contract before
       and after transfer *)
let transfer_to_itself_and_check_balances ~loc b ?(fee=Tez.zero) contract amount =
  Context.Contract.balance (I b) contract >>=? fun bal ->
  Op.transaction (I b) ~fee contract contract amount >>=? fun op ->
  Incremental.add_operation b op >>=? fun b ->
  Assert.balance_was_debited ~loc (I b) contract bal fee >>=? fun () ->
  return (b, op)

(** Apply a transfer n times *)
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

(** 1- Create a block and two contracts;
    2- Add a single transfer into this block;
    3- Bake this block. *)
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

(** single transfer without fee *)
let transfer_zero_tez () =
  single_transfer ~expect_failure:(
    function
    | Alpha_environment.Ecoproto_error (Contract_storage.Empty_transaction _) :: _ ->
        return_unit
    | _ ->
        failwith "Empty transaction should fail")
    Tez.zero

(** single transfer with fee *)
let block_with_a_single_transfer_with_fee () =
  single_transfer ~fee:Tez.one Tez.one

(** 1- Create a block, and a single contract;
    2- Add the originate operation into this block;
    3- Add a transfer from a contract to a contract created by
       originate operation, that requires to pay a fee of transfer;
    4- Bake this block. *)
let block_originate_and_transfer_with_fee () =
  Context.init 1 >>=? fun (b, contracts) ->
  let contract = List.nth contracts 0 in
  Incremental.begin_construction b >>=? fun b ->
  Op.origination (I b) ~fee:ten_tez contract >>=? fun (operation, new_contract) ->
  Incremental.add_operation b operation >>=? fun b ->
  transfer_and_check_balances ~loc:__LOC__ b ~fee:ten_tez contract new_contract ten_tez >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(** 1- Create a block, and two contracts;
    2- Add a transfer from a current balance of a source contract
       into this block;
    3- Bake this block. *)
let block_transfer_from_contract_balance () =
  register_two_contracts () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun b ->
  Context.Contract.balance (I b) contract_1 >>=? fun balance ->
  transfer_and_check_balances ~loc:__LOC__ b contract_1 contract_2 balance >>=? fun (b,_) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(** 1- Create a block and a single contract;
    2- Add a transfer to a contract itself without fee into this block;
    3- Add a transfer to a contract itself with fee into this block;
    4- Bake this block. *)
let block_transfers_without_with_fee_to_self () =
  Context.init 1 >>=? fun (b, contracts) ->
  let contract = List.nth contracts 0 in
  Incremental.begin_construction b >>=? fun b ->
  transfer_to_itself_and_check_balances ~loc:__LOC__ b contract ten_tez
  >>=? fun (b, _) ->
  transfer_to_itself_and_check_balances ~loc:__LOC__ b ~fee:ten_tez contract ten_tez
  >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(** 1- Create a block, two contracts;
    2- Add three transfers into the block;
    3- Do a transfer without adding it to the block;
    4- Bake the block with three transfers. *)
let four_transfers_bake_three_transfers () =
  register_two_contracts () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun b ->
  n_transactions 3 b contract_1 contract_2 ten_tez >>=? fun b ->
  Op.transaction (I b) contract_1 contract_2 ten_tez >>=? fun _ ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(** 1- Create a contract from a bootstrap contract;
    2- Create two implicit contracts;
    3- Build a block from genesis;
    4- Add a transfer with fee for transfer from a bootstrap contract into an
       implicit contract into this block;
    5- Add a transfer with fee for transfer, between two implicit contracts
       into this block;
    6- Bake this block. *)
let transfer_from_implicit_to_implicit_contract () =
  Context.init 1 >>=? fun (b, contracts) ->
  let bootstrap_contract = List.nth contracts 0 in
  let account_a = Account.new_account () in
  let account_b = Account.new_account () in
  Incremental.begin_construction b >>=? fun b ->
  let src = Contract.implicit_contract account_a.Account.pkh in
  transfer_and_check_balances ~loc:__LOC__ ~fee:ten_tez b
    bootstrap_contract src (Tez.of_int 20) >>=? fun (b, _) ->
  let dest = Contract.implicit_contract account_b.pkh in
  transfer_and_check_balances ~loc:__LOC__ ~fee:(Tez.of_int 3) b
    src dest ten_tez >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(** 1- Create a block, contract from bootstrap accounts, contract from originate;
    2- Add a transfer from the bootstract contract into the implicit contract;
    3- Add a transfer from the impicit contract to the originate contract;
    4- Bake this block. *)
let transfer_from_implicit_to_originated_contract () =
  Context.init 1 >>=? fun (b, contracts) ->
  let bootstrap_contract = List.nth contracts 0 in
  let contract = List.nth contracts 0 in
  let account = Account.new_account () in
  let src = Contract.implicit_contract account.Account.pkh in
  Incremental.begin_construction b >>=? fun b ->
  transfer_and_check_balances ~loc:__LOC__ b bootstrap_contract src ten_tez
  >>=? fun (b, _) ->
  Op.origination (I b) contract >>=? fun (operation, new_contract) ->
  Incremental.add_operation b operation >>=? fun b ->
  transfer_and_check_balances ~loc:__LOC__ b src new_contract Alpha_context.Tez.one
  >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(** 1- Create a block with 2 contracts;
    2- Originate 2 contracts from the previous ones;
    2- Add a transfer between the two originated contracts;
    3- Bake this block. *)
let transfer_from_originated_to_originated () =
  register_two_contracts () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun b ->
  Op.origination (I b) contract_1 >>=? fun (operation, orig_contract_1) ->
  Incremental.add_operation b operation >>=? fun b ->
  Op.origination (I b) contract_2 >>=? fun (operation, orig_contract_2) ->
  Incremental.add_operation b operation >>=? fun b ->
  transfer_and_check_balances ~loc:__LOC__ b
    orig_contract_1 orig_contract_2 Alpha_context.Tez.one >>=? fun (b,_) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(** 1- Create a block, an originate contract, an impicit contract, a contract
       from bootstrap;
    2- Add a transfer from the originate contract to impicit contract;
    3- Bake this block. *)
let transfer_from_originated_to_implicit () =
  Context.init 1 >>=? fun (b, contracts) ->
  let contract_1 = List.nth contracts 0 in
  let account = Account.new_account () in
  let src = Contract.implicit_contract account.pkh in
  Incremental.begin_construction b >>=? fun b ->
  Op.origination (I b) contract_1 >>=? fun (operation, new_contract) ->
  Incremental.add_operation b operation >>=? fun b ->
  transfer_and_check_balances ~loc:__LOC__ b new_contract src Alpha_context.Tez.one
  >>=? fun (b, _) ->
  Incremental.finalize_block b >>=? fun _ ->
  return_unit

(** Checking that the sender of a transaction is the actual
    manager of the contract.
    Ownership of sender manager key (in case of a contract) *)
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

(* Slow tests case *)

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

(** 1- Create a block with two contracts;
    2- Bake 10 blocks with a transfer each time. *)
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

(** 1- Create a block;
    2- transfer an amount of tez that is bigger than the balance of the source
       contract;
    3a- If fee is smaller than the balance:
        Transfer is accepted but not processed. Fees are taken.
    3b- If fee higher than the balance: raises an `Balance_too_low` error.
*)
let balance_too_low fee () =
  register_two_contracts () >>=? fun (b, contract_1, contract_2) ->
  Incremental.begin_construction b >>=? fun i ->
  Context.Contract.balance (I i) contract_1 >>=? fun balance1 ->
  Context.Contract.balance (I i) contract_2 >>=? fun balance2 ->
  Op.transaction ~fee (I i) contract_1 contract_2 Tez.max_tez >>=? fun op ->
  let expect_failure = function
    | Alpha_environment.Ecoproto_error (Contract_storage.Balance_too_low _) :: _ ->
        return_unit
    | _ ->
        failwith "balance too low should fail"
  in
  if fee > balance1 then begin
    Incremental.add_operation ~expect_failure i op >>= fun _res ->
    return_unit
  end
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

(** 1- Create a block;
    2- Do two transfers one after another;
    3- Add two transfers into the block sequently;
    4- Catch the expected error: Counter_in_the_past. *)
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

(** 1- Create a block;
    2- Add a transfer into this block;
    3- Make another transfer, but did not add this transfer into the block,
       instead add the previous transfer again into this block;
    4- Catch the expected error: Counter_in_the_past. *)
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

(** 1- Create a block;
    2- Originate an unspendable new contract;
    3- Make a transfer from this contract;
    4- Catch the expected error: Unspendable_contract. *)
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
  Incremental.begin_construction b >>=? fun b ->
  let source = random_contract contracts in
  let dest = random_contract contracts in
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
  Test.tztest "block with a single transfer" `Quick block_with_a_single_transfer ;
  Test.tztest "transfer zero tez" `Quick transfer_zero_tez ;
  Test.tztest "block with a single transfer with fee" `Quick block_with_a_single_transfer_with_fee ;
  Test.tztest "block originate and transfer with fee" `Quick block_originate_and_transfer_with_fee ;
  Test.tztest "block transfer from contract balance" `Quick block_transfer_from_contract_balance ;
  Test.tztest "block transfers without and with fee to itself" `Quick block_transfers_without_with_fee_to_self ;
  Test.tztest "four transfers but bake three transfers" `Quick four_transfers_bake_three_transfers ;
  Test.tztest "transfer from an implicit to implicit contract " `Quick transfer_from_implicit_to_implicit_contract ;
  Test.tztest "transfer from an implicit to an originated contract" `Quick transfer_from_implicit_to_originated_contract ;
  Test.tztest "transfer from an originated to an originated contract" `Quick transfer_from_originated_to_originated ;
  Test.tztest "transfer from an originated to an implicit contract" `Quick transfer_from_originated_to_implicit ;
  Test.tztest "ownership sender" `Quick  ownership_sender ;

  (* Slow tests *)
  Test.tztest "block with multiple transfers" `Quick block_with_multiple_transfers ;
  Test.tztest "block with multiple transfer paying fee" `Quick block_with_multiple_transfers_pay_fee ;
  Test.tztest "block with multiple transfer without paying fee" `Quick block_with_multiple_transfers_with_without_fee ;
  Test.tztest "build a chain" `Quick build_a_chain ;

  (* Erroneous *)
  Test.tztest "balance too low" `Quick (balance_too_low  Tez.zero);
  Test.tztest "balance too low" `Quick (balance_too_low  Tez.one);
  Test.tztest "balance too low (max fee)" `Quick (balance_too_low  Tez.max_tez);
  Test.tztest "balance too low with two transfers" `Quick (balance_too_low_two_transfers Tez.zero);
  Test.tztest "balance too low with two transfers" `Quick (balance_too_low_two_transfers Tez.one);
  Test.tztest "invalid_counter" `Quick invalid_counter ;
  Test.tztest "add the same operation twice" `Quick  add_the_same_operation_twice ;
  Test.tztest "unspendable contract" `Quick unspendable_contract ;

  (* Random tests *)
  Test.tztest "random transfer" `Quick random_transfer ;
  Test.tztest "random multi transfer" `Quick  random_multi_transactions ;
]
