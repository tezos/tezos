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

(** Multiple operations can be grouped in one ensuring their
    derministic application.

    If an invalid operation is present in this group of operation, the
    previous applied operations are backtracked leaving the context
    unchanged and the following operations are skipped. Fees attributed
    to the operations are collected by the baker nonetheless.

    Only manager operations are allowed in multiple transactions.
    They must all belong to the same manager as there is only one signature. *)

open Proto_alpha
open Test_tez
open Test_utils

let ten_tez = Tez.of_int 10

(** Groups ten transactions between the same parties. *)
let multiple_transfers () =
  Context.init 3 >>=? fun (blk, contracts) ->
  let c1 = List.nth contracts 0 in
  let c2 = List.nth contracts 1 in
  let c3 = List.nth contracts 2 in

  map_s (fun _ ->
      Op.transaction (B blk) c1 c2 Tez.one
    ) (1--10) >>=? fun ops ->

  Op.combine_operations ~source:c1 (B blk) ops >>=? fun operation ->

  Context.Contract.balance (B blk) c1 >>=? fun c1_old_balance ->
  Context.Contract.balance (B blk) c2 >>=? fun c2_old_balance ->
  Context.Contract.pkh c3 >>=? fun baker_pkh ->
  Block.bake ~policy:(By_account baker_pkh) ~operation blk >>=? fun blk ->

  Assert.balance_was_debited ~loc:__LOC__
    (B blk) c1 c1_old_balance (Tez.of_int 10) >>=? fun () ->
  Assert.balance_was_credited ~loc:__LOC__
    (B blk) c2 c2_old_balance (Tez.of_int 10) >>=? fun () ->
  return_unit


(** Groups ten delegated originations. *)
let multiple_origination_and_delegation () =
  Context.init 2 >>=? fun (blk, contracts) ->
  let c1 = List.nth contracts 0 in
  let n = 10 in
  Context.get_constants (B blk) >>=? fun { parametric = { origination_size ; cost_per_byte ; _ } ; _ } ->
  Context.Contract.pkh c1 >>=? fun delegate_pkh ->

  let new_accounts = List.map (fun _ -> Account.new_account ()) (1 -- n) in
  mapi_s (fun i { Account.pk ; _ } ->
      Op.origination ~delegate:delegate_pkh ~counter:(Z.of_int i) ~fee:Tez.zero
        ~public_key:pk ~spendable:true ~credit:(Tez.of_int 10) (B blk) c1
    ) new_accounts >>=? fun originations ->
  (* These computed originated contracts are not the ones really created *)
  (* We will extract them from the tickets *)
  let (originations_operations, _) = List.split originations in

  Op.combine_operations ~source:c1 (B blk) originations_operations >>=? fun operation ->

  Context.Contract.balance (B blk) c1 >>=? fun c1_old_balance ->
  Incremental.begin_construction blk >>=? fun inc ->
  Incremental.add_operation inc operation >>=? fun inc ->

  (* To retrieve the originated contracts, it is easier to extract them
     from the tickets. Else, we could (could we ?) hash each combined
     operation individually. *)
  let tickets = Incremental.rev_tickets inc in
  let open Apply_results in
  let tickets =
    List.fold_left (fun acc -> function
        | No_operation_metadata -> assert false
        | Operation_metadata { contents } ->
            to_list (Contents_result_list contents) @ acc
      ) [] tickets |> List.rev in
  let new_contracts =
    List.map (function
        | Contents_result
            (Manager_operation_result
               { operation_result =
                   Applied (Origination_result { originated_contracts = [ h ] ; _ })
               ; _ }) ->
            h
        | _ -> assert false
      ) tickets in

  (* Previous balance - (Credit (n * 10tz) + Origination cost (n tz)) *)
  Tez.(cost_per_byte *? Int64.of_int origination_size) >>?= fun origination_burn ->
  Tez.(origination_burn *? (Int64.of_int n)) >>?= fun origination_total_cost ->
  Tez.((Tez.of_int (10 * n)) +? origination_total_cost) >>?= fun total_cost ->
  Assert.balance_was_debited ~loc:__LOC__
    (I inc) c1 c1_old_balance total_cost >>=? fun () ->

  iter_s (fun c ->
      Assert.balance_is ~loc:__LOC__ (I inc) c (Tez.of_int 10)
    ) new_contracts >>=? fun () ->

  return_unit

let expect_balance_too_low = function
  | Alpha_environment.Ecoproto_error (Contract_storage.Balance_too_low _) :: _ ->
      return_unit
  | _ ->
      failwith "Contract should not have a sufficient balance : operation expected to fail."

(** Groups three operations, the midlle one failing.
    Checks that the receipt is consistent.
    Variant without fees. *)
let failing_operation_in_the_middle () =
  Context.init 2 >>=? fun (blk, contracts) ->
  let c1 = List.nth contracts 0 in
  let c2 = List.nth contracts 1 in

  Op.transaction ~fee:Tez.zero (B blk) c1 c2 Tez.one >>=? fun op1 ->
  Op.transaction ~fee:Tez.zero (B blk) c1 c2 Tez.max_tez >>=? fun op2 ->
  Op.transaction ~fee:Tez.zero (B blk) c1 c2 Tez.one >>=? fun op3 ->
  let operations = [ op1 ; op2 ; op3 ] in

  Op.combine_operations ~source:c1 (B blk) operations >>=? fun operation ->

  Context.Contract.balance (B blk) c1 >>=? fun c1_old_balance ->
  Context.Contract.balance (B blk) c2 >>=? fun c2_old_balance ->

  Incremental.begin_construction blk >>=? fun inc ->
  Incremental.add_operation
    ~expect_failure:expect_balance_too_low inc operation >>=? fun inc ->

  let tickets = Incremental.rev_tickets inc in
  let open Apply_results in
  let tickets =
    List.fold_left (fun acc -> function
        | No_operation_metadata -> assert false
        | Operation_metadata { contents } ->
            to_list (Contents_result_list contents) @ acc
      ) [] tickets in
  begin match tickets with
    | Contents_result (Manager_operation_result { operation_result = (Backtracked _) ; _ }) ::
      Contents_result (Manager_operation_result { operation_result = Failed (_, [ Contract_storage.Balance_too_low _ ]) ; _ }) ::
      Contents_result (Manager_operation_result { operation_result = Skipped _ ; _ }) ::
      _ -> ()
    | _ -> assert false
  end ;

  Assert.balance_is ~loc:__LOC__ (I inc) c1 c1_old_balance >>=? fun () ->
  Assert.balance_is ~loc:__LOC__ (I inc) c2 c2_old_balance >>=? fun () ->

  return_unit

(** Groups three operations, the midlle one failing.
    Checks that the receipt is consistent.
    Variant with fees, that should be spent even in case of failure. *)
let failing_operation_in_the_middle_with_fees () =
  Context.init 2 >>=? fun (blk, contracts) ->
  let c1 = List.nth contracts 0 in
  let c2 = List.nth contracts 1 in

  Op.transaction ~fee:Tez.one (B blk) c1 c2 Tez.one >>=? fun op1 ->
  Op.transaction ~fee:Tez.one (B blk) c1 c2 Tez.max_tez >>=? fun op2 ->
  Op.transaction ~fee:Tez.one (B blk) c1 c2 Tez.one >>=? fun op3 ->
  let operations = [ op1 ; op2 ; op3 ] in

  Op.combine_operations ~source:c1 (B blk) operations >>=? fun operation ->

  Context.Contract.balance (B blk) c1 >>=? fun c1_old_balance ->
  Context.Contract.balance (B blk) c2 >>=? fun c2_old_balance ->

  Incremental.begin_construction blk >>=? fun inc ->
  Incremental.add_operation
    ~expect_failure:expect_balance_too_low inc operation >>=? fun inc ->

  let tickets = Incremental.rev_tickets inc in
  let open Apply_results in
  let tickets =
    List.fold_left (fun acc -> function
        | No_operation_metadata -> assert false
        | Operation_metadata { contents } ->
            to_list (Contents_result_list contents) @ acc
      ) [] tickets in
  begin match tickets with
    | Contents_result (Manager_operation_result { operation_result = (Backtracked _) ; _ }) ::
      Contents_result (Manager_operation_result { operation_result = Failed (_, [ Contract_storage.Balance_too_low _ ]) ; _ }) ::
      Contents_result (Manager_operation_result { operation_result = Skipped _ ; _ }) ::
      _ -> ()
    | _ -> assert false
  end ;

  (* In the presence of a failure, all the fees are collected. Even for skipped operations. *)
  Assert.balance_was_debited ~loc:__LOC__ (I inc) c1 c1_old_balance (Tez.of_int 3) >>=? fun () ->
  Assert.balance_is ~loc:__LOC__ (I inc) c2 c2_old_balance >>=? fun () ->

  return_unit

let tests = [
  Test.tztest "multiple transfers" `Quick multiple_transfers ;
  Test.tztest "multiple originations and delegations" `Quick multiple_origination_and_delegation ;
  Test.tztest "Failing operation in the middle" `Quick failing_operation_in_the_middle ;
  Test.tztest "Failing operation in the middle (with fees)" `Quick failing_operation_in_the_middle_with_fees ;
]
