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

let account_pair = function
  | [a1; a2] -> (a1, a2)
  | _ -> assert false

let wrap e = Lwt.return (Alpha_environment.wrap_error e)
let traverse_rolls ctxt head =
  let rec loop acc roll =
    Storage.Roll.Successor.get_option ctxt roll >>= wrap >>=? function
    | None -> return (List.rev acc)
    | Some next -> loop (next :: acc) next in
  loop [head] head

let get_rolls ctxt delegate =
  Storage.Roll.Delegate_roll_list.get_option ctxt delegate >>= wrap >>=? function
  | None -> return_nil
  | Some head_roll -> traverse_rolls ctxt head_roll

let check_rolls b (account:Account.t) =
  Context.get_constants (B b) >>=? fun constants ->
  Context.Delegate.info (B b) account.pkh >>=? fun { staking_balance ; _ } ->
  let token_per_roll = constants.parametric.tokens_per_roll in
  let expected_rolls = Int64.div (Tez.to_mutez staking_balance) (Tez.to_mutez token_per_roll) in
  Raw_context.prepare b.context
    ~level:b.header.shell.level
    ~timestamp:b.header.shell.timestamp
    ~fitness:b.header.shell.fitness >>= wrap >>=? fun ctxt ->
  get_rolls ctxt account.pkh >>=? fun rolls ->
  Assert.equal_int ~loc:__LOC__ (List.length rolls) (Int64.to_int expected_rolls)

let check_no_rolls (b : Block.t) (account:Account.t) =
  Raw_context.prepare b.context
    ~level:b.header.shell.level
    ~timestamp:b.header.shell.timestamp
    ~fitness:b.header.shell.fitness >>= wrap >>=? fun ctxt ->
  get_rolls ctxt account.pkh >>=? fun rolls ->
  Assert.equal_int ~loc:__LOC__ (List.length rolls) 0

let simple_staking_rights () =
  Context.init 2 >>=? fun (b,accounts) ->
  let (a1, _a2) = account_pair accounts in

  Context.Contract.balance (B b) a1 >>=? fun balance ->
  Context.Contract.manager (B b) a1 >>=? fun m1 ->

  Context.Delegate.info (B b) m1.pkh >>=? fun info ->
  Assert.equal_tez ~loc:__LOC__ balance info.staking_balance >>=? fun () ->
  check_rolls b m1

let simple_staking_rights_after_baking () =
  Context.init 2 >>=? fun (b,accounts) ->
  let (a1, a2) = account_pair accounts in

  Context.Contract.balance (B b) a1 >>=? fun balance ->
  Context.Contract.manager (B b) a1 >>=? fun m1 ->
  Context.Contract.manager (B b) a2 >>=? fun m2 ->

  Block.bake_n ~policy:(By_account m2.pkh) 5 b >>=? fun b ->

  Context.Delegate.info (B b) m1.pkh >>=? fun info ->
  Assert.equal_tez ~loc:__LOC__ balance info.staking_balance >>=? fun () ->
  check_rolls b m1 >>=? fun () ->
  check_rolls b m2

let frozen_deposit (info:Context.Delegate.info) =
  Cycle.Map.fold (fun _ { Delegate.deposit ; _ } acc ->
      Test_tez.Tez.(deposit + acc))
    info.frozen_balance_by_cycle Tez.zero

let check_activate_staking_balance ~loc ~deactivated b (a, (m:Account.t)) =
  Context.Delegate.info (B b) m.pkh >>=? fun info ->
  Assert.equal_bool ~loc info.deactivated deactivated >>=? fun () ->
  Context.Contract.balance (B b) a >>=? fun balance ->
  let deposit = frozen_deposit info in
  Assert.equal_tez ~loc Test_tez.Tez.(balance + deposit) info.staking_balance

let run_until_deactivation () =
  Context.init ~preserved_cycles:2 2 >>=? fun (b,accounts) ->
  let (a1, a2) = account_pair accounts in

  Context.Contract.balance (B b) a1 >>=? fun balance_start ->
  Context.Contract.manager (B b) a1 >>=? fun m1 ->
  Context.Contract.manager (B b) a2 >>=? fun m2 ->

  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b (a1,m1) >>=? fun () ->

  Context.Delegate.info (B b) m1.pkh >>=? fun info ->
  Block.bake_until_cycle ~policy:(By_account m2.pkh) info.grace_period b >>=? fun b ->

  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b (a1,m1) >>=? fun () ->

  Block.bake_until_cycle_end ~policy:(By_account m2.pkh) b >>=? fun b ->

  check_activate_staking_balance ~loc:__LOC__ ~deactivated:true b (a1,m1) >>=? fun () ->
  return (b, ((a1, m1), balance_start), (a2, m2))

let deactivation_then_bake () =
  run_until_deactivation () >>=?
  fun (b, ((_deactivated_contract, deactivated_account) as deactivated, _start_balance),
       (_a2, _m2)) ->

  Block.bake ~policy:(By_account deactivated_account.pkh) b >>=? fun b ->

  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b deactivated >>=? fun () ->
  check_rolls b deactivated_account

let deactivation_then_self_delegation () =
  run_until_deactivation () >>=?
  fun (b, ((deactivated_contract, deactivated_account) as deactivated, start_balance),
       (_a2, m2)) ->

  Op.delegation (B b) deactivated_contract (Some deactivated_account.pkh) >>=? fun self_delegation ->

  Block.bake ~policy:(By_account m2.pkh) b ~operation:self_delegation >>=? fun b ->

  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b deactivated >>=? fun () ->
  Context.Contract.balance (B b) deactivated_contract >>=? fun balance ->
  Assert.equal_tez ~loc:__LOC__ start_balance balance >>=? fun () ->
  check_rolls b deactivated_account

let deactivation_then_empty_then_self_delegation () =
  run_until_deactivation () >>=?
  fun (b, ((deactivated_contract, deactivated_account) as deactivated, _start_balance),
       (_a2, m2)) ->
  (* empty the contract *)
  Context.Contract.balance (B b) deactivated_contract >>=? fun balance ->
  let sink_account = Account.new_account () in
  let sink_contract = Contract.implicit_contract sink_account.pkh in
  Context.get_constants (B b) >>=? fun { parametric = { origination_size ; cost_per_byte ; _ } ; _ } ->
  Tez.(cost_per_byte *? Int64.of_int origination_size) >>?= fun origination_burn ->
  let amount = match Tez.(balance -? origination_burn) with Ok r -> r | Error _ -> assert false in
  Op.transaction (B b) deactivated_contract sink_contract amount >>=? fun empty_contract ->
  Block.bake ~policy:(By_account m2.pkh) ~operation:empty_contract b >>=? fun b ->
  (* self delegation *)
  Op.delegation (B b) deactivated_contract (Some deactivated_account.pkh) >>=? fun self_delegation ->
  Block.bake ~policy:(By_account m2.pkh) ~operation:self_delegation b >>=? fun b ->

  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b deactivated >>=? fun () ->
  Context.Contract.balance (B b) deactivated_contract >>=? fun balance ->
  Assert.equal_tez ~loc:__LOC__ Tez.zero balance >>=? fun () ->
  check_rolls b deactivated_account

let deactivation_then_empty_then_self_delegation_then_recredit () =
  run_until_deactivation () >>=?
  fun (b, ((deactivated_contract, deactivated_account) as deactivated, balance),
       (_a2, m2)) ->
  (* empty the contract *)
  let sink_account = Account.new_account () in
  let sink_contract = Contract.implicit_contract sink_account.pkh in
  Context.get_constants (B b) >>=? fun { parametric = { origination_size ; cost_per_byte ; _ } ; _ } ->
  Tez.(cost_per_byte *? Int64.of_int origination_size) >>?= fun origination_burn ->
  let amount = match Tez.(balance -? origination_burn) with Ok r -> r | Error _ -> assert false in
  Op.transaction (B b) deactivated_contract sink_contract amount >>=? fun empty_contract ->
  Block.bake ~policy:(By_account m2.pkh) ~operation:empty_contract b >>=? fun b ->
  (* self delegation *)
  Op.delegation (B b) deactivated_contract (Some deactivated_account.pkh) >>=? fun self_delegation ->
  Block.bake ~policy:(By_account m2.pkh) ~operation:self_delegation b >>=? fun b ->
  (* recredit *)
  Op.transaction (B b) sink_contract deactivated_contract amount >>=? fun recredit_contract ->
  Block.bake ~policy:(By_account m2.pkh) ~operation:recredit_contract b >>=? fun b ->

  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b deactivated >>=? fun () ->
  Context.Contract.balance (B b) deactivated_contract >>=? fun balance ->
  Assert.equal_tez ~loc:__LOC__ amount balance >>=? fun () ->
  check_rolls b deactivated_account

let delegation () =
  Context.init 2 >>=? fun (b,accounts) ->
  let (a1, a2) = account_pair accounts in
  let m3 = Account.new_account () in
  Account.add_account m3;

  Context.Contract.manager (B b) a1 >>=? fun m1 ->
  Context.Contract.manager (B b) a2 >>=? fun m2 ->
  let a3 = Contract.implicit_contract m3.pkh in

  Context.Contract.delegate_opt (B b) a1 >>=? fun delegate ->
  begin
    match delegate with
    | None -> assert false
    | Some pkh ->
        assert (Signature.Public_key_hash.equal pkh m1.pkh)
  end;

  Op.transaction (B b) a1 a3 Tez.fifty_cents >>=? fun transact ->

  Block.bake ~policy:(By_account m2.pkh) b ~operation:transact >>=? fun b ->

  Context.Contract.delegate_opt (B b) a3 >>=? fun delegate ->
  begin
    match delegate with
    | None -> ()
    | Some _ -> assert false
  end;
  check_no_rolls b m3 >>=? fun () ->

  Op.delegation (B b) a3 (Some m3.pkh) >>=? fun delegation ->
  Block.bake ~policy:(By_account m2.pkh) b ~operation:delegation >>=? fun b ->

  Context.Contract.delegate_opt (B b) a3 >>=? fun delegate ->
  begin
    match delegate with
    | None -> assert false
    | Some pkh ->
        assert (Signature.Public_key_hash.equal pkh m3.pkh)
  end;
  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b (a3,m3) >>=? fun () ->
  check_rolls b m3 >>=? fun () ->
  check_rolls b m1

let tests = [
  Test.tztest "simple staking rights" `Quick (simple_staking_rights) ;
  Test.tztest "simple staking rights after baking" `Quick (simple_staking_rights_after_baking) ;
  Test.tztest "deactivation then bake" `Quick (deactivation_then_bake) ;
  Test.tztest "deactivation then self delegation" `Quick (deactivation_then_self_delegation) ;
  Test.tztest "deactivation then empty then self delegation" `Quick (deactivation_then_empty_then_self_delegation) ;
  Test.tztest "deactivation then empty then self delegation then recredit" `Quick (deactivation_then_empty_then_self_delegation_then_recredit) ;
  Test.tztest "delegation" `Quick (delegation) ;
]
