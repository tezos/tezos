(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

let account_pair = function
  | [a1; a2] -> (a1, a2)
  | _ -> assert false

let simple_staking_rights () =
  Context.init 2 >>=? fun (b,accounts) ->
  let (a1, a2) = account_pair accounts in

  Context.Contract.balance (B b) a1 >>=? fun balance ->
  Context.Contract.manager (B b) a1 >>=? fun m1 ->

  Context.Delegate.info (B b) m1.pkh >>=? fun info ->
  Assert.equal_tez ~loc:__LOC__ balance info.staking_balance

let simple_staking_rights_after_baking () =
  Context.init 2 >>=? fun (b,accounts) ->
  let (a1, a2) = account_pair accounts in

  Context.Contract.balance (B b) a1 >>=? fun balance ->
  Context.Contract.manager (B b) a1 >>=? fun m1 ->
  Context.Contract.manager (B b) a2 >>=? fun m2 ->

  Block.bake_n ~policy:(By_account m2.pkh) 5 b >>=? fun b ->

  Context.Delegate.info (B b) m1.pkh >>=? fun info ->
  Assert.equal_tez ~loc:__LOC__ balance info.staking_balance

let frozen_deposit (info:Context.Delegate.info) =
  Cycle.Map.fold (fun _ { Delegate.deposit } acc ->
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
  fun (b, ((deactivated_contract, deactivated_account) as deactivated, _start_balance),
       (a2, m2)) ->

  Block.bake ~policy:(By_account deactivated_account.pkh) b >>=? fun b ->

  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b deactivated

let deactivation_then_self_delegation () =
  run_until_deactivation () >>=?
  fun (b, ((deactivated_contract, deactivated_account) as deactivated, start_balance),
       (a2, m2)) ->

  Op.delegation (B b) deactivated_contract (Some deactivated_account.pkh) >>=? fun self_delegation ->

  Block.bake ~policy:(By_account m2.pkh) b ~operation:self_delegation >>=? fun b ->

  check_activate_staking_balance ~loc:__LOC__ ~deactivated:false b deactivated >>=? fun () ->
  Context.Contract.balance (B b) deactivated_contract >>=? fun balance ->
  Assert.equal_tez ~loc:__LOC__ start_balance balance


let tests = [
  Test.tztest "simple staking rights" `Quick (simple_staking_rights) ;
  Test.tztest "simple staking rights after baking" `Quick (simple_staking_rights_after_baking) ;
  Test.tztest "deactivation then bake" `Quick (deactivation_then_bake) ;
  Test.tztest "deactivation then self delegation" `Quick (deactivation_then_self_delegation) ;
]
