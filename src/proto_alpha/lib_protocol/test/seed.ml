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

(** Tests that baking [blocks_per_cycle] blocks without a
    [seed_nonce_hash] commitment fails with [Invalid_commitment] *)
let no_commitment () =
  Context.init 5 >>=? fun (b,_) ->
  Context.get_constants (B b) >>=? fun { parametric = csts } ->
  let blocks_per_cycle = Int32.to_int csts.blocks_per_cycle in

  (* Bake normally until before the commitment *)
  Block.bake_n (blocks_per_cycle-2) b >>=? fun b ->

  (* Forge a block with empty commitment and apply it *)
  Block.Forge.forge_header b >>=? fun header ->
  let header = Block.Forge.set_seed_nonce_hash None header in
  Block.apply header b >>= fun e ->

  let invalid_commitment = function
    | Proto_alpha.Apply.Invalid_commitment _ -> true
    | _ -> false in
  Assert.proto_error ~loc:__LOC__ e invalid_commitment


(** checks that if a baker of cycle 0 doesn't reveal its seed_nonce at cycle 1,
    it will loose its rewards and fees at the end of cycle 1 *)
let no_revelation () =
  let open Assert in

  Context.init 5 >>=? fun (b,_) ->
  Context.get_constants (B b) >>=? fun csts ->
  let bond = csts.parametric.block_security_deposit in
  let reward = csts.parametric.block_reward in
  let blocks_per_commitment = Int32.to_int csts.parametric.blocks_per_commitment in

  (* bake until commitment *)
  Block.bake_n (blocks_per_commitment-2) b >>=? fun b ->

  (* bake with commitment *)
  Block.get_next_baker b >>=? fun (pkh,_,_) ->
  let id = Contract.implicit_contract pkh in
  Context.Contract.balance (B b) id >>=? fun bal_main ->
  Context.Contract.balance ~kind:Deposit (B b) id >>=? fun bal_deposit ->
  Context.Contract.balance ~kind:Rewards (B b) id >>=? fun bal_rewards ->
  Block.bake ~policy:(Block.By_account pkh) b >>=? fun b ->
  balance_was_debited ~loc:__LOC__
    (B b) id bal_main bond >>=? fun () ->
  balance_was_credited ~loc:__LOC__
    (B b) id ~kind:Deposit bal_deposit bond >>=? fun () ->
  balance_was_credited ~loc:__LOC__
    (B b) id ~kind:Rewards bal_rewards reward >>=? fun () ->

  Context.Contract.balance (B b) id >>=? fun bal_main ->
  Context.Contract.balance ~kind:Rewards (B b) id >>=? fun bal_rewards ->
  (* finish the cycle excluding the committing baker *)
  let policy = Block.Excluding [pkh] in
  Block.bake_until_cycle_end ~policy b >>=? fun b ->
  (* one cycle w/o revelation (or baking) from committing baker *)
  Block.bake_until_cycle_end ~policy b >>=? fun b ->

  (* the reward is lost *)
  balance_is ~loc:__LOC__ (B b) id ~kind:Main bal_main >>=? fun () ->
  balance_was_debited ~loc:__LOC__
    (B b) id ~kind:Rewards bal_rewards reward >>=? fun _ ->
  return ()


let tests = [
  Test.tztest "no commitment" `Quick no_commitment;
  Test.tztest "no revelation" `Quick no_revelation;
]
