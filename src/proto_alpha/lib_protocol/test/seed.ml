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

(** Tests about
    - seed_nonce_hash included in some blocks
    - revelation operation of seed_nonce that should correspond to each
      seed_nonce_hash
*)

open Proto_alpha
open Test_tez

(** Tests that baking [blocks_per_commitment] blocks without a
    [seed_nonce_hash] commitment fails with [Invalid_commitment] *)
let no_commitment () =
  Context.init 5 >>=? fun (b,_) ->
  Context.get_constants (B b) >>=? fun { parametric = { blocks_per_commitment ; _ } ; _ } ->
  let blocks_per_commitment = Int32.to_int blocks_per_commitment in

  (* Bake normally until before the commitment *)
  Block.bake_n (blocks_per_commitment-2) b >>=? fun b ->

  (* Forge a block with empty commitment and apply it *)
  Block.Forge.forge_header b >>=? fun header ->
  Block.Forge.set_seed_nonce_hash None header |>
  Block.Forge.sign_header >>=? fun header ->
  Block.apply header b >>= fun e ->

  Assert.proto_error ~loc:__LOC__ e begin function
    | Apply.Invalid_commitment _ -> true
    | _ -> false
  end

(** Choose a baker, denote it by id. In the first cycle, make id bake only once.
    Test that:
    - after id bakes with a commitment the bond is frozen and the reward allocated
    - when id reveals the nonce too early, there's an error
    - when id reveals at the right time but the wrong value, there's an error
    - when another baker reveals correctly, it receives the tip
    - revealing twice produces an error
    - after [preserved cycles] a committer that correctly revealed
      receives back the bond and the reward
*)
let revelation_early_wrong_right_twice () =
  let open Assert in

  Context.init 5 >>=? fun (b,_) ->
  Context.get_constants (B b) >>=? fun csts ->
  let bond = csts.parametric.block_security_deposit in
  let reward = csts.parametric.block_reward in
  let tip = csts.parametric.seed_nonce_revelation_tip in
  let blocks_per_commitment = Int32.to_int csts.parametric.blocks_per_commitment in
  let preserved_cycles = csts.parametric.preserved_cycles in

  (* get the pkh of a baker *)
  Block.get_next_baker b >>=? fun (pkh,_,_) ->
  let id = Alpha_context.Contract.implicit_contract pkh in
  let policy = Block.Excluding [pkh] in
  (* bake until commitment, excluding id *)
  Block.bake_n ~policy (blocks_per_commitment-2) b >>=? fun b ->
  Context.Contract.balance ~kind:Main (B b) id >>=? fun bal_main ->
  Context.Contract.balance ~kind:Deposit (B b) id >>=? fun bal_deposit ->
  Context.Contract.balance ~kind:Rewards (B b) id >>=? fun bal_rewards ->

  (* the baker [id] will include a seed_nonce commitment *)
  Block.bake ~policy:(Block.By_account pkh) b >>=? fun b ->
  Context.get_level (B b) >>=? fun level_commitment ->
  Context.get_seed_nonce_hash (B b) >>=? fun committed_hash ->

  (* test that the bond was frozen and the reward allocated *)
  balance_was_debited ~loc:__LOC__
    (B b) id bal_main bond >>=? fun () ->
  balance_was_credited ~loc:__LOC__
    (B b) id ~kind:Deposit bal_deposit bond >>=? fun () ->
  balance_was_credited ~loc:__LOC__
    (B b) id ~kind:Rewards bal_rewards reward >>=? fun () ->

  (* test that revealing too early produces an error *)
  Op.seed_nonce_revelation (B b) level_commitment (Nonce.get committed_hash) >>=? fun operation ->

  Block.bake ~policy ~operation b >>= fun e ->
  let expected = function
    | Nonce_storage.Too_early_revelation -> true
    | _ -> false in
  Assert.proto_error ~loc:__LOC__ e expected >>=? fun () ->

  (* finish the cycle excluding the committing baker, id *)
  Block.bake_until_cycle_end ~policy b >>=? fun b ->

  (* test that revealing at the right time but the wrong value produces an error *)
  let wrong_hash,_ = Nonce.generate () in
  Op.seed_nonce_revelation (B b) level_commitment (Nonce.get wrong_hash) >>=? fun operation ->
  Block.bake ~operation b >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e begin function
    | Nonce_storage.Unexpected_nonce -> true
    | _ -> false
  end >>=? fun () ->

  (* reveals correctly *)
  Op.seed_nonce_revelation (B b) level_commitment (Nonce.get committed_hash) >>=? fun operation ->
  Block.get_next_baker ~policy b >>=? fun (baker_pkh,_,_) ->
  let baker = Alpha_context.Contract.implicit_contract baker_pkh in
  Context.Contract.balance ~kind:Main (B b) baker >>=? fun baker_bal_main ->
  Context.Contract.balance ~kind:Deposit (B b) baker >>=? fun baker_bal_deposit ->
  Context.Contract.balance ~kind:Rewards (B b) baker >>=? fun baker_bal_rewards ->

  (* bake the operation in a block *)
  Block.bake ~policy ~operation b >>=? fun b ->

  (* test that the baker gets the tip reward *)
  balance_was_debited ~loc:__LOC__
    (B b) baker ~kind:Main baker_bal_main bond >>=? fun () ->
  balance_was_credited ~loc:__LOC__
    (B b) baker ~kind:Deposit baker_bal_deposit bond >>=? fun () ->
  Lwt.return @@ Tez.(+?) reward tip >>=? fun expected_rewards ->
  balance_was_credited ~loc:__LOC__
    (B b) baker ~kind:Rewards baker_bal_rewards expected_rewards >>=? fun () ->

  (* test that revealing twice produces an error *)
  Op.seed_nonce_revelation (B b) level_commitment (Nonce.get wrong_hash) >>=? fun operation ->
  Block.bake ~operation ~policy b >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e begin function
    | Nonce_storage.Previously_revealed_nonce -> true
    | _ -> false
  end >>=? fun () ->

  (* bake [preserved_cycles] cycles excluding [id] *)
  Error_monad.fold_left_s (fun b _ -> Block.bake_until_cycle_end ~policy b)
    b (1 -- preserved_cycles) >>=? fun b ->

  (* test that [id] receives back the bond and the reward *)
  (* note that in order to have that new_bal = bal_main + reward,
     id can only bake once; this is why we exclude id from all other bake ops. *)
  balance_was_credited ~loc:__LOC__
    (B b) id ~kind:Main bal_main reward >>=? fun () ->
  balance_is ~loc:__LOC__
    (B b) id ~kind:Deposit Tez.zero >>=? fun () ->
  balance_is ~loc:__LOC__
    (B b) id ~kind:Rewards Tez.zero


(** Tests that:
    - a committer at cycle 0, which doesn't reveal at cycle 1,
      at the end of the cycle 1 looses the bond and the reward
    - revealing too late produces an error
*)
let revelation_missing_and_late () =
  let open Context in
  let open Assert in

  Context.init 5 >>=? fun (b,_) ->
  get_constants (B b) >>=? fun csts ->
  let reward = csts.parametric.block_reward in
  let blocks_per_commitment = Int32.to_int csts.parametric.blocks_per_commitment in

  (* bake until commitment *)
  Block.bake_n (blocks_per_commitment-2) b >>=? fun b ->
  (* the next baker [id] will include a seed_nonce commitment *)
  Block.get_next_baker b >>=? fun (pkh,_,_) ->
  let id = Alpha_context.Contract.implicit_contract pkh in
  Block.bake b >>=? fun b ->
  Context.get_level (B b) >>=? fun level_commitment ->
  Context.get_seed_nonce_hash (B b) >>=? fun committed_hash ->
  Context.Contract.balance ~kind:Main (B b) id >>=? fun bal_main ->
  Context.Contract.balance ~kind:Deposit (B b) id >>=? fun bal_deposit ->
  Context.Contract.balance ~kind:Rewards (B b) id >>=? fun bal_rewards ->

  (* finish cycle 0 excluding the committing baker [id] *)
  let policy = Block.Excluding [pkh] in
  Block.bake_until_cycle_end ~policy b >>=? fun b ->
  (* finish cycle 1 excluding the committing baker [id] *)
  Block.bake_until_cycle_end ~policy b >>=? fun b ->

  (* test that baker [id], which didn't reveal at cycle 1 like it was supposed to,
     at the end of the cycle 1 looses the reward but not the bond *)
  balance_is ~loc:__LOC__ (B b) id ~kind:Main bal_main >>=? fun () ->
  balance_is ~loc:__LOC__ (B b) id ~kind:Deposit bal_deposit >>=? fun () ->
  balance_was_debited ~loc:__LOC__
    (B b) id ~kind:Rewards bal_rewards reward >>=? fun () ->

  (* test that revealing too late (after cycle 1) produces an error *)
  Op.seed_nonce_revelation (B b) level_commitment (Nonce.get committed_hash) >>=? fun operation ->
  Block.bake ~operation b >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e begin function
    | Nonce_storage.Too_late_revelation -> true
    | _ -> false
  end


let tests = [
  Test.tztest "no commitment" `Quick no_commitment ;
  Test.tztest "revelation_early_wrong_right_twice" `Quick revelation_early_wrong_right_twice ;
  Test.tztest "revelation_missing_and_late" `Quick revelation_missing_and_late ;
]
