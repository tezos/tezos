(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Double baking evidence operation may happen when a baker
    baked two different blocks on the same level. *)

open Proto_alpha
open Alpha_context

(****************************************************************)
(*                  Utility functions                           *)
(****************************************************************)

let get_first_different_baker baker bakers =
  return @@ List.find (fun baker' ->
      Signature.Public_key_hash.(<>) baker baker')
    bakers

let get_first_different_bakers ctxt =
  Context.get_bakers ctxt >>=? fun bakers ->
  let baker_1 = List.hd bakers in
  get_first_different_baker baker_1 (List.tl bakers) >>=? fun baker_2 ->
  return (baker_1, baker_2)

let get_first_different_endorsers ctxt =
  Context.get_endorsers ctxt >>=? fun endorsers ->
  let endorser_1 = (List.hd endorsers).delegate in
  let endorser_2 = (List.hd (List.tl endorsers)).delegate in
  return (endorser_1, endorser_2)

(** Bake two block at the same level using the same policy (i.e. same
    baker) *)
let block_fork ?policy contracts b =
  let (contract_a, contract_b) =
    List.hd contracts, List.hd (List.tl contracts) in
  Op.transaction (B b) contract_a contract_b  Alpha_context.Tez.one_cent >>=? fun operation ->
  Block.bake ?policy ~operation b >>=? fun blk_a ->
  Block.bake ?policy b >>=? fun blk_b ->
  return (blk_a, blk_b)

(****************************************************************)
(*                        Tests                                 *)
(****************************************************************)

(** Simple scenario where two blocks are baked by a same baker and
    exposed by a double baking evidence operation *)
let valid_double_baking_evidence () =
  Context.init 2 >>=? fun (b, contracts) ->

  Context.get_bakers (B b) >>=? fun bakers ->
  let priority_0_baker = List.hd bakers in

  block_fork ~policy:(By_priority 0) contracts b >>=? fun (blk_a, blk_b) ->

  Op.double_baking (B blk_a) blk_a.header blk_b.header >>=? fun operation ->
  Block.bake ~policy:(Excluding [ priority_0_baker ]) ~operation blk_a >>=? fun blk ->

  (* Check that the frozen deposit, the fees and rewards are removed *)
  iter_s (fun kind ->
      let contract = Alpha_context.Contract.implicit_contract priority_0_baker in
      Assert.balance_is ~loc:__LOC__ (B blk) contract ~kind Tez.zero)
    [ Deposit ; Fees ; Rewards ]
(* TODO : check also that the baker receive half of the bad baker's frozen balance *)

(****************************************************************)
(*  The following test scenarios are supposed to raise errors.  *)
(****************************************************************)

(** Check that a double baking operation fails if it exposes the same two blocks *)
let same_blocks () =
  Context.init 2 >>=? fun (b, _contracts) ->
  Block.bake b >>=? fun ba ->
  Op.double_baking (B ba) ba.header ba.header >>=? fun operation ->
  Block.bake ~operation ba >>=? fun _ ->
  (* TODO: should fail *)
  return ()

(** Check that a double baking operation exposing two blocks with
    different levels fails *)
let different_levels () =
  Context.init 2 >>=? fun (b, contracts) ->

  block_fork ~policy:(By_priority 0) contracts b >>=? fun (blk_a, blk_b) ->

  Block.bake blk_b >>=? fun blk_b_2 ->

  Op.double_baking (B blk_a) blk_a.header blk_b_2.header >>=? fun operation ->
  Block.bake ~operation blk_a >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Apply.Invalid_double_baking_evidence _ -> true
    | _ -> false end

(** Check that a double baking operation exposing two yet to be baked
    blocks fails *)
let too_early_double_baking_evidence () =
  Context.init 2 >>=? fun (b, contracts) ->
  block_fork ~policy:(By_priority 0) contracts b >>=? fun (blk_a, blk_b) ->

  Op.double_baking (B b) blk_a.header blk_b.header >>=? fun operation ->
  Block.bake ~operation b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Apply.Too_early_double_baking_evidence _ -> true
    | _ -> false end

(** Check that after [preserved_cycles + 1], it is not possible to
    create a double baking operation anymore *)
let too_late_double_baking_evidence () =
  Context.init 2 >>=? fun (b, contracts) ->
  Context.get_constants (B b)
  >>=? fun Constants.{ parametric = { preserved_cycles ; _ } ; _ } ->

  block_fork ~policy:(By_priority 0) contracts b >>=? fun (blk_a, blk_b) ->

  fold_left_s (fun blk _ -> Block.bake_until_cycle_end blk)
    blk_a (1 -- (preserved_cycles + 1)) >>=? fun blk ->

  Op.double_baking (B blk) blk_a.header blk_b.header >>=? fun operation ->
  Block.bake ~operation blk >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res begin function
    | Apply.Outdated_double_baking_evidence _ -> true
    | _ -> false end

(** Check that an invalid double baking evidence that exposes two block
    baking with same level made by different bakers fails *)
let different_delegates () =
  Context.init 2 >>=? fun (b, _) ->

  get_first_different_bakers (B b) >>=? fun (baker_1, baker_2) ->
  Block.bake ~policy:(By_account baker_1) b >>=? fun blk_a ->
  Block.bake ~policy:(By_account baker_2) b >>=? fun blk_b ->

  Op.double_baking (B blk_a) blk_a.header blk_b.header >>=? fun operation ->
  Block.bake ~operation blk_a >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e begin function
    | Apply.Inconsistent_double_baking_evidence _ -> true
    | _ -> false end

let wrong_delegate () =
  (* TODO: change bake and forge_header "policy" so that it can force
     a baker **and** a priority *)
  return ()

let tests = [
  Test.tztest "valid double baking evidence" `Quick valid_double_baking_evidence ;

  (* Should fail*)
  (* Test.tztest "same blocks" `Quick same_blocks ; *)
  Test.tztest "different levels" `Quick different_levels ;
  Test.tztest "too early double baking evidence" `Quick too_early_double_baking_evidence ;
  Test.tztest "too late double baking evidence" `Quick too_late_double_baking_evidence ;
  Test.tztest "different delegates" `Quick different_delegates ;
]
