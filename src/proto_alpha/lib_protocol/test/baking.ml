(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha

(** Tests for [bake_n] and [bake_until_end_cycle]. *)
let test_cycle () =
  Context.init 5 >>=? fun (b,_) ->
  Context.get_constants (B b) >>=? fun csts ->
  let blocks_per_cycle = csts.parametric.blocks_per_cycle in

  let pp = fun fmt x -> Format.fprintf fmt "%ld" x in

  (* Tests that [bake_n n] bakes [n] blocks. *)
  Block.bake_n 10 b >>=? fun b ->
  Context.get_level (B b) >>=? fun curr_level ->
  Assert.equal ~loc:__LOC__ Int32.equal "not the right level" pp
    (Alpha_context.Raw_level.to_int32 curr_level)
    10l >>=? fun () ->

  (* Tests that [bake_until_cycle_end] returns a block at
     level [blocks_per_cycle]. *)
  Block.bake_until_cycle_end b >>=? fun b ->
  Context.get_level (B b) >>=? fun curr_level ->
  Assert.equal ~loc:__LOC__ Int32.equal "not the right level" pp
    (Alpha_context.Raw_level.to_int32 curr_level)
    blocks_per_cycle

let tests = [
  Test.tztest "cycle" `Quick (test_cycle) ;
]
