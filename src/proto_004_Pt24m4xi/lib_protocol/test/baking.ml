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
