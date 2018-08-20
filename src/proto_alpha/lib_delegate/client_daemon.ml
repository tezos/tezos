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

module Endorser = struct

  let run (cctxt : #Proto_alpha.full) ~delay ?min_date delegates =
    Client_baking_blocks.monitor_heads
      ~next_protocols:(Some [Proto_alpha.hash])
      cctxt `Main >>=? fun block_stream ->
    Client_baking_endorsement.create cctxt ~delay delegates block_stream >>=? fun () ->
    ignore min_date;
    return_unit

end

module Baker = struct

  let run (cctxt : #Proto_alpha.full) ?fee_threshold ?max_priority ?min_date ~context_path delegates =
    Client_baking_blocks.monitor_heads
      ~next_protocols:(Some [Proto_alpha.hash])
      cctxt `Main >>=? fun block_stream ->
    Client_baking_forge.create cctxt
      ?fee_threshold ?max_priority ~context_path delegates block_stream >>=? fun () ->
    ignore min_date;
    return_unit

end

module Accuser = struct

  let run (cctxt : #Proto_alpha.full) ~preserved_levels =
    Client_baking_blocks.monitor_valid_blocks
      ~next_protocols:(Some [Proto_alpha.hash])
      cctxt ~chains:[ `Main ] () >>=? fun valid_blocks_stream ->
    Client_baking_denunciation.create cctxt ~preserved_levels valid_blocks_stream >>=? fun () ->
    return_unit

end
