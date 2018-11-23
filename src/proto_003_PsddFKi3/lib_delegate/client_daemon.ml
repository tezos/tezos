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

let await_bootstrapped_node (cctxt: #Proto_alpha.full) =
  (* Waiting for the node to be synchronized *)
  cctxt#message "Waiting for the node to be synchronized with its \
                 peers..." >>= fun () ->
  Shell_services.Monitor.bootstrapped cctxt >>=? fun _ ->
  cctxt#message "Node synchronized." >>= fun () ->
  return_unit

module Endorser = struct

  let run (cctxt : #Proto_alpha.full) ~delay delegates =
    await_bootstrapped_node cctxt >>=? fun _ ->
    Client_baking_blocks.monitor_heads
      ~next_protocols:(Some [Proto_alpha.hash])
      cctxt `Main >>=? fun block_stream ->
    cctxt#message "Endorser started." >>= fun () ->
    Client_baking_endorsement.create cctxt ~delay delegates block_stream >>=? fun () ->
    return_unit

end

module Baker = struct

  let run
      (cctxt : #Proto_alpha.full)
      ?minimal_fees
      ?minimal_picotez_per_gas_unit
      ?minimal_picotez_per_byte
      ?await_endorsements
      ?max_priority
      ~context_path
      delegates =
    await_bootstrapped_node cctxt >>=? fun _ ->
    Client_baking_blocks.monitor_heads
      ~next_protocols:(Some [Proto_alpha.hash])
      cctxt `Main >>=? fun block_stream ->
    cctxt#message "Baker started." >>= fun () ->
    Client_baking_forge.create cctxt
      ?minimal_fees
      ?minimal_picotez_per_gas_unit
      ?minimal_picotez_per_byte
      ?await_endorsements
      ?max_priority
      ~context_path delegates block_stream >>=? fun () ->
    return_unit

end

module Accuser = struct

  let run (cctxt : #Proto_alpha.full) ~preserved_levels =
    await_bootstrapped_node cctxt >>=? fun _ ->
    Client_baking_blocks.monitor_valid_blocks
      ~next_protocols:(Some [Proto_alpha.hash])
      cctxt ~chains:[ `Main ] () >>=? fun valid_blocks_stream ->
    cctxt#message "Accuser started." >>= fun () ->
    Client_baking_denunciation.create cctxt ~preserved_levels valid_blocks_stream >>=? fun () ->
    return_unit

end
