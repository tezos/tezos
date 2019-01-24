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

include Tezos_stdlib.Logging.Make_semantic(struct let name = "client.nonce_revelation" end)

open Proto_alpha

let inject_seed_nonce_revelation rpc_config ?(chain = `Main) block ?async nonces =
  Alpha_block_services.hash rpc_config ~chain ~block () >>=? fun branch ->
  map_p
    (fun (level, nonce) ->
       Alpha_services.Forge.seed_nonce_revelation rpc_config
         (chain, block) ~branch ~level ~nonce () >>=? fun bytes ->
       let bytes = Signature.concat bytes Signature.zero in
       Shell_services.Injection.operation rpc_config ?async ~chain bytes >>=? fun oph ->
       lwt_debug Tag.DSL.(fun f ->
           f "Revealing nonce %a from level %a at chain %a, block %a with operation %a"
           -% t event "reveal_nonce"
           -% a Logging.nonce_tag nonce
           -% a Logging.level_tag level
           -% a Logging.chain_tag chain
           -% a Logging.block_tag block
           -% a Operation_hash.Logging.tag oph) >>= fun () ->
       return oph)
    nonces >>=? fun ophs ->
  return ophs

let forge_seed_nonce_revelation
    (cctxt: #Proto_alpha.full)
    ?(chain = `Main)
    block nonces =
  Shell_services.Blocks.hash cctxt ~chain ~block () >>=? fun hash ->
  match nonces with
  | [] ->
      lwt_log_notice Tag.DSL.(fun f ->
          f "Nothing to reveal for block %a"
          -% t event "no_nonce_reveal"
          -% a Block_hash.Logging.tag hash
        ) >>= fun () ->
      return_unit
  | _ ->
      inject_seed_nonce_revelation cctxt ~chain block nonces >>=? fun oph ->
      cctxt#answer
        "Operation successfully injected %d revelation(s) for %a."
        (List.length nonces)
        Block_hash.pp_short hash >>= fun () ->
      cctxt#answer "@[<v 2>Operation hash are:@ %a@]"
        (Format.pp_print_list Operation_hash.pp_short) oph >>= fun () ->
      return_unit
