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

let inject_seed_nonce_revelation rpc_config ?(chain = `Main) block ?async nonces =
  let operations =
    List.map
      (fun (level, nonce) ->
         Seed_nonce_revelation { level ; nonce }) nonces in
  Block_services.hash rpc_config ~chain ~block () >>=? fun branch ->
  Alpha_services.Forge.Anonymous.operations rpc_config
    (chain, block) ~branch operations >>=? fun bytes ->
  Injection_services.operation rpc_config ?async ~chain bytes >>=? fun oph ->
  return oph

let forge_seed_nonce_revelation
    (cctxt: #Proto_alpha.full)
    ?(chain = `Main)
    block nonces =
  Block_services.hash cctxt ~chain ~block () >>=? fun hash ->
  match nonces with
  | [] ->
      cctxt#message "No nonce to reveal for block %a"
        Block_hash.pp_short hash >>= fun () ->
      return ()
  | _ ->
      inject_seed_nonce_revelation cctxt ~chain block nonces >>=? fun oph ->
      cctxt#answer
        "Operation successfully injected %d revelation(s) for %a."
        (List.length nonces)
        Block_hash.pp_short hash >>= fun () ->
      cctxt#answer "Operation hash is '%a'."
        Operation_hash.pp_short oph >>= fun () ->
      return ()
