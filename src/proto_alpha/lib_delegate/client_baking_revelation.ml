(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha

let inject_seed_nonce_revelation rpc_config ?(chain = `Main) block ?async nonces =
  Alpha_block_services.hash rpc_config ~chain ~block () >>=? fun branch ->
  map_p
    (fun (level, nonce) ->
       Alpha_services.Forge.seed_nonce_revelation rpc_config
         (chain, block) ~branch ~level ~nonce () >>=? fun bytes ->
       let bytes = Signature.concat bytes Signature.zero in
       Shell_services.Injection.operation rpc_config ?async ~chain bytes)
    nonces >>=? fun ophs ->
  return ophs

let forge_seed_nonce_revelation
    (cctxt: #Proto_alpha.full)
    ?(chain = `Main)
    block nonces =
  Shell_services.Blocks.hash cctxt ~chain ~block () >>=? fun hash ->
  match nonces with
  | [] ->
      cctxt#message "No nonce to reveal for block %a"
        Block_hash.pp_short hash >>= fun () ->
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
