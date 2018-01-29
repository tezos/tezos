(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Tezos_context

let inject_seed_nonce_revelation rpc_config block ?async nonces =
  let operations =
    List.map
      (fun (level, nonce) ->
         Seed_nonce_revelation { level ; nonce }) nonces in
  let block = Client_rpcs.last_baked_block block in
  Client_node_rpcs.Blocks.info rpc_config block >>=? fun bi ->
  Client_proto_rpcs.Helpers.Forge.Anonymous.operations rpc_config
    block ~branch:bi.hash operations >>=? fun bytes ->
  Client_node_rpcs.inject_operation
    rpc_config ?async ~net_id:bi.net_id
    bytes >>=? fun oph ->
  return oph

let forge_seed_nonce_revelation
    (cctxt: Client_commands.full_context)
    block nonces =
  Client_node_rpcs.Blocks.hash cctxt block >>=? fun hash ->
  match nonces with
  | [] ->
      cctxt#message "No nonce to reveal for block %a"
        Block_hash.pp_short hash >>= fun () ->
      return ()
  | _ ->
      inject_seed_nonce_revelation cctxt block nonces >>=? fun oph ->
      cctxt#answer
        "Operation successfully injected %d revelation(s) for %a."
        (List.length nonces)
        Block_hash.pp_short hash >>= fun () ->
      cctxt#answer "Operation hash is '%a'."
        Operation_hash.pp_short oph >>= fun () ->
      return ()
