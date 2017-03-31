(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Cli_entries
open Tezos_context
open Logging.Client.Revelation

let inject_seed_nonce_revelation cctxt block ?force ?async nonces =
  let operations =
    List.map
      (fun (level, nonce) ->
         Seed_nonce_revelation { level ; nonce }) nonces in
  Client_node_rpcs.Blocks.net cctxt block >>= fun net ->
  Client_proto_rpcs.Helpers.Forge.Anonymous.operations cctxt
    block ~net operations >>=? fun bytes ->
  Client_node_rpcs.inject_operation cctxt ?force ?async bytes >>=? fun oph ->
  return oph

type Error_monad.error += Bad_revelation

let forge_seed_nonce_revelation
    (cctxt: Client_commands.context)
    block ?(force = false) nonces =
  Client_node_rpcs.Blocks.hash cctxt block >>= fun hash ->
  match nonces with
  | [] ->
      cctxt.message "No nonce to reveal for block %a"
        Block_hash.pp_short hash >>= fun () ->
      return ()
  | _ ->
      inject_seed_nonce_revelation cctxt block ~force nonces >>=? fun oph ->
      cctxt.answer
        "Operation successfully injected %d revelation(s) for %a."
        (List.length nonces)
        Block_hash.pp_short hash >>= fun () ->
      cctxt.answer "Operation hash is '%a'."
        Operation_hash.pp_short oph >>= fun () ->
      return ()
