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

let inject_seed_nonce_revelation cctxt block ?force ?wait nonces =
  let operations =
    List.map
      (fun (level, nonce) ->
         Seed_nonce_revelation { level ; nonce }) nonces in
  Client_node_rpcs.Blocks.net cctxt block >>= fun net ->
  Client_proto_rpcs.Helpers.Forge.Anonymous.operations cctxt
    block ~net operations >>=? fun bytes ->
  Client_node_rpcs.inject_operation cctxt ?force ?wait bytes >>=? fun oph ->
  return oph

type Error_monad.error += Bad_revelation

let forge_seed_nonce_revelation cctxt
    block ?(force = false) redempted_nonces =
  begin
    if force then return redempted_nonces else
    map_filter_s (fun (level, nonce) ->
          Client_proto_rpcs.Context.Nonce.get cctxt block level >>=? function
          | Forgotten ->
              cctxt.message "Too late revelation for level %a"
                Raw_level.pp level >>= fun () ->
              return None
          | Revealed _ ->
              cctxt.message "Ignoring previously-revealed nonce for level %a"
                Raw_level.pp level >>= fun () ->
              return None
          | Missing nonce_hash ->
              if Nonce.check_hash nonce nonce_hash then
                return (Some (level, nonce))
              else
                lwt_log_error "Incoherent nonce for level %a"
                  Raw_level.pp level >>= fun () ->
                return None)
      redempted_nonces
  end >>=? fun nonces ->
  match nonces with
  | [] ->
      cctxt.message "No nonce to reveal" >>= fun () ->
      return ()
  | _ ->
      inject_seed_nonce_revelation cctxt
        block ~force ~wait:true nonces >>=? fun oph ->
      cctxt.answer "Operation successfully injected in the node." >>= fun () ->
      cctxt.answer "Operation hash is '%a'." Operation_hash.pp_short oph >>= fun () ->
      return ()
