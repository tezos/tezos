(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let log = Signer_logging.lwt_log_notice

let run (cctxt : #Client_context.wallet) ~host ~port ~cert ~key ?magic_bytes ~require_auth =
  log "Accepting HTTPS requests on port %d" port >>= fun () ->
  let mode : Conduit_lwt_unix.server =
    `TLS (`Crt_file_path cert, `Key_file_path key, `No_password, `Port port) in
  let dir = RPC_directory.empty in
  let dir =
    RPC_directory.register1 dir Signer_services.sign begin fun pkh signature data ->
      Handler.sign cctxt { pkh ; data ; signature } ?magic_bytes ~require_auth
    end in
  let dir =
    RPC_directory.register1 dir Signer_services.public_key begin fun pkh () () ->
      Handler.public_key cctxt pkh
    end in
  let dir =
    RPC_directory.register0 dir Signer_services.authorized_keys begin fun () () ->
      if require_auth then
        return None
      else
        Handler.Authorized_key.load cctxt >>=? fun keys ->
        return (Some (keys |> List.split |> snd |> List.map Signature.Public_key.hash))
    end in
  Lwt.catch
    (fun () ->
       RPC_server.launch ~host mode dir
         ~media_types:Media_type.all_media_types
         ~cors: { allowed_origins = [ "*" ] ;
                  allowed_headers = [ "Content-Type" ] }
       >>= fun _server ->
       fst (Lwt.wait ()))
    (function
      | Unix.Unix_error(Unix.EADDRINUSE, "bind","") ->
          failwith "Port already in use."
      | exn -> Lwt.return (error_exn exn))
