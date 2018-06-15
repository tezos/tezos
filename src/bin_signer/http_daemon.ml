(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let log = Signer_logging.lwt_log_notice

let run (cctxt : #Client_context.wallet) ~hosts ?magic_bytes ~require_auth mode =
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
        Handler.Authorized_key.load cctxt >>=? fun keys ->
        return (Some (keys |> List.split |> snd |> List.map Signature.Public_key.hash))
      else
        return None
    end in
  Lwt.catch
    (fun () ->
       List.map
         (fun host ->
            let host = Ipaddr.V6.to_string host in
            log "Listening on address %s" host >>= fun () ->
            RPC_server.launch ~host mode dir
              ~media_types:Media_type.all_media_types
            >>= fun _server ->
            fst (Lwt.wait ()))
         hosts |> Lwt.choose)
    (function
      | Unix.Unix_error(Unix.EADDRINUSE, "bind","") ->
          failwith "Port already in use."
      | exn -> Lwt.return (error_exn exn))

let run_https (cctxt : #Client_context.wallet) ~host ~port ~cert ~key ?magic_bytes ~require_auth =
  Lwt_utils_unix.getaddrinfo ~passive:true ~node:host ~service:(string_of_int port) >>= function
  | []->
      failwith "Cannot resolve listening address: %S" host
  | points ->
      let hosts = fst (List.split points) in
      log "Accepting HTTPS requests on port %d" port >>= fun () ->
      let mode : Conduit_lwt_unix.server =
        `TLS (`Crt_file_path cert, `Key_file_path key, `No_password, `Port port) in
      run (cctxt : #Client_context.wallet) ~hosts ?magic_bytes ~require_auth mode

let run_http (cctxt : #Client_context.wallet) ~host ~port ?magic_bytes ~require_auth =
  Lwt_utils_unix.getaddrinfo ~passive:true ~node:host ~service:(string_of_int port) >>= function
  | [] ->
      failwith "Cannot resolve listening address: %S" host
  | points ->
      let hosts = fst (List.split points) in
      log "Accepting HTTP requests on port %d" port >>= fun () ->
      let mode : Conduit_lwt_unix.server =
        `TCP (`Port port) in
      run (cctxt : #Client_context.wallet) ~hosts ?magic_bytes ~require_auth mode
