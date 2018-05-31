(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Signer_messages

let log = Signer_logging.lwt_log_notice

let run (cctxt : #Client_context.wallet) path =
  Lwt_utils_unix.Socket.bind path >>=? fun fd ->
  let rec loop () =
    Lwt_unix.accept fd >>= fun (fd, _) ->
    Lwt.async begin fun () ->
      Lwt_utils_unix.Socket.recv fd Request.encoding >>=? function
      | Sign req ->
          let encoding = result_encoding Sign.Response.encoding in
          Handler.sign cctxt req.pkh req.data >>= fun res ->
          Lwt_utils_unix.Socket.send fd encoding res >>= fun _ ->
          Lwt_unix.close fd >>= fun () ->
          return ()
      | Public_key pkh ->
          let encoding = result_encoding Public_key.Response.encoding in
          Handler.public_key cctxt pkh >>= fun res ->
          Lwt_utils_unix.Socket.send fd encoding res >>= fun _ ->
          Lwt_unix.close fd >>= fun () ->
          return ()
    end ;
    loop ()
  in
  Lwt_unix.listen fd 10 ;
  begin
    match path with
    | Tcp (host, port) ->
        log "Accepting TCP requests on port %s:%d" host port
    | Unix path ->
        Sys.set_signal Sys.sigint (Signal_handle begin fun _ ->
            Format.printf "Removing the local socket file and quitting.@." ;
            Unix.unlink path ;
            exit 0
          end) ;
        log "Accepting UNIX requests on %s" path
  end >>= fun () ->
  loop ()
