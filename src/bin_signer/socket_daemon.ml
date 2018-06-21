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

let run (cctxt : #Client_context.wallet) path ?magic_bytes ~require_auth =
  Lwt_utils_unix.Socket.bind path >>=? fun fd ->
  let rec loop () =
    Lwt_unix.accept fd >>= fun (fd, _) ->
    Lwt.async begin fun () ->
      Lwt_utils_unix.Socket.recv fd Request.encoding >>=? function
      | Sign req ->
          let encoding = result_encoding Sign.Response.encoding in
          Handler.sign cctxt req ?magic_bytes ~require_auth >>= fun res ->
          Lwt_utils_unix.Socket.send fd encoding res >>= fun _ ->
          Lwt_unix.close fd >>= fun () ->
          return ()
      | Public_key pkh ->
          let encoding = result_encoding Public_key.Response.encoding in
          Handler.public_key cctxt pkh >>= fun res ->
          Lwt_utils_unix.Socket.send fd encoding res >>= fun _ ->
          Lwt_unix.close fd >>= fun () ->
          return ()
      | Authorized_keys ->
          let encoding = result_encoding Authorized_keys.Response.encoding in
          begin if require_auth then
              Handler.Authorized_key.load cctxt >>=? fun keys ->
              return (Authorized_keys.Response.Authorized_keys
                        (keys |> List.split |> snd |> List.map Signature.Public_key.hash))
            else return Authorized_keys.Response.No_authentication
          end >>= fun res ->
          Lwt_utils_unix.Socket.send fd encoding res >>= fun _ ->
          Lwt_unix.close fd >>= fun () ->
          return ()
    end ;
    loop ()
  in
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
