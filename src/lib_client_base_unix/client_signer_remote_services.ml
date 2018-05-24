(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_signer_remote_messages

let sign = RPC_service.post_service
    ~description: "Sign a piece of data with a given remote key"
    ~query: RPC_query.empty
    ~input: Sign.Request.encoding
    ~output: Sign.Response.encoding
    RPC_path.(root / "sign")

let public_key = RPC_service.post_service
    ~description: "Retrieve the public key of a given remote key"
    ~query: RPC_query.empty
    ~input: Public_key.Request.encoding
    ~output: Public_key.Response.encoding
    RPC_path.(root / "public_key")

type path = string * string

let read_env path =
  if path <> "" && String.get path 0 = '$' then
    try
      return (Sys.getenv (String.sub path 1 (String.length path - 1)))
    with
      Not_found ->
        match path with
        | "$TEZOS_SIGNER_HTTPS_HOST" -> return "localhost"
        | "$TEZOS_SIGNER_HTTPS_PORT" -> return "5732"
        | _ ->
            failwith "Remote signer location uses environment variable %s which is not bound" path
  else return path

let catch_unix_error msg f =
  Lwt.catch f @@ function
  | Unix.Unix_error (err, syscall, _) ->
      failwith "%s\nUnix error (%s): %s" msg syscall (Unix.error_message err)
  | Failure err -> failwith "%s\n%s" msg err
  | exn -> Lwt.fail exn

let base (host, port) =
  read_env host >>=? fun host ->
  read_env port >>=? fun port ->
  catch_unix_error "Cannot parse port" @@ fun () ->
  return (host, int_of_string port)

let call (host, port) service arg =
  read_env host >>=? fun host ->
  read_env port >>=? fun port ->
  catch_unix_error "Cannot call remote service" @@ fun () ->
  let port = int_of_string port in
  RPC_client.call_service
    Media_type.all_media_types
    ~base: (Uri.of_string (Format.asprintf "https://%s:%d" host port))
    service () () arg
