(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_keys
open Client_signer_remote_messages

type path =
  | Socket of Lwt_utils_unix.Socket.addr
  | Https of Client_signer_remote_services.path

let socket_sign path key data =
  let req = { Sign.Request.key = key ; data } in
  Lwt_utils_unix.Socket.connect path >>=? fun conn ->
  Lwt_utils_unix.Socket.send conn Request.encoding (Request.Sign req) >>=? fun () ->
  let encoding = result_encoding Sign.Response.encoding in
  Lwt_utils_unix.Socket.recv conn encoding >>=? function
  | Error err -> Lwt.return (Error err)
  | Ok res -> Lwt_unix.close conn >>= fun () -> return res.signature

let socket_request_public_key path key =
  let req = { Public_key.Request.key = key } in
  Lwt_utils_unix.Socket.connect path >>=? fun conn ->
  Lwt_utils_unix.Socket.send conn Request.encoding (Request.Public_key req) >>=? fun () ->
  let encoding = result_encoding Public_key.Response.encoding in
  Lwt_utils_unix.Socket.recv conn encoding >>=? function
  | Error err -> Lwt.return (Error err)
  | Ok res -> Lwt_unix.close conn >>= fun () -> return res.public_key

let sign path key data = match path with
  | Socket path -> socket_sign path key data
  | Https path ->
      Client_signer_remote_services.(call path sign) { key ; data } >>=? fun res ->
      return res.signature

let request_public_key path key = match path with
  | Socket path -> socket_request_public_key path key
  | Https path ->
      Client_signer_remote_services.(call path public_key) { key } >>=? fun res ->
      return res.public_key

module Remote_signer : SIGNER = struct
  let scheme = "remote"

  let title =
    "Built-in tezos-signer using remote wallet."

  let description =
    "Valid locators are one of these two forms:\n\
    \  - unix [path to local signer socket] <remote key alias>\n\
    \  - tcp [host] [port] <remote key alias>\n\
    \  - https [host] [port] <remote key alias>\n\
     All fields except the key can be of the form '$VAR', \
     in which case their value is taken from environment variable \
     VAR each time the key is accessed.\n\
     Not specifiyng fields sets them to $TEZOS_SIGNER_UNIX_PATH, \
     $TEZOS_SIGNER_TCP_HOST and $TEZOS_SIGNER_TCP_PORT, \
     $TEZOS_SIGNER_HTTPS_HOST and $TEZOS_SIGNER_HTTPS_PORT, \
     that get evaluated to default values '$HOME/.tezos-signer-socket', \
     localhost and 6732, and can be set later on."

  type key_path = path * key

  (* secret key is the identifier of the location key identifier *)
  type secret_key = key_path
  (* public key is the identifier of the location key identifier *)
  type public_key = key_path * Signature.Public_key.t

  let pks : (secret_key, Signature.Public_key.t) Hashtbl.t = Hashtbl.create 53

  (* load and init the remote wallet. initialize the connection *)
  let init _cctxt = return ()

  let path_of_human_input = function
    | "unix" :: key :: [] ->
        return (Socket (Unix "$TEZOS_SIGNER_UNIX_PATH"), key)
    | "unix" :: file :: key :: [] ->
        return (Socket (Unix file), key)
    | "tcp" :: host :: port :: key :: [] ->
        return (Socket (Tcp (host, int_of_string port)), key)
    (* Temporary FIXME *)
    (* | "tcp" :: host :: key :: [] -> *)
    (* return (Socket (Tcp (host, "$TEZOS_SIGNER_TCP_PORT")), key) *)
    (* | "tcp" :: key :: [] -> *)
    (* return (Socket (Tcp ("$TEZOS_SIGNER_TCP_HOST", "$TEZOS_SIGNER_TCP_PORT")), key) *)
    | "https" :: host :: port :: key :: [] ->
        return (Https (host, port), key)
    | "https" :: host :: key :: [] ->
        return (Https (host, "$TEZOS_SIGNER_HTTPS_PORT"), key)
    | "https" :: key :: [] ->
        return (Https ("$TEZOS_SIGNER_HTTPS_HOST", "$TEZOS_SIGNER_HTTPS_PORT"), key)
    | location ->
        failwith
          "@[<v 2>Remote Schema : wrong locator %s.@,@[<hov 0>%a@]@]"
          (Secret_key_locator.to_string (Secret_key_locator.create ~scheme ~location))
          Format.pp_print_text description

  let locator_of_path = function
    | Socket (Unix path), key -> [ "unix" ; path ; key ]
    | Socket (Tcp (host, port)), key -> [ "tcp" ; host ; string_of_int port ; key ]
    | Https (host, port), key -> [ "https" ; host ; port ; key ]

  let pk_locator_of_human_input _cctxt path =
    path_of_human_input path >>=? fun pk ->
    let location = locator_of_path pk in
    return (Public_key_locator.create ~scheme ~location)

  let sk_to_locator sk =
    let location = locator_of_path sk in
    Lwt.return (Secret_key_locator.create ~scheme ~location)

  let sk_locator_of_human_input _cctxt input =
    path_of_human_input input >>=? fun (path, key) ->
    request_public_key path key >>=? fun pk ->
    Hashtbl.replace pks (path, key) pk ;
    sk_to_locator (path,key) >>= fun locator ->
    return locator

  let sk_of_locator loc =
    path_of_human_input (Secret_key_locator.location loc)

  let pk_of_locator loc =
    path_of_human_input (Public_key_locator.location loc) >>=? fun (path, key) ->
    request_public_key path key >>=? fun pk ->
    Hashtbl.replace pks (path, key) pk ;
    return ((path, key), pk)

  let pk_to_locator (path, _) =
    let location = locator_of_path path in
    Lwt.return (Public_key_locator.create ~scheme ~location)

  let neuterize ((path, key) as sk) =
    match Hashtbl.find_opt pks sk with
    | Some pk -> Lwt.return (sk, pk)
    | None -> begin
        request_public_key path key >>= function
        | Error _ -> Lwt.fail_with "Remote : Cannot obtain public key from remote signer"
        | Ok pk -> begin
            Hashtbl.replace pks sk pk ;
            Lwt.return (sk, pk)
          end
      end

  let public_key (_, x) = return x
  let public_key_hash (_, x) = return (Signature.Public_key.hash x)

  let sign ?watermark (path, key) msg =
    let msg =
      match watermark with
      | None -> msg
      | Some watermark ->
          MBytes.concat "" [ Signature.bytes_of_watermark watermark ; msg ] in
    sign path key msg

end

let () =
  register_signer (module Remote_signer)
