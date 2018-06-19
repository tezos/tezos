(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_keys
open Signer_messages

let tcp_scheme = "tcp"
let unix_scheme = "unix"

module Make(P : sig
    val authenticate: Signature.Public_key_hash.t list -> MBytes.t -> Signature.t tzresult Lwt.t
  end) = struct

  let sign ?watermark path pkh msg =
    let msg =
      match watermark with
      | None -> msg
      | Some watermark ->
          MBytes.concat "" [ Signature.bytes_of_watermark watermark ; msg ] in
    Lwt_utils_unix.Socket.connect path >>=? fun conn ->
    Lwt_utils_unix.Socket.send
      conn Request.encoding Request.Authorized_keys >>=? fun () ->
    Lwt_utils_unix.Socket.recv conn
      Authorized_keys.Response.encoding >>=? fun authorized_keys ->
    begin match authorized_keys with
      | No_authentication -> return None
      | Authorized_keys authorized_keys ->
          P.authenticate authorized_keys
            (Sign.Request.to_sign ~pkh ~data:msg) >>=? fun signature ->
          return (Some signature)
    end >>=? fun signature ->
    let req = { Sign.Request.pkh ; data = msg ; signature } in
    Lwt_utils_unix.Socket.send
      conn Request.encoding (Request.Sign req) >>=? fun () ->
    Lwt_utils_unix.Socket.recv conn
      (result_encoding Sign.Response.encoding) >>=? fun res ->
    Lwt_unix.close conn >>= fun () ->
    Lwt.return res

  let public_key path pkh =
    Lwt_utils_unix.Socket.connect path >>=? fun conn ->
    Lwt_utils_unix.Socket.send
      conn Request.encoding (Request.Public_key pkh) >>=? fun () ->
    let encoding = result_encoding Public_key.Response.encoding in
    Lwt_utils_unix.Socket.recv conn encoding >>=? fun res ->
    Lwt_unix.close conn >>= fun () ->
    Lwt.return res

  module Unix = struct

    let scheme = unix_scheme

    let title =
      "Built-in tezos-signer using remote signer through hardcoded unix socket."

    let description =
      "Valid locators are of the form\n\
      \ - unix:///path/to/socket?pkh=tz1..."

    let parse uri =
      assert (Uri.scheme uri = Some scheme) ;
      trace (Invalid_uri uri) @@
      match Uri.get_query_param uri "pkh" with
      | None -> failwith "Missing the query parameter: 'pkh=tz1...'"
      | Some key ->
          Lwt.return (Signature.Public_key_hash.of_b58check key) >>=? fun key ->
          return (Lwt_utils_unix.Socket.Unix (Uri.path uri), key)

    let public_key uri =
      parse (uri : pk_uri :> Uri.t) >>=? fun (path, pkh) ->
      public_key path pkh

    let neuterize uri =
      return (Client_keys.make_pk_uri (uri : sk_uri :> Uri.t))

    let public_key_hash uri =
      public_key uri >>=? fun pk ->
      return (Signature.Public_key.hash pk, Some pk)

    let sign ?watermark uri msg =
      parse (uri : sk_uri :> Uri.t) >>=? fun (path, pkh) ->
      sign ?watermark path pkh msg

  end

  module Tcp = struct

    let scheme = tcp_scheme

    let title =
      "Built-in tezos-signer using remote signer through hardcoded tcp socket."

    let description =
      "Valid locators are of the form\n\
      \ - tcp://host:port/tz1..."

    let parse uri =
      assert (Uri.scheme uri = Some scheme) ;
      trace (Invalid_uri uri) @@
      match Uri.host uri, Uri.port uri with
      | None, _ ->
          failwith "Missing host address"
      | _, None ->
          failwith "Missing host port"
      | Some path, Some port ->
          Lwt.return
            (Signature.Public_key_hash.of_b58check (Uri.path uri)) >>=? fun pkh ->
          return (Lwt_utils_unix.Socket.Tcp (path, port), pkh)

    let public_key uri =
      parse (uri : pk_uri :> Uri.t) >>=? fun (path, pkh) ->
      public_key path pkh

    let neuterize uri =
      return (Client_keys.make_pk_uri (uri : sk_uri :> Uri.t))

    let public_key_hash uri =
      public_key uri >>=? fun pk ->
      return (Signature.Public_key.hash pk, Some pk)

    let sign ?watermark uri msg =
      parse (uri : sk_uri :> Uri.t) >>=? fun (path, pkh) ->
      sign ?watermark path pkh msg

  end

end


let make_unix_base path =
  Uri.make ~scheme:unix_scheme ~path ()

let make_tcp_base host port =
  Uri.make ~scheme:tcp_scheme ~host ~port ()
