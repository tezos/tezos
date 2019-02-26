(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Client_keys
open Signer_messages

let tcp_scheme = "tcp"
let unix_scheme = "unix"

module Make(P : sig
    val authenticate: Signature.Public_key_hash.t list -> MBytes.t -> Signature.t tzresult Lwt.t
  end) = struct

  type request_type =
    | Sign_request
    | Deterministic_nonce_request
    | Deterministic_nonce_hash_request

  let build_request pkh data signature = function
    | Sign_request ->
        Request.Sign { Sign.Request.pkh ; data ; signature }
    | Deterministic_nonce_request ->
        Request.Deterministic_nonce
          { Deterministic_nonce.Request.pkh ; data ; signature }
    | Deterministic_nonce_hash_request ->
        Request.Deterministic_nonce_hash
          { Deterministic_nonce_hash.Request.pkh ; data ; signature }

  let signer_operation path pkh msg request_type =
    begin
      Lwt_utils_unix.Socket.connect path >>=? fun conn ->
      Lwt_utils_unix.Socket.send
        conn Request.encoding Request.Authorized_keys >>=? fun () ->
      Lwt_utils_unix.Socket.recv conn
        (result_encoding Authorized_keys.Response.encoding) >>=? fun authorized_keys ->
      Lwt.return authorized_keys >>=? fun authorized_keys ->
      Lwt_unix.close conn >>= fun () ->
      begin match authorized_keys with
        | No_authentication -> return_none
        | Authorized_keys authorized_keys ->
            P.authenticate authorized_keys
              (Sign.Request.to_sign ~pkh ~data:msg) >>=? fun signature ->
            return_some signature
      end
    end >>=? fun signature ->
    Lwt_utils_unix.Socket.connect path >>=? fun conn ->
    let req = build_request pkh msg signature request_type in
    Lwt_utils_unix.Socket.send conn Request.encoding req >>=? fun () ->
    return conn

  let sign ?watermark path pkh msg =
    let msg =
      match watermark with
      | None -> msg
      | Some watermark ->
          MBytes.concat "" [ Signature.bytes_of_watermark watermark ; msg ] in
    signer_operation path pkh msg Sign_request >>=? fun conn ->
    Lwt_utils_unix.Socket.recv conn
      (result_encoding Sign.Response.encoding) >>=? fun res ->
    Lwt_unix.close conn >>= fun () ->
    Lwt.return res

  let deterministic_nonce path pkh msg =
    signer_operation path pkh msg Deterministic_nonce_request >>=? fun conn ->
    Lwt_utils_unix.Socket.recv conn
      (result_encoding Deterministic_nonce.Response.encoding) >>=? fun res ->
    Lwt_unix.close conn >>= fun () ->
    Lwt.return res

  let deterministic_nonce_hash path pkh msg =
    signer_operation path pkh msg Deterministic_nonce_hash_request >>=? fun conn ->
    Lwt_utils_unix.Socket.recv conn
      (result_encoding Deterministic_nonce_hash.Response.encoding) >>=? fun res ->
    Lwt_unix.close conn >>= fun () ->
    Lwt.return res

  let supports_deterministic_nonces path pkh =
    Lwt_utils_unix.Socket.connect path >>=? fun conn ->
    Lwt_utils_unix.Socket.send
      conn Request.encoding (Request.Supports_deterministic_nonces pkh) >>=? fun () ->
    Lwt_utils_unix.Socket.recv conn
      (result_encoding Supports_deterministic_nonces.Response.encoding) >>=? fun res ->
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
      \ - unix:/path/to/socket?pkh=tz1..."

    let parse uri =
      assert (Uri.scheme uri = Some scheme) ;
      trace (Invalid_uri uri) @@
      match Uri.get_query_param uri "pkh" with
      | None -> failwith "Missing the query parameter: 'pkh=tz1...'"
      | Some key ->
          Lwt.return (Signature.Public_key_hash.of_b58check key) >>=? fun key ->
          return (Lwt_utils_unix.Socket.Unix (Uri.path uri), key)

    let public_key ?interactive:(_) uri =
      parse (uri : pk_uri :> Uri.t) >>=? fun (path, pkh) ->
      public_key path pkh

    let neuterize uri =
      return (Client_keys.make_pk_uri (uri : sk_uri :> Uri.t))

    let public_key_hash ?interactive:(_) uri =
      public_key uri >>=? fun pk ->
      return (Signature.Public_key.hash pk, Some pk)

    let sign ?watermark uri msg =
      parse (uri : sk_uri :> Uri.t) >>=? fun (path, pkh) ->
      sign ?watermark path pkh msg

    let deterministic_nonce uri msg =
      parse (uri : sk_uri :> Uri.t) >>=? fun (path, pkh) ->
      deterministic_nonce path pkh msg

    let deterministic_nonce_hash uri msg =
      parse (uri : sk_uri :> Uri.t) >>=? fun (path, pkh) ->
      deterministic_nonce_hash path pkh msg

    let supports_deterministic_nonces uri =
      parse (uri : sk_uri :> Uri.t) >>=? fun (path, pkh) ->
      supports_deterministic_nonces path pkh

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
          let pkh = Uri.path uri in
          let pkh =
            try String.(sub pkh 1 (length pkh - 1))
            with _ -> "" in
          Lwt.return
            (Signature.Public_key_hash.of_b58check pkh) >>=? fun pkh ->
          return (Lwt_utils_unix.Socket.Tcp (path, string_of_int port,
                                             [Lwt_unix.AI_SOCKTYPE SOCK_STREAM]), pkh)

    let public_key ?interactive:(_) uri =
      parse (uri : pk_uri :> Uri.t) >>=? fun (path, pkh) ->
      public_key path pkh

    let neuterize uri =
      return (Client_keys.make_pk_uri (uri : sk_uri :> Uri.t))

    let public_key_hash ?interactive uri =
      public_key ?interactive uri >>=? fun pk ->
      return (Signature.Public_key.hash pk, Some pk)

    let sign ?watermark uri msg =
      parse (uri : sk_uri :> Uri.t) >>=? fun (path, pkh) ->
      sign ?watermark path pkh msg

    let deterministic_nonce uri msg =
      parse (uri : sk_uri :> Uri.t) >>=? fun (path, pkh) ->
      deterministic_nonce path pkh msg

    let deterministic_nonce_hash uri msg =
      parse (uri : sk_uri :> Uri.t) >>=? fun (path, pkh) ->
      deterministic_nonce_hash path pkh msg

    let supports_deterministic_nonces uri =
      parse (uri : sk_uri :> Uri.t) >>=? fun (path, pkh) ->
      supports_deterministic_nonces path pkh

  end

end


let make_unix_base path =
  Uri.make ~scheme:unix_scheme ~path ()

let make_tcp_base host port =
  Uri.make ~scheme:tcp_scheme ~host ~port ()
