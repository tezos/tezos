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

let scheme = "remote"

module Make(S : sig
    val default : Uri.t
    val authenticate: Signature.Public_key_hash.t list -> MBytes.t -> Signature.t tzresult Lwt.t
    val logger: RPC_client.logger
  end) = struct

  let scheme = scheme

  let title =
    "Built-in tezos-signer using remote wallet."

  let description =
    "Valid locators are of the form\n\
    \ - remote://tz1...\n\
     The key will be queried to current remote signer, which can be \
     configured with the `--remote-signer` or `-R` options, \
     or by defining the following environment variables:\n\
    \ - $TEZOS_SIGNER_UNIX_PATH,\n\
    \ - $TEZOS_SIGNER_TCP_HOST and $TEZOS_SIGNER_TCP_PORT (default: 7732),\n\
    \ - $TEZOS_SIGNER_HTTP_HOST and $TEZOS_SIGNER_HTTP_PORT (default: 6732),\n\
    \ - $TEZOS_SIGNER_HTTPS_HOST and $TEZOS_SIGNER_HTTPS_PORT (default: 443)."

  module Socket = Socket.Make(S)
  module Http = Http.Make(S)
  module Https = Https.Make(S)

  let get_remote () =
    match Uri.scheme S.default with
    | Some "unix" -> (module Socket.Unix : SIGNER)
    | Some "tcp" -> (module Socket.Tcp : SIGNER)
    | Some "http" -> (module Http : SIGNER)
    | Some "https" -> (module Https : SIGNER)
    | _ -> assert false

  module Remote = (val get_remote () : SIGNER)
  let key =
    match Uri.scheme S.default with
    | Some "unix" ->
        (fun uri ->
           let key = Uri.path uri in
           Uri.add_query_param' S.default ("pkh", key))
    | Some "tcp" ->
        (fun uri ->
           let key = Uri.path uri in
           Uri.with_path S.default key)
    | Some ("https" | "http") ->
        (fun uri ->
           let key = Uri.path uri in
           match Uri.path S.default with
           | "" -> Uri.with_path S.default key
           | path -> Uri.with_path S.default (path ^ "/" ^ key))
    | _ -> assert false

  let public_key ?interactive pk_uri =
    Remote.public_key ?interactive
      (Client_keys.make_pk_uri (key (pk_uri : pk_uri :> Uri.t)))

  let public_key_hash ?interactive pk_uri =
    Remote.public_key_hash ?interactive
      (Client_keys.make_pk_uri (key (pk_uri : pk_uri :> Uri.t)))

  let neuterize sk_uri =
    return (Client_keys.make_pk_uri (sk_uri : sk_uri :> Uri.t))

  let sign ?watermark sk_uri msg =
    Remote.sign
      ?watermark
      (Client_keys.make_sk_uri (key (sk_uri : sk_uri :> Uri.t)))
      msg

  let deterministic_nonce sk_uri msg =
    Remote.deterministic_nonce
      (Client_keys.make_sk_uri (key (sk_uri : sk_uri :> Uri.t)))
      msg

  let deterministic_nonce_hash sk_uri msg =
    Remote.deterministic_nonce_hash
      (Client_keys.make_sk_uri (key (sk_uri : sk_uri :> Uri.t)))
      msg

  let supports_deterministic_nonces sk_uri =
    Remote.supports_deterministic_nonces
      (Client_keys.make_sk_uri (key (sk_uri : sk_uri :> Uri.t)))

end

let make_sk sk =
  Client_keys.make_sk_uri
    (Uri.make ~scheme ~path:(Signature.Secret_key.to_b58check sk) ())

let make_pk pk =
  Client_keys.make_pk_uri
    (Uri.make ~scheme ~path:(Signature.Public_key.to_b58check pk) ())

let read_base_uri_from_env () =
  match Sys.getenv_opt "TEZOS_SIGNER_UNIX_PATH",
        Sys.getenv_opt "TEZOS_SIGNER_TCP_HOST",
        Sys.getenv_opt "TEZOS_SIGNER_HTTP_HOST",
        Sys.getenv_opt "TEZOS_SIGNER_HTTPS_HOST" with
  | None, None, None, None -> return_none
  | Some path, None, None, None ->
      return_some (Socket.make_unix_base path)
  | None, Some host, None, None -> begin
      try
        let port =
          match Sys.getenv_opt "TEZOS_SIGNER_TCP_PORT" with
          | None -> 7732
          | Some port -> int_of_string port in
        return_some (Socket.make_tcp_base host port)
      with Invalid_argument _ ->
        failwith "Failed to parse TEZOS_SIGNER_TCP_PORT.@."
    end
  | None, None, Some host, None -> begin
      try
        let port =
          match Sys.getenv_opt "TEZOS_SIGNER_HTTP_PORT" with
          | None -> 6732
          | Some port -> int_of_string port in
        return_some (Http.make_base host port)
      with Invalid_argument _ ->
        failwith "Failed to parse TEZOS_SIGNER_HTTP_PORT.@."
    end
  | None, None, None, Some host -> begin
      try
        let port =
          match Sys.getenv_opt "TEZOS_SIGNER_HTTPS_PORT" with
          | None -> 443
          | Some port -> int_of_string port in
        return_some (Https.make_base host port)
      with Invalid_argument _ ->
        failwith "Failed to parse TEZOS_SIGNER_HTTPS_PORT.@."
    end
  | _, _, _, _ ->
      failwith
        "Only one the following environment variable must be defined: \
         TEZOS_SIGNER_UNIX_PATH, \
         TEZOS_SIGNER_TCP_HOST, \
         TEZOS_SIGNER_HTTP_HOST, \
         TEZOS_SIGNER_HTTPS_HOST@."

type error += Invalid_remote_signer of string

let () =
  register_error_kind
    `Branch
    ~id: "invalid_remote_signer"
    ~title: "Unexpected URI fot remote signer"
    ~description: "The provided remote signer is invalid."
    ~pp:
      (fun ppf s ->
         Format.fprintf ppf
           "@[<v 0>Value '%s' is not a valid URI for a remote signer.@,\
            Supported URIs for remote signers are of the form:@,\
           \ - unix:///path/to/socket/file@,\
           \ - tcp://host:port@,\
           \ - http://host[:port][/prefix]@,\
           \ - https://host[:port][/prefix]@]" s)
    Data_encoding.(obj1 (req "uri" string))
    (function Invalid_remote_signer s -> Some s | _ -> None)
    (fun s -> Invalid_remote_signer s)

let parse_base_uri s =
  trace (Invalid_remote_signer s) @@
  try
    let uri = Uri.of_string s in
    match Uri.scheme uri with
    | Some "http" -> return uri
    | Some "https" -> return uri
    | Some "tcp" -> return uri
    | Some "unix" -> return uri
    | Some scheme -> failwith "Unknown scheme: %s" scheme
    | None -> failwith "Unknown scheme: <empty>"
  with Invalid_argument msg -> failwith "Malformed URI: %s" msg
