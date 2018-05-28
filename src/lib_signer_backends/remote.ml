(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_keys

let scheme = "remote"

module Make(S : sig val default : Uri.t end) = struct

  let scheme = scheme

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

  let get_remote () =
    match Uri.scheme S.default with
    | Some "unix" -> (module Socket.Unix : SIGNER)
    | Some "tcp" -> (module Socket.Tcp : SIGNER)
    | Some "https" -> (module Https : SIGNER)
    | _ -> assert false

  module Remote = (val get_remote () : SIGNER)
  let key =
    match Uri.scheme S.default with
    | Some "unix" | Some "tcp" ->
        (fun uri ->
           let key = Uri.path uri in
           Uri.add_query_param S.default ("key", [key]))
    | Some "https" ->
        (fun uri ->
           let key = Uri.path uri in
           match Uri.path S.default with
           | "" -> Uri.with_path S.default key
           | path -> Uri.with_path S.default (path ^ "/" ^ key))
    | _ -> assert false

  let public_key pk_uri =
    Remote.public_key
      (Client_keys.make_pk_uri (key (pk_uri : pk_uri :> Uri.t)))

  let public_key_hash pk_uri =
    Remote.public_key_hash
      (Client_keys.make_pk_uri (key (pk_uri : pk_uri :> Uri.t)))

  let neuterize sk_uri =
    Remote.neuterize
      (Client_keys.make_sk_uri (key (sk_uri : sk_uri :> Uri.t)))

  let sign ?watermark sk_uri msg =
    Remote.sign
      ?watermark
      (Client_keys.make_sk_uri (key (sk_uri : sk_uri :> Uri.t)))
      msg

end

let make_sk sk =
  Client_keys.make_sk_uri
    (Uri.make ~scheme ~path:(Signature.Secret_key.to_b58check sk) ())

let make_pk pk =
  Client_keys.make_pk_uri
    (Uri.make ~scheme ~path:(Signature.Public_key.to_b58check pk) ())
