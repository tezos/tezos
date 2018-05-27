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
    "Valid locators are of this form: remote://tz1...\n\
     The key will be queried to current remote signer, which can be \
     configured with the `--remote-signer` or `-R` options"

  let get_remote () =
    match Uri.scheme S.default with
    | Some "unix" -> (module Socket.Unix : SIGNER)
    | Some "tcp" -> (module Socket.Tcp : SIGNER)
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
    return (Client_keys.make_pk_uri (sk_uri : sk_uri :> Uri.t))

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
