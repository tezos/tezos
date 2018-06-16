(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_keys

let scheme = "unencrypted"

let title =
  "Built-in signer using raw unencrypted keys."

let description =
  "Please DO NOT USE this signer outside of test environments.\n\
   Valid secret key URIs are of the form\n\
  \ - unencrypted:<key>\n\
   where <key> is the secret key in Base58.\n\
   Valid public key URIs are of the form\n\
  \ - unencrypted:<public_key>\n\
   where <public_key> is the public key in Base58."

let secret_key sk_uri =
  Lwt.return
    (Signature.Secret_key.of_b58check (Uri.path (sk_uri : sk_uri :> Uri.t)))

let make_sk sk =
  Client_keys.make_sk_uri
    (Uri.make ~scheme ~path:(Signature.Secret_key.to_b58check sk) ())

let public_key pk_uri =
  Lwt.return
    (Signature.Public_key.of_b58check (Uri.path (pk_uri : pk_uri :> Uri.t)))

let make_pk pk =
  Client_keys.make_pk_uri
    (Uri.make ~scheme ~path:(Signature.Public_key.to_b58check pk) ())

let neuterize sk_uri =
  secret_key sk_uri >>=? fun sk ->
  return (make_pk (Signature.Secret_key.to_public_key sk))

let public_key_hash pk_uri =
  public_key pk_uri >>=? fun pk ->
  return (Signature.Public_key.hash pk, Some pk)

let sign ?watermark sk_uri buf =
  secret_key sk_uri >>=? fun sk ->
  return (Signature.sign ?watermark sk buf)
