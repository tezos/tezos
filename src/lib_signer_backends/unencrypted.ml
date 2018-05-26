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
  "Do not use this signer except for playing on the test chain.\n\
   The format for importing secret keys is either no argument (will \
   generate a key) or the raw Base58-encoded key (starting with \
   'edsk').\n\
   The format for importing public keys is the raw Base58-encoded \
   key (starting with 'edpk')."

(*
let sk_locator_of_human_input _cctxt = function
  | sk :: _ ->
      return (Secret_key_locator.create ~scheme ~location:[sk])
  | [] ->
      let _, _, sk = Ed25519.generate_key () in
      return (Secret_key_locator.create ~scheme
                ~location:[Ed25519.Secret_key.to_b58check sk])

let pk_locator_of_human_input _cctxt = function
  | [] -> failwith "Missing public key argument"
  | pk :: _ -> return (Public_key_locator.create ~scheme ~location:[pk])

*)
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
  return (Signature.Public_key.hash pk)

let sign ?watermark sk_uri buf =
  secret_key sk_uri >>=? fun sk ->
  return (Signature.sign ?watermark sk buf)
