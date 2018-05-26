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

type secret_key = Signature.Secret_key.t
type public_key = Signature.Public_key.t

let init _wallet = return ()

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

let sk_of_locator = function
  |(Sk_locator { location = ( location :: _ ) }) ->
      Lwt.return (Signature.Secret_key.of_b58check location)
  |(Sk_locator { location = _ }) ->
      failwith "Wrong type of location"


let pk_of_locator = function
  |(Pk_locator { location = ( location :: _ ) }) ->
      Lwt.return (Signature.Public_key.of_b58check location)
  |(Pk_locator { location = _ }) ->
      failwith "Wrong type of location"

let sk_to_locator sk =
  Secret_key_locator.create
    ~scheme ~location:[Signature.Secret_key.to_b58check sk] |>
  Lwt.return

let pk_to_locator pk =
  Public_key_locator.create
    ~scheme ~location:[Signature.Public_key.to_b58check pk] |>
  Lwt.return

let neuterize x = Lwt.return (Signature.Secret_key.to_public_key x)
let public_key x = return x
let public_key_hash x = return (Signature.Public_key.hash x)
let sign ?watermark t buf = return (Signature.sign ?watermark t buf)
