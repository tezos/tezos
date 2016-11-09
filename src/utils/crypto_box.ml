(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos - X25519/XSalsa20-Poly1305 cryptography *)

type secret_key = Sodium.Box.secret_key
type public_key = Sodium.Box.public_key
type channel_key = Sodium.Box.channel_key
type nonce = Sodium.Box.nonce
type difficulty = int

let random_keypair = Sodium.Box.random_keypair
let random_nonce = Sodium.Box.random_nonce
let increment_nonce = Sodium.Box.increment_nonce
let box = Sodium.Box.Bigbytes.box
let box_open sk pk msg nonce =
  try Some (Sodium.Box.Bigbytes.box_open sk pk msg nonce) with
    | Sodium.Verification_failure -> None
let to_secret_key = Sodium.Box.Bigbytes.to_secret_key
let of_secret_key = Sodium.Box.Bigbytes.of_secret_key
let to_public_key = Sodium.Box.Bigbytes.to_public_key
let of_public_key = Sodium.Box.Bigbytes.of_public_key
let to_nonce = Sodium.Box.Bigbytes.to_nonce
let of_nonce = Sodium.Box.Bigbytes.of_nonce
let check_proof_of_work pk nonce difficulty = assert false
let generate_proof_of_work pk difficulty =
  let rec loop nonce =
    if check_proof_of_work pk nonce difficulty then nonce
    else loop (increment_nonce nonce) in
  loop (random_nonce ())
