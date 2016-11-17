(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Utils

(** Tezos - X25519/XSalsa20-Poly1305 cryptography *)

type secret_key = Sodium.Box.secret_key
type public_key = Sodium.Box.public_key
type channel_key = Sodium.Box.channel_key
type nonce = Sodium.Box.nonce
type difficulty = int64

let random_keypair = Sodium.Box.random_keypair
let random_nonce = Sodium.Box.random_nonce
let increment_nonce = Sodium.Box.increment_nonce
let box = Sodium.Box.Bigbytes.box
let box_open sk pk msg nonce =
  try Some (Sodium.Box.Bigbytes.box_open sk pk msg nonce) with
    | Sodium.Verification_failure -> None

let check_proof_of_work pk nonce difficulty =
  let hash_bytes l =
    let hash = Cryptokit.Hash.sha256 () in
    List.iter (fun b -> hash#add_string (MBytes.to_string b)) l;
    let r = hash#result in hash#wipe; r in
  let hash =
    hash_bytes
      [ Sodium.Box.Bigbytes.of_public_key pk ;
        Sodium.Box.Bigbytes.of_nonce nonce ] in
  let bytes = MBytes.of_string hash in
  let last_int64 =
    EndianBigstring.BigEndian.get_int64 bytes (MBytes.length bytes - 8) in
  Int64.logand last_int64 (Int64.of_int 1) < difficulty
let generate_proof_of_work pk difficulty =
  let rec loop nonce =
    if check_proof_of_work pk nonce difficulty then nonce
    else loop (increment_nonce nonce) in
  loop (random_nonce ())

let public_key_encoding =
  let open Data_encoding in
    conv
      Sodium.Box.Bigbytes.of_public_key
      Sodium.Box.Bigbytes.to_public_key
      (Fixed.bytes Sodium.Box.public_key_size)

let secret_key_encoding =
  let open Data_encoding in
    conv
      Sodium.Box.Bigbytes.of_secret_key
      Sodium.Box.Bigbytes.to_secret_key
      (Fixed.bytes Sodium.Box.secret_key_size)

let nonce_encoding =
  let open Data_encoding in
    conv
      Sodium.Box.Bigbytes.of_nonce
      Sodium.Box.Bigbytes.to_nonce
      (Fixed.bytes Sodium.Box.nonce_size)
