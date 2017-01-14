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
type target = int64 list (* used as unsigned intergers... *)
exception TargetNot256Bit

module Public_key_hash = Hash.Make_Blake2B (Base48) (struct
    let name = "Crypto_box.Public_key_hash"
    let title = "A Cryptobox public key ID"
    let b48check_prefix = Base48.Prefix.cryptobox_public_key_hash
    let size = Some 16
  end)

let hash pk =
  Public_key_hash.hash_bytes [Sodium.Box.Bigbytes.of_public_key pk]

let random_keypair () =
  let sk, pk = Sodium.Box.random_keypair () in
  sk, pk, hash pk
let random_nonce = Sodium.Box.random_nonce
let increment_nonce = Sodium.Box.increment_nonce
let box = Sodium.Box.Bigbytes.box
let box_open sk pk msg nonce =
  try Some (Sodium.Box.Bigbytes.box_open sk pk msg nonce) with
    | Sodium.Verification_failure -> None

let precompute = Sodium.Box.precompute
let fast_box = Sodium.Box.Bigbytes.fast_box
let fast_box_open ck msg nonce =
  try Some (Sodium.Box.Bigbytes.fast_box_open ck msg nonce) with
    | Sodium.Verification_failure -> None

let make_target target =
  if List.length target > 8 then raise TargetNot256Bit ;
  target

(* Compare a SHA256 hash to a 256bits-target prefix.
   The prefix is a list of "unsigned" int64. *)
let compare_target hash target =
  let hash = Hash.Generic_hash.to_string hash in
  let rec check offset = function
    | [] -> true
    | x :: xs ->
        Compare.Uint64.(EndianString.BigEndian.get_int64 hash offset <= x)
        && check (offset + 8) xs in
  check 0 target

let default_target =
  (* FIXME we use an easy target until we allow custom configuration. *)
  [ Int64.shift_left 1L 48 ]

let check_proof_of_work pk nonce target =
  let hash =
    Hash.Generic_hash.hash_bytes [
      Sodium.Box.Bigbytes.of_public_key pk ;
      Sodium.Box.Bigbytes.of_nonce nonce ;
    ] in
  compare_target hash target

let generate_proof_of_work pk target =
  let rec loop nonce =
    if check_proof_of_work pk nonce target then nonce
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
