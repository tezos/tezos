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
(* target ought to be an unsigned 256 bit integer
but this representation works better with ocplib-endian; make
sure target has length 16! *)
type target = int list
exception TargetNot256Bit

let random_keypair = Sodium.Box.random_keypair
let random_nonce = Sodium.Box.random_nonce
let increment_nonce = Sodium.Box.increment_nonce
let box = Sodium.Box.Bigbytes.box
let box_open sk pk msg nonce =
  try Some (Sodium.Box.Bigbytes.box_open sk pk msg nonce) with
    | Sodium.Verification_failure -> None

let validate_target target =
  if List.length target <> 16 then raise TargetNot256Bit;
  if List.for_all (fun t -> t < 0 || t >= 1 lsl 16) target
    then raise TargetNot256Bit

(* compare a SHA256 hash to a 256 bit target *)
let compare_target xs target =
  let hash =
    let hash = Cryptokit.Hash.sha256 () in
    List.iter (fun b -> hash#add_string (MBytes.to_string b)) xs;
    let r = hash#result in hash#wipe; r in
  let bytes = MBytes.of_string hash in
  let get_16 = EndianBigstring.BigEndian.get_uint16 bytes in
  let offsets = [0;2;4;6;8;10;12;14;16;18;20;22;24;26;28;30] in
  List.for_all2 (fun o t -> get_16 o < t) offsets target

let check_proof_of_work pk nonce target =
  let what_to_hash =
    [ Sodium.Box.Bigbytes.of_public_key pk
    ; Sodium.Box.Bigbytes.of_nonce nonce ] in
  compare_target what_to_hash target

let generate_proof_of_work pk target =
  validate_target target;
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
