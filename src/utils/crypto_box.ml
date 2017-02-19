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
type target = Z.t
exception TargetNot256Bit

module Public_key_hash = Hash.Make_Blake2B (Base58) (struct
    let name = "Crypto_box.Public_key_hash"
    let title = "A Cryptobox public key ID"
    let b58check_prefix = Base58.Prefix.cryptobox_public_key_hash
    let size = Some 16
  end)

let () =
  Base58.check_encoded_prefix Public_key_hash.b58check_encoding "id" 30

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

let compare_target hash target =
  let hash = Z.of_bits (Hash.Generic_hash.to_string hash) in
  Z.compare hash target <= 0

let make_target f =
  if f < 0. || 256. < f then invalid_arg "Cryptobox.target_of_float" ;
  let frac, shift = modf f in
  let shift = int_of_float shift in
  let m =
    Z.of_int64 @@
    if frac = 0. then
      Int64.(pred (shift_left 1L 54))
    else
      Int64.of_float (2. ** (54. -. frac))
  in
  if shift < 202 then
    Z.logor
      (Z.shift_left m (202 - shift))
      (Z.pred @@ Z.shift_left Z.one (202 - shift))
  else
    Z.shift_right m (shift - 202)

let default_target = make_target 24.

let check_proof_of_work pk nonce target =
  let hash =
    Hash.Generic_hash.hash_bytes [
      Sodium.Box.Bigbytes.of_public_key pk ;
      Sodium.Box.Bigbytes.of_nonce nonce ;
    ] in
  compare_target hash target

let generate_proof_of_work ?max pk target =
  let may_interupt =
    match max with
    | None -> (fun _ -> ())
    | Some max -> (fun cpt -> if max < cpt then raise Not_found) in
  let rec loop nonce cpt =
    may_interupt cpt ;
    if check_proof_of_work pk nonce target then
      nonce
    else
      loop (increment_nonce nonce) (cpt + 1) in
  loop (random_nonce ()) 0

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
