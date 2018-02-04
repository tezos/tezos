(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos - X25519/XSalsa20-Poly1305 cryptography *)

type nonce

val random_nonce : unit -> nonce
val increment_nonce : ?step:int -> nonce -> nonce
val nonce_encoding : nonce Data_encoding.t

type target
val default_target : target
val make_target : float -> target

type secret_key
type public_key
module Public_key_hash : S.INTERNAL_HASH
type channel_key

val public_key_encoding : public_key Data_encoding.t
val secret_key_encoding : secret_key Data_encoding.t

val hash : public_key -> Public_key_hash.t

val zerobytes : int
val boxzerobytes : int

val random_keypair : unit -> secret_key * public_key * Public_key_hash.t

val box : secret_key -> public_key -> MBytes.t -> nonce -> MBytes.t
val box_open : secret_key -> public_key -> MBytes.t -> nonce -> MBytes.t option

val box_noalloc : secret_key -> public_key -> nonce -> MBytes.t -> unit
val box_open_noalloc : secret_key -> public_key -> nonce -> MBytes.t -> bool

val precompute : secret_key -> public_key -> channel_key

val fast_box        : channel_key -> MBytes.t -> nonce -> MBytes.t
val fast_box_open   : channel_key -> MBytes.t -> nonce -> MBytes.t option

val fast_box_noalloc : channel_key -> nonce -> MBytes.t -> unit
val fast_box_open_noalloc : channel_key -> nonce -> MBytes.t -> bool

val check_proof_of_work : public_key -> nonce -> target -> bool
val generate_proof_of_work : ?max:int -> public_key -> target -> nonce


