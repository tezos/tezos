(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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
val compare_target : MBytes.t list -> target -> bool
val validate_target : target -> unit
val default_target : target

type secret_key
type public_key

val public_key_encoding : public_key Data_encoding.t
val secret_key_encoding : secret_key Data_encoding.t

val random_keypair : unit -> secret_key * public_key

val box : secret_key -> public_key -> MBytes.t -> nonce -> MBytes.t

val box_open : secret_key -> public_key -> MBytes.t -> nonce -> MBytes.t option

val check_proof_of_work : public_key -> nonce -> target -> bool
val generate_proof_of_work : public_key -> target -> nonce
