(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos - X25519/XSalsa20-Poly1305 cryptography *)

type secret_key
type public_key
type nonce

val random_keypair : unit -> secret_key * public_key
val random_nonce : unit -> nonce
val increment_nonce : ?step:int -> nonce -> nonce
val box : secret_key -> public_key -> MBytes.t -> nonce -> MBytes.t
val box_open : secret_key -> public_key -> MBytes.t -> nonce -> MBytes.t
val to_secret_key : MBytes.t -> secret_key
val of_secret_key : secret_key -> MBytes.t
val to_public_key : MBytes.t -> public_key
val of_public_key : public_key -> MBytes.t
val to_nonce : MBytes.t -> nonce
val of_nonce : nonce -> MBytes.t
