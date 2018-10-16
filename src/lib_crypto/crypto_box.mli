(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Tezos - X25519/XSalsa20-Poly1305 cryptography *)

type nonce = Bigstring.t
val nonce_size : int

val zero_nonce : nonce
val random_nonce : unit -> nonce
val increment_nonce : ?step:int -> nonce -> nonce

(** [generate_nonces ~incoming ~sent_msg ~recv_msg] generates two
    nonces by hashing (Blake2B) the arguments. The nonces should be
    used to initialize the encryption on the communication
    channels. Because an attacker cannot control both messages,
    it cannot determine the nonces that will be used to encrypt
    the messages. The sent message should contains a random nonce,
    and we should never send the exact same message twice. *)
val generate_nonces :
  incoming:bool -> sent_msg:MBytes.t -> recv_msg:MBytes.t -> nonce * nonce

module Secretbox : sig
  type key

  val unsafe_of_bytes : MBytes.t -> key

  val box_noalloc : key -> nonce -> MBytes.t -> unit
  val box_open_noalloc : key -> nonce -> MBytes.t -> bool

  val box : key -> MBytes.t -> nonce -> MBytes.t
  val box_open : key -> MBytes.t -> nonce -> MBytes.t option
end

type target
val default_target : target
val make_target : float -> target

type secret_key
type public_key
module Public_key_hash : S.HASH
type channel_key

val hash : public_key -> Public_key_hash.t

val zerobytes : int
val boxzerobytes : int

val random_keypair : unit -> secret_key * public_key * Public_key_hash.t

val precompute : secret_key -> public_key -> channel_key

val fast_box        : channel_key -> MBytes.t -> nonce -> MBytes.t
val fast_box_open   : channel_key -> MBytes.t -> nonce -> MBytes.t option

val fast_box_noalloc : channel_key -> nonce -> MBytes.t -> unit
val fast_box_open_noalloc : channel_key -> nonce -> MBytes.t -> bool

val check_proof_of_work : public_key -> nonce -> target -> bool
val generate_proof_of_work : ?max:int -> public_key -> target -> nonce

val public_key_to_bigarray : public_key -> Cstruct.buffer
val public_key_of_bigarray : Cstruct.buffer -> public_key
val public_key_size : int

val secret_key_to_bigarray : secret_key -> Cstruct.buffer
val secret_key_of_bigarray : Cstruct.buffer -> secret_key
val secret_key_size : int

val public_key_encoding : public_key Data_encoding.t
val secret_key_encoding : secret_key Data_encoding.t
val nonce_encoding : nonce Data_encoding.t

val neuterize : secret_key -> public_key
val equal : public_key -> public_key -> bool

val pp_pk :Format.formatter -> public_key -> unit
