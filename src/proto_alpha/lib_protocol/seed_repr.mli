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

(** Tezos Protocol Implementation - Random number generation

    This is not expected to be a good cryptographic random number
    generator. In particular this is supposed to be used in situations
    where the seed is a globaly known information.

    The only expected property is: It should be difficult to find a
    seed such that the generated sequence is a given one. *)


(** {2 Random Generation} *)

(** The state of the random number generator *)
type t

(** A random seed, to derive random sequences from *)
type seed

(** A random sequence, to derive random values from *)
type sequence

(** [initialize_new state ident] returns a new generator *)
val initialize_new : seed -> MBytes.t list -> t

(** [sequence state n] prepares the n-th sequence of a state  *)
val sequence : t -> int32 -> sequence

(** Generates the next random value in the sequence *)
val take : sequence -> MBytes.t * sequence

(** Generates the next random value as a bounded [int32] *)
val take_int32 : sequence -> int32 -> int32 * sequence

(** {2 Predefined seeds} *)

val empty : seed

(** Returns a new seed by hashing the one passed with a constant. *)
val deterministic_seed : seed -> seed

(** [intial_seeds n] generates the first [n] seeds for which there are no nonces.
    The first seed is a constant value. The kth seed is the hash of seed (k-1)
    concatenated with a constant. *)
val initial_seeds : int -> seed list

(** {2 Entropy} *)

(** A nonce for adding entropy to the generator *)
type nonce

(** Add entropy to the seed generator *)
val nonce : seed -> nonce -> seed

(** Use a byte sequence as a nonce *)
val make_nonce : MBytes.t -> nonce tzresult

(** Compute the has of a nonce *)
val hash : nonce -> Nonce_hash.t

(** [check_hash nonce hash] is true if the nonce correspond to the hash *)
val check_hash : nonce -> Nonce_hash.t -> bool

(** For using nonce hashes as keys in the hierarchical database *)
val nonce_hash_key_part : Nonce_hash.t -> string list -> string list

(** {2 Predefined nonce} *)

val initial_nonce_0 : nonce
val initial_nonce_hash_0 : Nonce_hash.t

(** {2 Serializers} *)

val nonce_encoding  : nonce Data_encoding.t
val seed_encoding : seed Data_encoding.t
