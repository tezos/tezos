(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Protocol Implementation - Random number generation

    This is not expected to be a good cryptographic random number
    generator. In particular this is supposed to be used in situations
    where the seed is a globaly known information.

    The only expected property is: It should be difficult to find a
    seed such that the generated sequence is a given one. *)


(** {2 Random Generation} ****************************************************)

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

(** {2 Predefined seeds} *****************************************************)

val empty : seed
val initial_seed_0 : seed
val initial_seed_1 : seed
val initial_seed_2 : seed

(** {2 Entropy} **************************************************************)

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

(** {2 Predefined nonce} *****************************************************)

val initial_nonce_0 : nonce
val initial_nonce_hash_0 : Nonce_hash.t

(** {2 Serializers} **********************************************************)

val nonce_encoding  : nonce Data_encoding.t
val seed_encoding : seed Data_encoding.t
