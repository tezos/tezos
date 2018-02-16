(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** {1 Prefixed Base58Check encodings} *)

module Prefix : sig

  val block_hash: string
  val operation_hash: string
  val operation_list_hash: string
  val operation_list_list_hash: string
  val protocol_hash: string
  val context_hash: string
  val ed25519_public_key_hash: string
  val cryptobox_public_key_hash: string
  val ed25519_seed: string
  val ed25519_public_key: string
  val ed25519_secret_key: string
  val ed25519_signature: string
  val chain_id: string

end

(** An extensible sum-type for decoded data: one case per known
    "prefix". See for instance [Hash.Block_hash.Hash] or
    [Environment.Ed25519.Public_key_hash]. *)
type data = ..

(** Abstract representation of registred encodings. The type paramater
    is the type of the encoded data, for instance [Hash.Block_hash.t]. *)
type 'a encoding = private {
  prefix: string ;
  length: int ;
  encoded_prefix: string ;
  encoded_length: int ;
  to_raw: 'a -> string ;
  of_raw: string -> 'a option ;
  wrap: 'a -> data ;
}

(** Register a new encoding. The function might raise `Invalid_arg` if
    the provided [prefix] overlap with a previously registred
    prefix. The [to_raw] and [of_raw] are the ad-hoc
    serialisation/deserialisation for the data. The [wrap] should wrap
    the deserialised value into the extensible sum-type [data] (see
    the generic function [decode]). *)
val register_encoding:
  prefix: string ->
  length: int ->
  to_raw: ('a -> string) ->
  of_raw: (string -> 'a option) ->
  wrap: ('a -> data) ->
  'a encoding

val check_encoded_prefix: 'a encoding -> string -> int -> unit

module Alphabet : sig
  type t
  val bitcoin: t
  val ripple: t
  val flickr: t
  val make: string -> t
  val all_in_alphabet :  t -> string -> bool
  val pp : Format.formatter -> t -> unit
end

(** Encoder for a given kind of data. *)
val simple_encode: ?alphabet:Alphabet.t -> 'a encoding -> 'a -> string

(** Decoder for a given kind of data. It returns [None] when
    the decoded data does not start with the expected prefix. *)
val simple_decode: ?alphabet:Alphabet.t -> 'a encoding -> string -> 'a option

(** Generic decoder. It returns [None] when the decoded data does
    not start with a registred prefix. *)
val decode: ?alphabet:Alphabet.t -> string -> data option

(** {2 Completion of partial Base58Check value} *)

(** Register a (global) resolver for a previsously
    registred kind af data. *)
val register_resolver: 'a encoding -> (string -> 'a list Lwt.t) -> unit

(** Try to complete a prefix of a Base58Check encoded data, by using
    the previously registered resolver associated to this kind of
    data. Note that a prefix of [n] characters of a Base58-encoded
    value provides at least [n/2] bytes of a prefix of the original value. *)
val complete: ?alphabet:Alphabet.t -> string -> string list Lwt.t

(** {1 Low-level: distinct registering function for economic protocol} *)

(** See [src/environment/v1/base58.mli] for an inlined
    documentation. *)
module Make(C: sig type context end) : sig

  val register_encoding:
    prefix: string ->
    length: int ->
    to_raw: ('a -> string) ->
    of_raw: (string -> 'a option) ->
    wrap: ('a -> data) ->
    'a encoding

  val decode: ?alphabet:Alphabet.t -> string -> data option

  val register_resolver:
    'a encoding -> (C.context -> string -> 'a list Lwt.t) -> unit

  val complete:
    ?alphabet:Alphabet.t -> C.context -> string -> string list Lwt.t

end

(** {2 Low-level Base58Check encodings} *)

(** Base58Check-encoding/decoding functions (with error detections). *)
val safe_encode: ?alphabet:Alphabet.t -> string -> string
val safe_decode: ?alphabet:Alphabet.t -> string -> string option

(** Base58-encoding/decoding functions (without error detections). *)
val raw_encode: ?alphabet:Alphabet.t -> string -> string
val raw_decode: ?alphabet:Alphabet.t -> string -> string option

(**/**)

val partial_decode: ?alphabet:Alphabet.t -> string -> int -> string option
val make_encoded_prefix: string -> int -> string * int
