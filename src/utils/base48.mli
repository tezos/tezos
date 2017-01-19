(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** {1 Prefixed Base48Check encodings} *)

(** Like Bitcoin's Base58Check, all the data encoded in Tezos are
    prefixed with a constant which depends on the kind of encoded
    data.

    The [Prefix] exports all the prefix used by the Tezos Shell. Each
    version of the economical protocol might complete this list.

    Unlike Bitcoin's Base58Check, the prefix in the unencoded-data
    is visible in the encoded data.

*)
module Prefix : sig

  val block_hash: string
    (** Prefix for block hashes: "\000".
        (in Base48: "e" "f" or "g") *)

  val operation_hash: string
    (** Prefix for operation hashes: "\001".
        (in Base48: "E" "F" or "G") *)

  val protocol_hash: string
    (** Prefix for protocol-version hashes: "\002".
        (in Base48: "2" "3" or "4") *)

  val ed25519_public_key_hash: string
    (** Prefix for Ed25519 public key hashes: "\003". *)

  val cryptobox_public_key_hash: string
    (** Prefix for Ed25519 public key hashes: "\004". *)

  val ed25519_public_key: string
    (** Prefix for Ed25519 public key: "\012". *)

  val ed25519_secret_key: string
    (** Prefix for Ed25519 secret key: "\013". *)

  val ed25519_signature: string
    (** Prefix for Ed25519 signature key: "\014". *)

  val protocol_prefix: string
    (** Prefix for all the encodings defined by economical protocol:
       "\015". *)

end

(** An extensible sum-type for decoded data: one case per known
    "prefix". See for instance [Hash.Block_hash.Hash] or
    [Environment.Ed25519.Public_key_hash]. *)
type data = ..

(** Abstract representation of registred encodings. The type paramater
    is the type of the encoded data, for instance [Hash.Block_hash.t]. *)
type 'a encoding = private {
  prefix: string;
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
  to_raw: ('a -> string) ->
  of_raw: (string -> 'a option) ->
  wrap: ('a -> data) ->
  'a encoding

(** Encoder for a given kind of data. *)
val simple_encode: ?alphabet:string -> 'a encoding -> 'a -> string

(** Decoder for a given kind of data. It returns [None] when
    the decoded data does not start with the expected prefix. *)
val simple_decode: ?alphabet:string -> 'a encoding -> string -> 'a option

(** Generic decoder. It returns [None] when the decoded data does
    not start with a registred prefix. *)
val decode: ?alphabet:string -> string -> data option

(** {2 Completion of partial Base48Check value} *)

(** Register a (global) resolver for a previsously
    registred kind af data. *)
val register_resolver: 'a encoding -> (string -> 'a list Lwt.t) -> unit

(** Try to complete a prefix of a Base48Check encoded data, by using
    the previously registered resolver associated to this kind of
    data. Note that a prefix of [n] characters of a Base48-encoded
    value provides at least [n/2] bytes of a prefix of the original value. *)
val complete: ?alphabet:string -> string -> string list Lwt.t

(** {1 Low-level: distinct registering function for economical protocol} *)

(** See [src/proto/environment/base48.mli]} for an inlined
    documentation. *)
module Make(C: sig type context end) : sig

  val register_encoding:
    prefix: string ->
    to_raw: ('a -> string) ->
    of_raw: (string -> 'a option) ->
    wrap: ('a -> data) ->
    'a encoding

  val decode: ?alphabet:string -> string -> data option

  val register_resolver:
    'a encoding -> (C.context -> string -> 'a list Lwt.t) -> unit

  val complete:
    ?alphabet:string -> C.context -> string -> string list Lwt.t

end

(** {2 Low-level Base48Check encodings} *)

(** Base48Check-encoding/decoding functions (with error detections). *)
val safe_encode: ?alphabet:string -> string -> string
val safe_decode: ?alphabet:string -> string -> string

(** Base48-encoding/decoding functions (without error detections). *)
val raw_encode: ?alphabet:string -> string -> string
val raw_decode: ?alphabet:string -> string -> string

