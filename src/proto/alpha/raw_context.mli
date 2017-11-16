(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** {1 Errors} ****************************************************************)

(** An internal storage error that should not happen *)
type storage_error =
  | Incompatible_protocol_version of string
  | Missing_key of string list * [`Get | `Set | `Del]
  | Existing_key of string list
  | Corrupted_data of string list

type error += Storage_error of storage_error
type error += Failed_to_parse_sandbox_parameter of MBytes.t

val storage_error: storage_error -> 'a tzresult Lwt.t

(** {1 Abstract Context} **************************************************)

(** Abstract view of the context *)
type t
type context = t
type root_context = t

(** Retrieves the state of the database and gives its abstract view.
    It also returns wether this is the first block validated
    with this version of the protocol. *)
val prepare:
  level: Int32.t ->
  timestamp: Time.t ->
  fitness: Fitness.t ->
  Context.t -> (context * bool) tzresult Lwt.t

val activate: context -> Protocol_hash.t -> t Lwt.t
val fork_test_network: context -> Protocol_hash.t -> Time.t -> t Lwt.t

val register_resolvers:
  'a Base58.encoding -> (context -> string -> 'a list Lwt.t) -> unit

val configure_sandbox:
  Context.t -> Data_encoding.json option -> Context.t tzresult Lwt.t

(** Returns the state of the database resulting of operations on its
    abstract view *)
val recover: context -> Context.t

val current_level: context -> Level_repr.t
val current_timestamp: context -> Time.t

val current_fitness: context -> Int64.t
val set_current_fitness: context -> Int64.t -> t

val constants: context -> Constants_repr.constants
val first_level: context -> Raw_level_repr.t

(** {1 Generic accessors} *************************************************)

type key = string list

type value = MBytes.t

module type T = sig

  type t
  type context = t

  (** Tells if the key is already defined as a value. *)
  val mem: context -> key -> bool Lwt.t

  (** Tells if the key is already defined as a directory. *)
  val dir_mem: context -> key -> bool Lwt.t

  (** Retrieve the value from the storage bucket ; returns a
      {!Storage_error Missing_key} if the key is not set. *)
  val get: context -> key -> value tzresult Lwt.t

  (** Retrieves the value from the storage bucket ; returns [None] if
      the data is not initialized. *)
  val get_option: context -> key -> value option Lwt.t

  (** Allocates the storage bucket and initializes it ; returns a
      {!Storage_error Existing_key} if the bucket exists. *)
  val init: context -> key -> value -> context tzresult Lwt.t

  (** Updates the content of the bucket ; returns a {!Storage_error
      Missing_key} if the value does not exists. *)
  val set: context -> key -> value -> context tzresult Lwt.t

  (** Allocates the data and initializes it with a value ; just
      updates it if the bucket exists. *)
  val init_set: context -> key -> value -> context Lwt.t

  (** When the value is [Some v], allocates the data and initializes
      it with [v] ; just updates it if the bucket exists. When the
      valus is [None], delete the storage bucket when the value ; does
      nothing if the bucket does not exists. *)
  val set_option: context -> key -> value option -> context Lwt.t

  (** Delete the storage bucket ; returns a {!Storage_error
      Missing_key} if the bucket does not exists. *)
  val delete: context -> key -> context tzresult Lwt.t

  (** Removes the storage bucket and its contents ; does nothing if the
      bucket does not exists. *)
  val remove: context -> key -> context Lwt.t

  (** Recursively removes all the storage buckets and contents ; does
      nothing if no bucket exists. *)
  val remove_rec: context -> key -> context Lwt.t

  (** Iterator on all the items of a given directory. *)
  val fold:
    context -> key -> init:'a ->
    f:([ `Key of key | `Dir of key ] -> 'a -> 'a Lwt.t) ->
    'a Lwt.t

  (** Recursively list all subkeys of a given key. *)
  val keys: context -> key -> key list Lwt.t

  (** Recursive iterator on all the subkeys of a given key. *)
  val fold_keys:
    context -> key -> init:'a -> f:(key -> 'a -> 'a Lwt.t) -> 'a Lwt.t

  val project: context -> root_context

end

include T with type t := t and type context := context
