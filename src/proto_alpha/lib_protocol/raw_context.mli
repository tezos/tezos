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

(** {1 Errors} *)

type error += Too_many_internal_operations (* `Permanent *)

(** An internal storage error that should not happen *)
type storage_error =
  | Incompatible_protocol_version of string
  | Missing_key of string list * [`Get | `Set | `Del | `Copy]
  | Existing_key of string list
  | Corrupted_data of string list

type error += Storage_error of storage_error
type error += Failed_to_parse_parameter of MBytes.t
type error += Failed_to_decode_parameter of Data_encoding.json * string

val storage_error: storage_error -> 'a tzresult Lwt.t

(** {1 Abstract Context} *)

(** Abstract view of the context.
    Includes a handle to the functional key-value database
    ({!Context.t}) along with some in-memory values (gas, etc.). *)
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
  Context.t -> context tzresult Lwt.t

type 'a previous_protocol =
  | Genesis of 'a
  | Alpha_previous

val prepare_first_block:
  level:int32 ->
  timestamp:Time.t ->
  fitness:Fitness.t ->
  Context.t -> (Parameters_repr.t previous_protocol * context) tzresult Lwt.t

val activate: context -> Protocol_hash.t -> t Lwt.t
val fork_test_chain: context -> Protocol_hash.t -> Time.t -> t Lwt.t

val register_resolvers:
  'a Base58.encoding -> (context -> string -> 'a list Lwt.t) -> unit

(** Returns the state of the database resulting of operations on its
    abstract view *)
val recover: context -> Context.t

val current_level: context -> Level_repr.t
val current_timestamp: context -> Time.t

val current_fitness: context -> Int64.t
val set_current_fitness: context -> Int64.t -> t

val constants: context -> Constants_repr.parametric
val patch_constants:
  context ->
  (Constants_repr.parametric -> Constants_repr.parametric) ->
  context Lwt.t
val first_level: context -> Raw_level_repr.t

(** Increment the current block fee stash that will be credited to baker's
    frozen_fees account at finalize_application *)
val add_fees: context -> Tez_repr.t -> context tzresult Lwt.t

(** Increment the current block reward stash that will be credited to baker's
    frozen_fees account at finalize_application *)
val add_rewards: context -> Tez_repr.t -> context tzresult Lwt.t

(** Increment the current block deposit stash for a specific delegate. All the
    delegates' frozen_deposit accounts are credited at finalize_application *)
val add_deposit:
  context -> Signature.Public_key_hash.t -> Tez_repr.t -> context tzresult Lwt.t

val get_fees: context -> Tez_repr.t
val get_rewards: context -> Tez_repr.t
val get_deposits: context -> Tez_repr.t Signature.Public_key_hash.Map.t

type error += Gas_limit_too_high (* `Permanent *)

val check_gas_limit: t -> Z.t -> unit tzresult
val set_gas_limit: t -> Z.t -> t
val set_gas_unlimited: t -> t
val gas_level: t -> Gas_limit_repr.t
val gas_consumed: since: t -> until: t -> Z.t
val block_gas_level: t -> Z.t

val init_storage_space_to_pay: t -> t
val update_storage_space_to_pay: t -> Z.t -> t
val update_allocated_contracts_count: t -> t
val clear_storage_space_to_pay: t -> t * Z.t * int

type error += Undefined_operation_nonce (* `Permanent *)

val init_origination_nonce: t -> Operation_hash.t -> t
val origination_nonce: t -> Contract_repr.origination_nonce tzresult
val increment_origination_nonce: t -> (t * Contract_repr.origination_nonce) tzresult
val unset_origination_nonce: t -> t

(** {1 Generic accessors} *)

type key = string list

type value = MBytes.t

(** All context manipulation functions. This signature is included
    as-is for direct context accesses, and used in {!Storage_functors}
    to provide restricted views to the context. *)
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

  val copy: context -> from:key -> to_:key -> context tzresult Lwt.t

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

  (** Internally used in {!Storage_functors} to escape from a view. *)
  val project: context -> root_context

  (** Internally used in {!Storage_functors} to retrieve a full key
      from partial key relative a view. *)
  val absolute_key: context -> key -> key

  (** Internally used in {!Storage_functors} to consume gas from
      within a view. *)
  val consume_gas: context -> Gas_limit_repr.cost -> context tzresult

  (** Check if consume_gas will fail *)
  val check_enough_gas: context -> Gas_limit_repr.cost -> unit tzresult

  val description: context Storage_description.t

end

include T with type t := t and type context := context

(** Initialize the local nonce used for preventing a script to
    duplicate an internal operation to replay it. *)
val reset_internal_nonce: context -> context

(** Increments the internal operation nonce. *)
val fresh_internal_nonce: context -> (context * int) tzresult

(** Mark an internal operation nonce as taken. *)
val record_internal_nonce: context -> int -> context

(** Check is the internal operation nonce has been taken. *)
val internal_nonce_already_recorded: context -> int -> bool

(** Returns a map where to each endorser's pkh is associated the list of its
    endorsing slots (in decreasing order) for a given level. *)
val allowed_endorsements:
  context ->
  (Signature.Public_key.t * int list * bool) Signature.Public_key_hash.Map.t

(** Initializes the map of allowed endorsements, this function must only be
    called once. *)
val init_endorsements:
  context ->
  (Signature.Public_key.t * int list * bool) Signature.Public_key_hash.Map.t ->
  context

(** Marks an endorsment in the map as used. *)
val record_endorsement:
  context -> Signature.Public_key_hash.t -> context
