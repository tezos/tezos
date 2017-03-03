(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos - Versioned, block indexed (key x value) store *)

(** A block-indexed (key x value) store directory.  *)
type index

(** A (key x value) store for a given block. *)
type t
type context = t

(** Open or initialize a versioned store at a given path. *)
val init:
  ?patch_context:(context -> context Lwt.t) ->
  root:string ->
  index Lwt.t

val commit_genesis:
  index ->
  id:Block_hash.t ->
  time:Time.t ->
  protocol:Protocol_hash.t ->
  test_protocol:Protocol_hash.t ->
  context Lwt.t

(** {2 Generic interface} ****************************************************)

include Persist.STORE with type t := context

(** {2 Accessing and Updating Versions} **************************************)

exception Preexistent_context of Block_hash.t
val exists: index -> Block_hash.t -> bool Lwt.t
val checkout: index -> Block_hash.t -> context option Lwt.t
val checkout_exn: index -> Block_hash.t -> context Lwt.t
val commit: Block_hash.t -> context -> unit Lwt.t

(** {2 Predefined Fields} ****************************************************)

val get_protocol: context -> Protocol_hash.t Lwt.t
val set_protocol: context -> Protocol_hash.t -> context Lwt.t

val get_test_protocol: context -> Protocol_hash.t Lwt.t
val set_test_protocol: context -> Protocol_hash.t -> context Lwt.t

val get_test_network: context -> Store.Net_id.t option Lwt.t
val set_test_network: context -> Store.Net_id.t -> context Lwt.t
val del_test_network: context -> context Lwt.t

val get_test_network_expiration: context -> Time.t option Lwt.t
val set_test_network_expiration: context -> Time.t -> context Lwt.t
val del_test_network_expiration: context -> context Lwt.t

val read_and_reset_fork_test_network: context -> (bool * context) Lwt.t
val fork_test_network: context -> context Lwt.t

val set_fitness: context -> Fitness.fitness -> context Lwt.t
val get_fitness: context -> Fitness.fitness Lwt.t

val set_timestamp: context -> Time.t -> context Lwt.t
val get_timestamp: context -> Time.t Lwt.t

val set_commit_message: context -> string -> context Lwt.t

val init_test_network:
  context ->  time:Time.t -> genesis:Block_hash.t -> context tzresult Lwt.t
