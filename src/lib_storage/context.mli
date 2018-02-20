(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
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
  chain_id:Chain_id.t ->
  time:Time.t ->
  protocol:Protocol_hash.t ->
  Context_hash.t Lwt.t

val commit_test_chain_genesis:
  index -> Block_hash.t -> Time.t -> context ->
  (Chain_id.t * Block_hash.t * Context_hash.t) tzresult Lwt.t

(** {2 Generic interface} ****************************************************)

type key = string list
type value = MBytes.t

val mem: context -> key -> bool Lwt.t
val dir_mem: context -> key -> bool Lwt.t
val get: context -> key -> value option Lwt.t
val set: context -> key -> value -> t Lwt.t
val del: context -> key -> t Lwt.t
val remove_rec: context -> key -> t Lwt.t

(** [copy] returns None if the [from] key is not bound *)
val copy: context -> from:key -> to_:key -> context option Lwt.t

val fold:
  context -> key -> init:'a ->
  f:([ `Key of key | `Dir of key ] -> 'a -> 'a Lwt.t) ->
  'a Lwt.t

(** {2 Accessing and Updating Versions} **************************************)

val exists: index -> Context_hash.t -> bool Lwt.t
val checkout: index -> Context_hash.t -> context option Lwt.t
val checkout_exn: index -> Context_hash.t -> context Lwt.t
val commit:
  time:Time.t ->
  ?message:string ->
  context ->
  Context_hash.t Lwt.t
val set_head: index -> Chain_id.t -> Context_hash.t -> unit Lwt.t
val set_master: index -> Context_hash.t -> unit Lwt.t

(** {2 Predefined Fields} ****************************************************)

val get_protocol: context -> Protocol_hash.t Lwt.t
val set_protocol: context -> Protocol_hash.t -> context Lwt.t

val get_test_chain: context -> Test_chain_status.t Lwt.t
val set_test_chain: context -> Test_chain_status.t -> context Lwt.t

val del_test_chain: context -> context Lwt.t

val reset_test_chain: context -> Block_hash.t -> Time.t -> context Lwt.t

val fork_test_chain:
  context -> protocol:Protocol_hash.t -> expiration:Time.t -> context Lwt.t
val clear_test_chain: index -> Chain_id.t -> unit Lwt.t
