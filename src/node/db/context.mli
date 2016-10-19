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
type store

(** Open or initialize a versioned store at a given path. *)
val init:
  ?patch_context:(store -> store Lwt.t) ->
  root:string ->
  index Lwt.t

val create_genesis_context:
  index -> Store.genesis -> Protocol_hash.t -> store Lwt.t

(** {2 Generic interface} ****************************************************)

include Persist.STORE with type t = store

(** {2 Accessing and Updating Versions} **************************************)

exception Preexistent_context of string * Block_hash.t
val exists: index -> Block_hash.t -> bool Lwt.t
val commit: index -> Store.block -> Block_hash.t -> store -> unit Lwt.t
val commit_invalid:
  index -> Store.block -> Block_hash.t -> error list -> unit Lwt.t
val checkout: index -> Block_hash.t -> store tzresult option Lwt.t
exception Invalid_context of error list
val checkout_exn: index -> Block_hash.t -> store Lwt.t

(** {2 Predefined Fields} ****************************************************)

val get_protocol: store -> Protocol_hash.t Lwt.t
val set_protocol: store -> Protocol_hash.t -> store Lwt.t

val get_test_protocol: store -> Protocol_hash.t Lwt.t
val set_test_protocol: store -> Protocol_hash.t -> store Lwt.t

val get_test_network: store -> Store.net_id option Lwt.t
val set_test_network: store -> Store.net_id -> store Lwt.t
val del_test_network: store -> store Lwt.t

val get_test_network_expiration: store -> Time.t option Lwt.t
val set_test_network_expiration: store -> Time.t -> store Lwt.t
val del_test_network_expiration: store -> store Lwt.t

val read_and_reset_fork_test_network: store -> (bool * store) Lwt.t
val fork_test_network: store -> store Lwt.t

val get_genesis_time: store -> Time.t Lwt.t
val get_genesis_block: store -> Block_hash.t Lwt.t
