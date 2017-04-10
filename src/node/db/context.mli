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
  context Lwt.t

val commit_test_network_genesis:
  Block_hash.t -> Time.t -> context ->
  (Net_id.t * Block_hash.t) tzresult Lwt.t

(** {2 Generic interface} ****************************************************)

include Persist.STORE with type t := context

(** {2 Accessing and Updating Versions} **************************************)

exception Preexistent_context of Block_hash.t
val exists: index -> Block_hash.t -> bool Lwt.t
val checkout: index -> Block_hash.t -> context option Lwt.t
val checkout_exn: index -> Block_hash.t -> context Lwt.t
val commit:
  Block_hash.t ->
  time:Time.t ->
  message:string ->
  context -> unit Lwt.t

(** {2 Predefined Fields} ****************************************************)

val get_protocol: context -> Protocol_hash.t Lwt.t
val set_protocol: context -> Protocol_hash.t -> context Lwt.t

type test_network =
  | Not_running
  | Forking of {
      protocol: Protocol_hash.t ;
      expiration: Time.t ;
    }
  | Running of {
      net_id: Net_id.t ;
      genesis: Block_hash.t ;
      protocol: Protocol_hash.t ;
      expiration: Time.t ;
    }

val test_network_encoding: test_network Data_encoding.t

val get_test_network: context -> test_network Lwt.t
val set_test_network: context -> test_network -> context Lwt.t
val del_test_network: context -> context Lwt.t

val reset_test_network: context -> Block_hash.t -> Time.t -> context Lwt.t

val fork_test_network:
  context -> protocol:Protocol_hash.t -> expiration:Time.t -> context Lwt.t
