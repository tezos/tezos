(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Shell - Main entry point of the validation scheduler. *)

type t

val create:
  State.t ->
  Distributed_db.t ->
  Peer_validator.limits ->
  Block_validator.limits ->
  Prevalidator.limits ->
  Chain_validator.limits ->
  t Lwt.t
val shutdown: t -> unit Lwt.t

(** Start the validation scheduler of a given chain. *)
val activate:
  t ->
  ?max_child_ttl:int ->
  State.Chain.t -> Chain_validator.t Lwt.t

type error +=
  | Inactive_chain of Chain_id.t
val get: t -> Chain_id.t -> Chain_validator.t tzresult Lwt.t
val get_exn: t -> Chain_id.t -> Chain_validator.t Lwt.t

(** Force the validation of a block. *)
val validate_block:
  t ->
  ?force:bool ->
  ?chain_id:Chain_id.t ->
  MBytes.t -> Operation.t list list ->
  (Block_hash.t * State.Block.t tzresult Lwt.t) tzresult Lwt.t

(** Monitor all the valid block (for all activate chains). *)
val watcher: t -> State.Block.t Lwt_stream.t * Lwt_watcher.stopper

val inject_operation:
  t ->
  ?chain_id:Chain_id.t ->
  Operation.t -> unit tzresult Lwt.t
