(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t

type timeout = {
  operation: float ;
  block_header: float ;
  block_operations: float ;
  protocol: float ;
  new_head_request: float ;
}

val create:
  ?max_child_ttl:int ->
  ?bootstrap_threshold:int ->
  timeout ->
  Block_validator.t ->
  State.Block.t Lwt_watcher.input ->
  Distributed_db.t ->
  State.Net.t ->
  t Lwt.t

val bootstrapped: t -> unit Lwt.t

val net_id: t -> Net_id.t
val net_state: t -> State.Net.t
val prevalidator: t -> Prevalidator.t
val net_db: t -> Distributed_db.net_db
val child: t -> t option

val validate_block:
  t ->
  ?force:bool ->
  Block_hash.t -> Block_header.t -> Operation.t list list ->
  State.Block.t tzresult Lwt.t

val shutdown: t -> unit Lwt.t

val valid_block_watcher: t -> State.Block.t Lwt_stream.t * Lwt_watcher.stopper
val new_head_watcher: t -> State.Block.t Lwt_stream.t * Lwt_watcher.stopper

