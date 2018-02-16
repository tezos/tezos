(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t

type limits = {
  bootstrap_threshold: int ;
  worker_limits: Worker_types.limits
}

val create:
  ?max_child_ttl:int ->
  Peer_validator.limits ->
  Prevalidator.limits ->
  Block_validator.t ->
  State.Block.t Lwt_watcher.input ->
  Distributed_db.t ->
  State.Chain.t ->
  limits ->
  t Lwt.t

val bootstrapped: t -> unit Lwt.t

val chain_id: t -> Chain_id.t
val chain_state: t -> State.Chain.t
val prevalidator: t -> Prevalidator.t
val chain_db: t -> Distributed_db.chain_db
val child: t -> t option

val validate_block:
  t ->
  ?force:bool ->
  Block_hash.t -> Block_header.t -> Operation.t list list ->
  State.Block.t tzresult Lwt.t

val shutdown: t -> unit Lwt.t

val valid_block_watcher: t -> State.Block.t Lwt_stream.t * Lwt_watcher.stopper
val new_head_watcher: t -> State.Block.t Lwt_stream.t * Lwt_watcher.stopper

val running_workers: unit -> (Chain_id.t * t) list
val status: t -> Worker_types.worker_status

val pending_requests : t -> (Time.t * Chain_validator_worker_state.Request.view) list
val current_request : t -> (Time.t * Time.t * Chain_validator_worker_state.Request.view) option
val last_events : t -> (Lwt_log_core.level * Chain_validator_worker_state.Event.t list) list
