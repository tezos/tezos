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
  new_head_request_timeout: float ;
  block_header_timeout: float ;
  block_operations_timeout: float ;
  protocol_timeout: float ;
  worker_limits: Worker_types.limits
}

val peer_id: t -> P2p_peer.Id.t
val bootstrapped: t -> bool
val current_head: t -> Block_header.t

val create:
  ?notify_new_block: (State.Block.t -> unit) ->
  ?notify_bootstrapped: (unit -> unit) ->
  ?notify_termination: (unit -> unit) ->
  limits ->
  Block_validator.t ->
  Distributed_db.chain_db -> P2p_peer.Id.t -> t Lwt.t
val shutdown: t -> unit Lwt.t

val notify_branch: t -> Block_locator.t -> unit
val notify_head: t -> Block_header.t -> unit

val running_workers: unit -> ((Chain_id.t * P2p_peer.Id.t) * t) list
val status: t -> Worker_types.worker_status

val current_request : t -> (Time.t * Time.t * Peer_validator_worker_state.Request.view) option
val last_events : t -> (Lwt_log_core.level * Peer_validator_worker_state.Event.t list) list
