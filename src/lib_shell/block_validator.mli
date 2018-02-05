(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t

type limits = {
  protocol_timeout: float ;
  worker_limits : Worker_types.limits ;
}

type error += Closed of unit

val create:
  limits -> Distributed_db.t -> t Lwt.t

val validate:
  t ->
  ?canceler:Lwt_canceler.t ->
  ?peer:P2p_peer.Id.t ->
  ?notify_new_block:(State.Block.t -> unit) ->
  Distributed_db.net_db ->
  Block_hash.t -> Block_header.t -> Operation.t list list ->
  State.Block.t tzresult Lwt.t

val fetch_and_compile_protocol:
  t ->
  ?peer:P2p_peer.Id.t ->
  ?timeout:float ->
  Protocol_hash.t -> Registred_protocol.t tzresult Lwt.t

val shutdown: t -> unit Lwt.t

val running_worker: unit -> t
val status: t -> Worker_types.worker_status

val pending_requests : t -> (Time.t * Block_validator_worker_state.Request.view) list
val current_request : t -> (Time.t * Time.t * Block_validator_worker_state.Request.view) option
val last_events : t -> (Lwt_log_core.level * Block_validator_worker_state.Event.t list) list
