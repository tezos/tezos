(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t

val create: State.t -> Distributed_db.t -> t
val shutdown: t -> unit Lwt.t

val notify_block: t -> Block_hash.t -> Block_header.t -> unit Lwt.t

type net_validator

type error +=
  | Non_increasing_timestamp
  | Non_increasing_fitness

val activate: t -> ?max_child_ttl:int -> State.Net.t -> net_validator Lwt.t
val get: t -> Net_id.t -> net_validator tzresult Lwt.t
val get_exn: t -> Net_id.t -> net_validator Lwt.t
val deactivate: net_validator -> unit Lwt.t

val net_state: net_validator -> State.Net.t
val net_db: net_validator -> Distributed_db.net_db

val fetch_block:
  net_validator -> Block_hash.t -> State.Block.t tzresult Lwt.t

val inject_block:
  t -> ?force:bool ->
  MBytes.t -> Distributed_db.operation list list ->
  (Block_hash.t * State.Block.t tzresult Lwt.t) tzresult Lwt.t

val prevalidator: net_validator -> Prevalidator.t
val test_validator: net_validator -> (net_validator * Distributed_db.net_db) option

val watcher: net_validator -> State.Block.t Lwt_stream.t * Watcher.stopper
val new_head_watcher: net_validator -> State.Block.t Lwt_stream.t * Watcher.stopper
val global_watcher: t -> State.Block.t Lwt_stream.t * Watcher.stopper

val bootstrapped: net_validator -> unit Lwt.t
