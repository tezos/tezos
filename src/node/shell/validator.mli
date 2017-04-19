(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type worker

val create_worker: ?max_ttl:int -> State.t -> Distributed_db.t -> worker
val shutdown: worker -> unit Lwt.t

val notify_block: worker -> Block_hash.t -> Block_header.t -> unit Lwt.t

type t

type error +=
   | Non_increasing_timestamp
   | Non_increasing_fitness

val activate: worker -> State.Net.t -> t Lwt.t
val get: worker -> Net_id.t -> t tzresult Lwt.t
val get_exn: worker -> Net_id.t -> t Lwt.t
val deactivate: t -> unit Lwt.t

val net_state: t -> State.Net.t
val net_db: t -> Distributed_db.net_db

val fetch_block:
  t -> Block_hash.t -> State.Block.t tzresult Lwt.t

val inject_block:
  worker -> ?force:bool ->
  MBytes.t -> Distributed_db.operation list list ->
  (Block_hash.t * State.Block.t tzresult Lwt.t) tzresult Lwt.t

val prevalidator: t -> Prevalidator.t
val test_validator: t -> (t * Distributed_db.net_db) option

val watcher: t -> State.Block.t Lwt_stream.t * Watcher.stopper
val new_head_watcher: t -> State.Block.t Lwt_stream.t * Watcher.stopper
val global_watcher: worker -> State.Block.t Lwt_stream.t * Watcher.stopper

val bootstrapped: t -> unit Lwt.t
