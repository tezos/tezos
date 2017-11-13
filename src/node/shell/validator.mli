(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t

val create: State.t -> Distributed_db.t -> Net_validator.timeout -> t
val shutdown: t -> unit Lwt.t

val activate:
  t ->
  ?bootstrap_threshold:int ->
  ?max_child_ttl:int ->
  State.Net.t -> Net_validator.t Lwt.t

type error +=
  | Inactive_network of Net_id.t
val get: t -> Net_id.t -> Net_validator.t tzresult Lwt.t
val get_exn: t -> Net_id.t -> Net_validator.t Lwt.t

val inject_block:
  t ->
  ?force:bool ->
  MBytes.t -> Operation.t list list ->
  (Block_hash.t * State.Block.t tzresult Lwt.t) tzresult Lwt.t

val watcher: t -> State.Block.t Lwt_stream.t * Watcher.stopper
