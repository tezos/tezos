(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type worker

module P2p = Netparams

val create_worker: P2p.net -> State.t -> worker
val shutdown: worker -> unit Lwt.t

val notify_block: worker -> Block_hash.t -> Store.block -> unit Lwt.t

type t

val activate: worker -> State.Net.t -> t Lwt.t
val get: worker -> State.net_id -> t tzresult Lwt.t
val get_exn: worker -> State.net_id -> t Lwt.t
val deactivate: t -> unit Lwt.t

val fetch_block:
  t -> Block_hash.t -> State.Valid_block.t tzresult Lwt.t
val prevalidator: t -> Prevalidator.t
val test_validator: t -> (t * State.Net.t) option
