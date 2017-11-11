(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t

val peer_id: t -> P2p.Peer_id.t
val bootstrapped: t -> bool
val current_head: t -> Block_hash.t

val create:
  ?notify_new_block: (State.Block.t -> unit) ->
  ?notify_bootstrapped: (unit -> unit) ->
  ?notify_termination: (t -> unit) ->
  Block_validator.t ->
  Distributed_db.net_db -> P2p.Peer_id.t -> t Lwt.t
val shutdown: t -> unit Lwt.t

val notify_branch: t -> Block_locator.t -> unit
val notify_head: t -> Block_header.t -> unit
