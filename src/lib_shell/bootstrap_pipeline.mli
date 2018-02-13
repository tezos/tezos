(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t

val create:
  ?notify_new_block: (State.Block.t -> unit) ->
  block_header_timeout:float ->
  block_operations_timeout: float ->
  Block_validator.t ->
  P2p_peer.Id.t -> Distributed_db.chain_db ->
  Block_locator.t -> t

val wait: t -> unit tzresult Lwt.t

val cancel: t -> unit Lwt.t
