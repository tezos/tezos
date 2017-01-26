(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type block_info = {
  hash: Block_hash.t ;
  predecessor: Block_hash.t ;
  fitness: MBytes.t list ;
  timestamp: Time.t ;
  protocol: Protocol_hash.t option ;
  level: Level.t ;
}

val info:
  Client_commands.context ->
  ?operations:bool -> Client_node_rpcs.Blocks.block -> block_info tzresult Lwt.t

val compare:
  block_info -> block_info -> int

val monitor:
  Client_commands.context ->
  ?operations:bool -> ?length:int -> ?heads:Block_hash.t list ->
  ?delay:int -> ?min_date:Time.t -> ?min_heads:int ->
  ?compare:(block_info -> block_info -> int) ->
  unit -> block_info list Lwt_stream.t Lwt.t

val blocks_from_cycle:
  Client_commands.context ->
  Client_node_rpcs.Blocks.block ->
  Cycle.t ->
  block_info list tzresult Lwt.t
