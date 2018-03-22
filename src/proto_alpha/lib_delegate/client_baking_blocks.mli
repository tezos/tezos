(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

type block_info = {
  hash: Block_hash.t ;
  chain_id: Chain_id.t ;
  predecessor: Block_hash.t ;
  fitness: MBytes.t list ;
  timestamp: Time.t ;
  protocol: Protocol_hash.t ;
  next_protocol: Protocol_hash.t ;
  level: Level.t ;
}

val info:
  #Proto_alpha.rpc_context ->
  ?chain:Chain_services.chain ->
  Block_services.block ->
  block_info tzresult Lwt.t

val monitor_valid_blocks:
  #Proto_alpha.rpc_context ->
  ?chains:Chain_services.chain list ->
  ?protocols:Protocol_hash.t list ->
  ?next_protocols:Protocol_hash.t list ->
  unit -> block_info tzresult Lwt_stream.t tzresult Lwt.t

val monitor_heads:
  #Proto_alpha.rpc_context ->
  ?next_protocols:Protocol_hash.t list ->
  Chain_services.chain ->
  block_info tzresult Lwt_stream.t tzresult Lwt.t

val blocks_from_current_cycle:
  #Proto_alpha.rpc_context ->
  ?chain:Chain_services.chain ->
  Block_services.block ->
  ?offset:int32 ->
  unit ->
  Block_hash.t list tzresult Lwt.t
