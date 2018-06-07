(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val get_block: State.Chain.t -> Block_services.block -> State.Block.t Lwt.t

val build_raw_rpc_directory:
  (module Block_services.PROTO) ->
  (module Registered_protocol.T) ->
  State.Block.t RPC_directory.directory

val build_rpc_directory:
  State.Chain.t ->
  Block_services.block ->
  'a RPC_directory.t Lwt.t
