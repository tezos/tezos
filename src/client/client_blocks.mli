(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val genesis: Block_hash.t

val get_block_hash:
  Client_commands.context ->
  Client_node_rpcs.Blocks.block ->
  Block_hash.Table.key Lwt.t

val get_block_info:
  Client_commands.context ->
  Client_node_rpcs.Blocks.block ->
  Client_node_rpcs.Blocks.block_info Lwt.t
