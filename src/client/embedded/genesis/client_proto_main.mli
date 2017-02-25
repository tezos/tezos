(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val mine:
  Client_commands.context ->
  Client_node_rpcs.Blocks.block ->
  Data.Command.t ->
  int64 -> Sodium.secret Sodium.Sign.key -> unit tzresult Lwt.t

