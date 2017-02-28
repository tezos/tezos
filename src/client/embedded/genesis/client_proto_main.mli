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
  Fitness.fitness ->
  Environment.Ed25519.Secret_key.t ->
  unit tzresult Lwt.t

