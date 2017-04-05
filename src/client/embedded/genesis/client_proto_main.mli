(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val mine:
  Client_rpcs.config ->
  ?timestamp: Time.t ->
  Client_node_rpcs.Blocks.block ->
  Data.Command.t ->
  Fitness.fitness ->
  Environment.Ed25519.Secret_key.t ->
  Block_hash.t tzresult Lwt.t

