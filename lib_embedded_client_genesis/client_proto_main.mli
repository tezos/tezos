(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_embedded_raw_protocol_genesis

val bake:
  #Client_rpcs.ctxt ->
  ?timestamp: Time.t ->
  Client_node_rpcs.Blocks.block ->
  Data.Command.t ->
  Environment.Ed25519.Secret_key.t ->
  Block_hash.t tzresult Lwt.t

