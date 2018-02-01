(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_genesis

val bake:
  #Client_rpcs.ctxt ->
  ?timestamp: Time.t ->
  Client_node_rpcs.Blocks.block ->
  Data.Command.t ->
  Client_keys.sk_locator ->
  Block_hash.t tzresult Lwt.t

