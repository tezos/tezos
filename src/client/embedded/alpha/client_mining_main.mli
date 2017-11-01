(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val mine_block:
  Client_commands.context ->
  Client_proto_rpcs.block ->
  ?force:bool ->
  ?max_priority: int ->
  ?free_baking: bool ->
  ?src_sk:secret_key ->
  public_key_hash ->
  unit tzresult Lwt.t

val commands: unit -> Client_commands.command list
