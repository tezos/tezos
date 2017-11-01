(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val forge_endorsement:
  Client_commands.context ->
  Client_proto_rpcs.block ->
  ?force:bool ->
  src_sk:secret_key ->
  ?slot:int ->
  ?max_priority:int ->
  public_key ->
  Operation_hash.t tzresult Lwt.t

val create:
  Client_commands.context ->
  delay: int ->
  public_key_hash list ->
  Client_baking_blocks.block_info list tzresult Lwt_stream.t ->
  unit Lwt.t
