(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Tezos_context

val forge_endorsement:
  Client_commands.full_context ->
  Client_proto_rpcs.block ->
  src_sk:Client_keys.sk_locator ->
  ?slot:int ->
  ?max_priority:int ->
  public_key ->
  Operation_hash.t tzresult Lwt.t

val create :
  Client_commands.full_context ->
  delay:int ->
  public_key_hash list ->
  Client_baking_blocks.block_info list tzresult Lwt_stream.t -> unit Lwt.t
