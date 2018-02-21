(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

val forge_endorsement:
  #Proto_alpha.full ->
  Block_services.block ->
  src_sk:Client_keys.sk_locator ->
  ?slots:int list ->
  ?max_priority:int ->
  public_key ->
  Operation_hash.t tzresult Lwt.t

val create :
  #Proto_alpha.full ->
  delay:int ->
  public_key_hash list ->
  Client_baking_blocks.block_info list tzresult Lwt_stream.t -> unit Lwt.t
