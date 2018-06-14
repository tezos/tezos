(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


val sleep_until: Time.t -> unit Lwt.t option

val wait_for_first_block:
  ?info:((unit Lwt.t, unit) Client_context.lwt_format -> unit Lwt.t) ->
  Client_baking_blocks.block_info tzresult Lwt_stream.t ->
  (Client_baking_blocks.block_info -> 'a Lwt.t) ->
  'a Lwt.t
