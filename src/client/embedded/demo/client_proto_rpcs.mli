(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Node_rpc_services

val echo:
  Client_commands.context ->
  Blocks.block -> string -> string Lwt.t
val failing:
  Client_commands.context ->
  Blocks.block -> int -> unit tzresult Lwt.t
