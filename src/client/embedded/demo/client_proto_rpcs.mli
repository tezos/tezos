(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Node_rpc_services

val echo:
  Client_rpcs.config ->
  Blocks.block -> string -> string tzresult Lwt.t
val failing:
  Client_rpcs.config ->
  Blocks.block -> int -> unit tzresult Lwt.t
