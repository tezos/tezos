(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val parse_program:
  Client_commands.context ->
  string -> Script.code Lwt.t
val parse_data:
  Client_commands.context ->
  string -> Script.expr Lwt.t
val parse_data_type:
  Client_commands.context ->
  string -> Script.expr Lwt.t

module Program : Client_aliases.Alias with type t = Script.code

val commands: unit -> Client_commands.command list
