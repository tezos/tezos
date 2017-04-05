(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val parse_program: string -> Script.code tzresult Lwt.t
val parse_data: string -> Script.expr tzresult Lwt.t
val parse_data_type: string -> Script.expr tzresult Lwt.t

module Program : Client_aliases.Alias with type t = Script.code

val commands: unit -> Client_commands.command list
