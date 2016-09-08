(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val parse_program: string -> Script.code Lwt.t
val parse_data: string -> Script.expr Lwt.t
val parse_data_type: string -> Script.expr Lwt.t

val print_program: Format.formatter -> Script.code -> unit

module Program : Client_aliases.Alias with type t = Script.code

val commands: unit -> Cli_entries.command list
