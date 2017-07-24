(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type 'a parsed =
  { ast : 'a ;
    source : string ;
    loc_table : (string * (int * Script_located_ir.location) list) list }

val parse_program: string -> Script.code parsed tzresult Lwt.t
val parse_data: string -> Script.expr parsed tzresult Lwt.t
val parse_data_type: string -> Script.expr parsed tzresult Lwt.t

val print_storage: Format.formatter -> Script.storage -> unit

module Program : Client_aliases.Alias with type t = Script.code parsed

val commands: unit -> Client_commands.command list
