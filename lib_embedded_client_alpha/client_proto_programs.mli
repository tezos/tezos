(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_micheline

module Program : Client_aliases.Alias
  with type t = Michelson_v1_parser.parsed Micheline_parser.parsing_result

val run :
  ?amount:Tez.t ->
  program:Michelson_v1_parser.parsed ->
  storage:Michelson_v1_parser.parsed ->
  input:Michelson_v1_parser.parsed ->
  Client_rpcs.block ->
  #Client_rpcs.ctxt ->
  (Script.expr * Script.expr) tzresult Lwt.t

val trace :
  ?amount:Tez.t ->
  program:Michelson_v1_parser.parsed ->
  storage:Michelson_v1_parser.parsed ->
  input:Michelson_v1_parser.parsed ->
  Client_rpcs.block ->
  #Client_rpcs.ctxt ->
  (Script.expr * Script.expr * (int * Gas.t * Script.expr list) list) tzresult Lwt.t

val print_trace_result :
  #Client_commands.logger ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  (Script_repr.expr * Script_repr.expr *
   (int * Gas.t * Script_repr.expr list) list)
    tzresult -> unit tzresult Lwt.t

val print_run_result :
  #Client_commands.logger ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  (Script.expr * Script.expr) tzresult ->
  unit tzresult Lwt.t

val hash_and_sign :
  Michelson_v1_parser.parsed ->
  Ed25519.Secret_key.t ->
  Client_proto_rpcs.block ->
  #Client_rpcs.ctxt ->
  (string * string) tzresult Lwt.t

val typecheck_data :
  data:Michelson_v1_parser.parsed ->
  ty:Michelson_v1_parser.parsed ->
  Client_proto_rpcs.block ->
  #Client_rpcs.ctxt ->
  unit tzresult Lwt.t

val typecheck_program :
  Michelson_v1_parser.parsed ->
  Client_proto_rpcs.block ->
  #Client_rpcs.ctxt ->
  Script_ir_translator.type_map tzresult Lwt.t

val print_typecheck_result :
  emacs:bool ->
  show_types:bool ->
  print_source_on_error:bool ->
  Michelson_v1_parser.parsed ->
  (Script_ir_translator.type_map, error list) result ->
  #Client_commands.logger ->
  unit tzresult Lwt.t
