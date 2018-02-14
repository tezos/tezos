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
open Tezos_micheline

module Program : Client_aliases.Alias
  with type t = Michelson_v1_parser.parsed Micheline_parser.parsing_result

val run :
  ?amount:Tez.t ->
  program:Michelson_v1_parser.parsed ->
  storage:Michelson_v1_parser.parsed ->
  input:Michelson_v1_parser.parsed ->
  Block_services.block ->
  #Proto_alpha.rpc_context ->
  (Script.expr * Script.expr * (Script.expr * Script.expr option) list option) tzresult Lwt.t

val trace :
  ?amount:Tez.t ->
  program:Michelson_v1_parser.parsed ->
  storage:Michelson_v1_parser.parsed ->
  input:Michelson_v1_parser.parsed ->
  Block_services.block ->
  #Proto_alpha.rpc_context ->
  (Script.expr * Script.expr * (int * Gas.t * Script.expr list) list * (Script.expr * Script.expr option) list option) tzresult Lwt.t

val print_run_result :
  #Client_context.logger ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  (Script_repr.expr * Script_repr.expr *
   (Script_repr.expr * Script_repr.expr option) list option) tzresult -> unit tzresult Lwt.t

val print_trace_result :
  #Client_context.logger ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  (Script_repr.expr * Script_repr.expr *
   (int * Gas.t * Script_repr.expr list) list *
   (Script_repr.expr * Script_repr.expr option) list option)
    tzresult -> unit tzresult Lwt.t

val hash_and_sign :
  Michelson_v1_parser.parsed ->
  Michelson_v1_parser.parsed ->
  Client_keys.sk_locator ->
  Block_services.block ->
  #Proto_alpha.rpc_context ->
  (string * string) tzresult Lwt.t

val typecheck_data :
  data:Michelson_v1_parser.parsed ->
  ty:Michelson_v1_parser.parsed ->
  Block_services.block ->
  #Proto_alpha.rpc_context ->
  unit tzresult Lwt.t

val typecheck_program :
  Michelson_v1_parser.parsed ->
  Block_services.block ->
  #Proto_alpha.rpc_context ->
  Script_tc_errors.type_map tzresult Lwt.t

val print_typecheck_result :
  emacs:bool ->
  show_types:bool ->
  print_source_on_error:bool ->
  Michelson_v1_parser.parsed ->
  (Script_tc_errors.type_map, error list) result ->
  #Client_context.logger ->
  unit tzresult Lwt.t
