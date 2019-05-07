(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Proto_alpha
open Alpha_context
open Tezos_micheline

module Program : Client_aliases.Alias
  with type t = Michelson_v1_parser.parsed Micheline_parser.parsing_result

val run :
  #Proto_alpha.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?amount:Tez.t ->
  program:Michelson_v1_parser.parsed ->
  storage:Michelson_v1_parser.parsed ->
  input:Michelson_v1_parser.parsed ->
  unit ->
  (Script.expr *
   packed_internal_operation list *
   Contract.big_map_diff option) tzresult Lwt.t

val trace :
  #Proto_alpha.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?amount:Tez.t ->
  program:Michelson_v1_parser.parsed ->
  storage:Michelson_v1_parser.parsed ->
  input:Michelson_v1_parser.parsed ->
  unit ->
  (Script.expr *
   packed_internal_operation list *
   Script_interpreter.execution_trace *
   Contract.big_map_diff option) tzresult Lwt.t

val print_run_result :
  #Client_context.printer ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  (Script_repr.expr *
   packed_internal_operation list *
   Contract.big_map_diff option) tzresult -> unit tzresult Lwt.t

val print_trace_result :
  #Client_context.printer ->
  show_source:bool ->
  parsed:Michelson_v1_parser.parsed ->
  (Script_repr.expr *
   packed_internal_operation list *
   Script_interpreter.execution_trace *
   Contract.big_map_diff option)
    tzresult -> unit tzresult Lwt.t

val typecheck_data :
  #Proto_alpha.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?gas:Z.t ->
  data:Michelson_v1_parser.parsed ->
  ty:Michelson_v1_parser.parsed ->
  unit ->
  Gas.t tzresult Lwt.t

val typecheck_program :
  #Proto_alpha.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?gas:Z.t ->
  Michelson_v1_parser.parsed ->
  (Script_tc_errors.type_map * Gas.t) tzresult Lwt.t

val print_typecheck_result :
  emacs:bool ->
  show_types:bool ->
  print_source_on_error:bool ->
  Michelson_v1_parser.parsed ->
  (Script_tc_errors.type_map * Gas.t) tzresult ->
  #Client_context.printer ->
  unit tzresult Lwt.t
