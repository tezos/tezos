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

open Alpha_context

type execution_trace =
  (Script.location * Gas.t * (Script.expr * string option) list) list

type error += Reject of Script.location * Script.expr * execution_trace option
type error += Overflow of Script.location * execution_trace option
type error += Runtime_contract_error : Contract.t * Script.expr -> error
type error += Bad_contract_parameter of Contract.t (* `Permanent *)
type error += Cannot_serialize_log
type error += Cannot_serialize_failure
type error += Cannot_serialize_storage

type execution_result =
  { ctxt : context ;
    storage : Script.expr ;
    big_map_diff : Contract.big_map_diff option ;
    operations : packed_internal_operation list }

val execute:
  Alpha_context.t ->
  Script_ir_translator.unparsing_mode ->
  source: Contract.t ->
  payer: Contract.t ->
  self: (Contract.t * Script.t) ->
  parameter: Script.expr ->
  amount: Tez.t ->
  execution_result tzresult Lwt.t

val trace:
  Alpha_context.t ->
  Script_ir_translator.unparsing_mode ->
  source: Contract.t ->
  payer: Contract.t ->
  self: (Contract.t * Script.t) ->
  parameter: Script.expr ->
  amount: Tez.t ->
  (execution_result * execution_trace) tzresult Lwt.t
