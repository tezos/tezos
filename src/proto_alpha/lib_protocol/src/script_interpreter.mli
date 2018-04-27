(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

type execution_trace =
  (Script.location * Gas.t * (Script.expr * string option) list) list

type error += Reject of Script.location * Script.expr option * execution_trace option
type error += Overflow of Script.location * execution_trace option
type error += Runtime_contract_error : Contract.t * Script.expr -> error
type error += Bad_contract_parameter of Contract.t (* `Permanent *)

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
