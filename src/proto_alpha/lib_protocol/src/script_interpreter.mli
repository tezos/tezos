(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

type error += Overflow of Script.location
type error += Reject of Script.location
type error += Runtime_contract_error : Contract.t * Script.expr -> error

type execution_result =
  { ctxt : context ;
    storage : Script.expr ;
    big_map_diff : Contract.big_map_diff option ;
    operations : internal_operation list }

val execute:
  Alpha_context.t ->
  check_operations: bool ->
  source: Contract.t ->
  payer: Contract.t ->
  self: (Contract.t * Script.t) ->
  parameter: Script.expr ->
  amount: Tez.t ->
  execution_result tzresult Lwt.t

type execution_trace =
  (Script.location * Gas.t * Script.expr list) list

val trace:
  Alpha_context.t ->
  check_operations: bool ->
  source: Contract.t ->
  payer: Contract.t ->
  self: (Contract.t * Script.t) ->
  parameter: Script.expr ->
  amount: Tez.t ->
  (execution_result * execution_trace) tzresult Lwt.t
