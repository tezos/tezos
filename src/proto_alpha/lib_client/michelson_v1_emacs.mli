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

val print_expr :
  Format.formatter ->
  Script.expr ->
  unit
val print_type_map :
  Format.formatter ->
  Michelson_v1_parser.parsed * Script_tc_errors.type_map ->
  unit
val report_errors :
  Format.formatter ->
  Michelson_v1_parser.parsed * Error_monad.error list ->
  unit
