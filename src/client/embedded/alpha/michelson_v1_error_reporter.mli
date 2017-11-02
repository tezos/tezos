(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val report_errors :
  details: bool ->
  show_source: bool ->
  ?parsed: Michelson_v1_parser.parsed ->
  Format.formatter ->
  Error_monad.error list ->
  unit
