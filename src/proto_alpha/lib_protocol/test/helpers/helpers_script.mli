(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

val init_amount : int
val execute_code_pred :
  ?tc:Alpha_context.t -> Helpers_block.result -> Script.t -> Script.expr ->
  Script_interpreter.execution_result proto_tzresult Lwt.t

