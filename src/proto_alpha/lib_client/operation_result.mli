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

val pp_internal_operation:
  Format.formatter -> internal_operation -> unit

val pp_operation_result:
  Format.formatter -> (operation * Apply_operation_result.operation_result) -> unit
