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
  Format.formatter -> packed_internal_operation -> unit

val pp_operation_result:
  Format.formatter ->
  ('kind contents_list * 'kind Apply_results.contents_result_list) -> unit
