(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

type error += Cannot_pay_storage_fee

val origination_burn:
  Alpha_context.t -> payer:Contract.t ->
  Contract.t -> Alpha_context.t tzresult Lwt.t

val update_script_storage:
  Alpha_context.t -> payer:Contract.t ->
  Contract.t -> (Alpha_context.t * Tez.t) tzresult Lwt.t

