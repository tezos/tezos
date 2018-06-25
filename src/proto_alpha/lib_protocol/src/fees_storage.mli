(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error += Cannot_pay_storage_fee (* `Temporary *)
type error += Operation_quota_exceeded (* `Temporary *)
type error += Storage_limit_too_high (* `Permanent *)

val origination_burn:
  Raw_context.t -> payer:Contract_repr.t -> (Raw_context.t * Tez_repr.t) tzresult Lwt.t

(** The returned Tez quantity is for logging purpose only *)
val record_paid_storage_space:
  Raw_context.t -> Contract_repr.t ->
  (Raw_context.t * Z.t * Z.t * Tez_repr.t) tzresult Lwt.t

val check_storage_limit:
  Raw_context.t -> storage_limit:Z.t -> unit tzresult

val with_fees_for_storage:
  Raw_context.t -> storage_limit:Z.t -> payer:Contract_repr.t ->
  (Raw_context.t -> (Raw_context.t * 'a) tzresult Lwt.t) ->
  (Raw_context.t * 'a) tzresult Lwt.t
