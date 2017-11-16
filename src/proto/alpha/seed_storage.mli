(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error +=
  | Precomputed_seed
  | Invalid_cycle

val init:
  Raw_context.t -> Raw_context.t tzresult Lwt.t

val compute_for_cycle:
  Raw_context.t -> Cycle_repr.t -> Raw_context.t tzresult Lwt.t

val for_cycle: Raw_context.t -> Cycle_repr.t -> Seed_repr.seed tzresult Lwt.t

val clear_cycle:
  Raw_context.t -> Cycle_repr.t -> Raw_context.t tzresult Lwt.t
