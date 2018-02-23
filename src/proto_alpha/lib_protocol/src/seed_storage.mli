(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error +=
  | Unknown of { oldest : Cycle_repr.t ;
                 cycle : Cycle_repr.t ;
                 latest : Cycle_repr.t } (* `Permanent *)

val init:
  Raw_context.t -> Raw_context.t tzresult Lwt.t

val for_cycle:
  Raw_context.t -> Cycle_repr.t -> Seed_repr.seed tzresult Lwt.t

val cycle_end:
  Raw_context.t -> Cycle_repr.t -> Raw_context.t tzresult Lwt.t
