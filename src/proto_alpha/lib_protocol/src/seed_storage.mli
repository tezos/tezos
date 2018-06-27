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

(** Generates the first [preserved_cycles+2] seeds for which
    there are no nonces. *)
val init:
  Raw_context.t -> Raw_context.t tzresult Lwt.t

val for_cycle:
  Raw_context.t -> Cycle_repr.t -> Seed_repr.seed tzresult Lwt.t

(** If it is the end of the cycle, computes and stores the seed of cycle at
    distance [preserved_cycle+2] in the future using the seed of the previous
    cycle and the revelations of the current one.  *)
val cycle_end:
  Raw_context.t -> Cycle_repr.t ->
  (Raw_context.t * Nonce_storage.unrevealed list) tzresult Lwt.t
