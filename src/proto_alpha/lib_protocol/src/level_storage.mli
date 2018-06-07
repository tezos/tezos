(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val current: Raw_context.t -> Level_repr.t
val previous: Raw_context.t -> Level_repr.t

val root: Raw_context.t -> Level_repr.t

val from_raw: Raw_context.t -> ?offset:int32 -> Raw_level_repr.t -> Level_repr.t
val pred: Raw_context.t -> Level_repr.t -> Level_repr.t option
val succ: Raw_context.t -> Level_repr.t -> Level_repr.t

val first_level_in_cycle: Raw_context.t -> Cycle_repr.t -> Level_repr.t
val last_level_in_cycle: Raw_context.t -> Cycle_repr.t -> Level_repr.t
val levels_in_cycle: Raw_context.t -> Cycle_repr.t -> Level_repr.t list
val levels_in_current_cycle:
  Raw_context.t -> ?offset:int32 -> unit -> Level_repr.t list

val levels_with_commitments_in_cycle:
  Raw_context.t -> Cycle_repr.t -> Level_repr.t list

val last_allowed_fork_level: Raw_context.t -> Raw_level_repr.t
