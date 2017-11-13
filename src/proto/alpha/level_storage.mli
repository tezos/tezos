(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val current: Storage.t -> Level_repr.t
val previous: Storage.t -> Level_repr.t

val root: Storage.t -> Level_repr.t

val from_raw: Storage.t -> ?offset:int32 -> Raw_level_repr.t -> Level_repr.t
val pred: Storage.t -> Level_repr.t -> Level_repr.t option
val succ: Storage.t -> Level_repr.t -> Level_repr.t

val last_level_in_cycle: Storage.t -> Cycle_repr.t -> Level_repr.t
val levels_in_cycle: Storage.t -> Cycle_repr.t -> Level_repr.t list
