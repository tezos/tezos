(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = private {
  level: Raw_level_repr.t ;
  level_position: int32 ;
  cycle: Cycle_repr.t ;
  cycle_position: int32 ;
  voting_period: Voting_period_repr.t ;
  voting_period_position: int32 ;
  expected_commitment: bool ;
}

type level = t

include Compare.S with type t := level

val encoding: level Data_encoding.t
val pp: Format.formatter -> level -> unit
val pp_full: Format.formatter -> level -> unit

val root: Raw_level_repr.t -> level

val from_raw:
  first_level:Raw_level_repr.t ->
  blocks_per_cycle:int32 ->
  voting_period_length:int32 ->
  blocks_per_commitment:int32 ->
  Raw_level_repr.t -> level

val diff: level -> level -> int32
