(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = private {
  level: Raw_level_repr.t (** The level of the block relative to genesis. This
                              is also the Shell's notion of level. *);
  level_position: int32 (** The level of the block relative to the block that
                            starts protocol alpha. This is specific to the
                            protocol alpha. Other protocols might or might not
                            include a similar notion. *);
  cycle: Cycle_repr.t (** The current cycle's number. Note that cycles are a
                          protocol-specific notion. As a result, the cycle
                          number starts at 0 with the first block of protocol
                          alpha. *);
  cycle_position: int32 (** The current level of the block relative to the first
                            block of the current cycle. *);
  voting_period: Voting_period_repr.t ;
  voting_period_position: int32 ;
  expected_commitment: bool ;
}

(* Note that, the type `t` above must respect some invariants (hence the
   `private` annotation). Notably:

   level_position = cycle * blocks_per_cycle + cycle_position
*)



type level = t

include Compare.S with type t := level

val encoding: level Data_encoding.t
val pp: Format.formatter -> level -> unit
val pp_full: Format.formatter -> level -> unit

val root: Raw_level_repr.t -> level

val from_raw:
  first_level:Raw_level_repr.t ->
  blocks_per_cycle:int32 ->
  blocks_per_voting_period:int32 ->
  blocks_per_commitment:int32 ->
  Raw_level_repr.t -> level

val diff: level -> level -> int32
