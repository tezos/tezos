(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

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
