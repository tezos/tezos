(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t
type voting_period = t
val encoding: voting_period Data_encoding.t
val arg: voting_period RPC.Arg.arg
val pp: Format.formatter -> voting_period -> unit
include Compare.S with type t := voting_period

val to_int32: voting_period -> int32
val of_int32_exn: int32 -> voting_period

val root: voting_period
val succ: voting_period -> voting_period

type kind =
  | Proposal
  | Testing_vote
  | Testing
  | Promotion_vote

val kind_encoding: kind Data_encoding.t
