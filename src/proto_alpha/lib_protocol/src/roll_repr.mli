(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = private int32
type roll = t

val encoding: roll Data_encoding.t
val rpc_arg: roll RPC_arg.t

val random:
  Seed_repr.sequence -> bound:roll -> roll * Seed_repr.sequence

val first: roll
val succ: roll -> roll

val to_int32: roll -> Int32.t

val (=): roll -> roll -> bool

module Index : sig
  type t = roll
  val path_length: int
  val to_path: t -> string list -> string list
  val of_path: string list -> t option
end
