(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t
type raw_level = t
val encoding: raw_level Data_encoding.t
val arg: raw_level RPC.Arg.arg
val pp: Format.formatter -> raw_level -> unit
include Compare.S with type t := raw_level

val to_int32: raw_level -> int32
val of_int32_exn: int32 -> raw_level
val of_int32: int32 -> raw_level tzresult

val diff: raw_level -> raw_level -> int32

val root: raw_level

val succ: raw_level -> raw_level
val pred: raw_level -> raw_level option
