(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** The shell's notion of a level: an integer indicating the number of blocks
    since genesis: genesis is 0, all other blocks have increasing levels from
    there. *)
type t
type raw_level = t
val encoding: raw_level Data_encoding.t
val rpc_arg: raw_level RPC_arg.arg
val pp: Format.formatter -> raw_level -> unit
include Compare.S with type t := raw_level

val to_int32: raw_level -> int32
val of_int32_exn: int32 -> raw_level
val of_int32: int32 -> raw_level tzresult

val diff: raw_level -> raw_level -> int32

val root: raw_level

val succ: raw_level -> raw_level
val pred: raw_level -> raw_level option

module Index : sig
  type t = raw_level
  val path_length: int
  val to_path: t -> string list -> string list
  val of_path: string list -> t option
end
