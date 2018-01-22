(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t
type cycle = t
include Compare.S with type t := t
val encoding: cycle Data_encoding.t
val arg: cycle RPC_arg.arg
val pp: Format.formatter -> cycle -> unit

val root: cycle
val pred: cycle -> cycle option
val succ: cycle -> cycle

val to_int32: cycle -> int32
val of_int32_exn: int32 -> cycle

module Index : sig
  (* Storage_functors.INDEX with type t = cycle *)
  type t = cycle
  val path_length: int
  val to_path: t -> string list -> string list
  val of_path: string list -> t option
end
