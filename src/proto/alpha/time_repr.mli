(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include module type of (struct include Time end)
type time = t

val pp: Format.formatter -> t -> unit
val of_seconds: string -> time option
val to_seconds: time -> string

val (+?) : time -> Period_repr.t -> time tzresult

