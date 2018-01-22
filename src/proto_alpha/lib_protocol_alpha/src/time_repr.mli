(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include module type of (struct include Time end)
type time = t

val pp: Format.formatter -> t -> unit
val of_seconds: string -> time option
val to_seconds_string: time -> string

val (+?) : time -> Period_repr.t -> time tzresult

