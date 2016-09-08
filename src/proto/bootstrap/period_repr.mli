(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t
type period = t
include Compare.S with type t := t
val encoding : period Data_encoding.t
val pp: Format.formatter -> period -> unit


val to_seconds : period -> int64

(** [of_second period] fails if period is not positive *)
val of_seconds : int64 -> period tzresult

(** [of_second period] fails if period is not positive.
    It should only be used at toplevel for constants. *)
val of_seconds_exn : int64 -> period

val mult : int32 -> period -> period tzresult
