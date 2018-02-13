(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t
include Compare.S with type t := t

val add : t -> int64 -> t
val diff : t -> t -> int64

val of_seconds : int64 -> t
val to_seconds : t -> int64

val of_notation : string -> t option
val of_notation_exn : string -> t
val to_notation : t -> string

val encoding : t Data_encoding.t
val rfc_encoding : t Data_encoding.t

val pp_hum : Format.formatter -> t -> unit



