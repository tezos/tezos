(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type fitness = MBytes.t list

val compare: fitness -> fitness -> int
val pp: Format.formatter -> fitness -> unit
val to_string: fitness -> string

val encoding: fitness Data_encoding.
                        t

