(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = Ipaddr.V6.t
type port = int

val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

val of_string_opt : string -> t option
val of_string_exn : string -> t

val to_string : t -> string
