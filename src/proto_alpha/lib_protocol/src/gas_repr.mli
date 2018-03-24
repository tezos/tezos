(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Unaccounted
  | Limited of { remaining : int }

val encoding : t Data_encoding.encoding
val pp : Format.formatter -> t -> unit

type cost

val cost_encoding : cost Data_encoding.encoding
val pp_cost : Format.formatter -> cost -> unit

type error += Quota_exceeded

val consume : t -> cost -> t tzresult

val free : cost
val step_cost : int -> cost
val alloc_cost : int -> cost
val alloc_bytes_cost : int -> cost
val alloc_bits_cost : int -> cost

val ( *@ ) : int -> cost -> cost
val ( +@ ) : cost -> cost -> cost
