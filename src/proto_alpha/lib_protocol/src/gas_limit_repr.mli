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
  | Limited of { remaining : Z.t }

val encoding : t Data_encoding.encoding
val pp : Format.formatter -> t -> unit

type cost

val cost_encoding : cost Data_encoding.encoding
val pp_cost : Format.formatter -> cost -> unit

type error += Block_quota_exceeded (* `Temporary *)
type error += Operation_quota_exceeded (* `Temporary *)

val consume : Z.t -> t -> cost -> (Z.t * t) tzresult
val check_enough : Z.t -> t -> cost -> unit tzresult

val free : cost
val step_cost : int -> cost
val alloc_cost : int -> cost
val alloc_bytes_cost : int -> cost
val alloc_bits_cost : int -> cost
val read_bytes_cost : Z.t -> cost
val write_bytes_cost : Z.t -> cost

val ( *@ ) : int -> cost -> cost
val ( +@ ) : cost -> cost -> cost
