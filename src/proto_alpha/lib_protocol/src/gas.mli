(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t
type cost

val consume : t -> cost -> t

val encoding : t Data_encoding.encoding
val pp : Format.formatter -> t -> unit

val encoding_cost : cost Data_encoding.encoding
val pp_cost : Format.formatter -> cost -> unit

val check : t -> unit tzresult Lwt.t
val consume_check : t -> cost -> t tzresult Lwt.t
val check_error : t -> unit tzresult
val consume_check_error : t -> cost -> t tzresult
type error += Quota_exceeded

val of_int : int -> t
val remaining : t -> int

val ( *@ ) : int -> cost -> cost
val ( +@ ) : cost -> cost -> cost

val used : original:t -> current:t -> t

val free : cost
val step_cost : int -> cost
val alloc_cost : int -> cost
val alloc_bytes_cost : int -> cost
val alloc_bits_cost : int -> cost

val max_gas : t

val fold_left : cycle_cost:cost ->
  t ->
  (t -> 'a -> 'b -> ('b * t) tzresult Lwt.t) ->
  'b -> 'a list -> ('b * t) tzresult Lwt.t

val fold_right : cycle_cost:cost ->
  t ->
  (t -> 'a -> 'b -> ('b * t) tzresult Lwt.t) ->
  'b -> 'a list -> ('b * t) tzresult Lwt.t

val fold_right_error : cycle_cost:cost ->
  t ->
  (t -> 'a -> 'b -> ('b * t) tzresult) ->
  'b -> 'a list -> ('b * t) tzresult

val fold_left_error : cycle_cost:cost ->
  t ->
  (t -> 'a -> 'b -> ('b * t) tzresult) ->
  'b -> 'a list -> ('b * t) tzresult
