(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t
val zero: t
val one: t

external abs: t -> t = "ml_z_abs" "ml_as_z_abs"
(** Absolute value. *)

external neg: t -> t = "ml_z_neg" "ml_as_z_neg"
(** Unary negation. *)

external add: t -> t -> t = "ml_z_add" "ml_as_z_add"
(** Addition. *)

external sub: t -> t -> t = "ml_z_sub" "ml_as_z_sub"
(** Subtraction. *)

external mul: t -> t -> t = "ml_z_mul" "ml_as_z_mul"
(** Multiplication. *)

val ediv_rem: t -> t -> (t * t)
(** Euclidean division and remainder.  [ediv_rem a b] returns a pair [(q, r)]
    such that [a = b * q + r] and [0 <= r < |b|].
    Raises [Division_by_zero] if [b = 0].
*)

external logand: t -> t -> t = "ml_z_logand" "ml_as_z_logand"
(** Bitwise logical and. *)

external logor: t -> t -> t = "ml_z_logor" "ml_as_z_logor"
(** Bitwise logical or. *)

external logxor: t -> t -> t = "ml_z_logxor" "ml_as_z_logxor"
(** Bitwise logical exclusive or. *)

external lognot: t -> t = "ml_z_lognot" "ml_as_z_lognot"
(** Bitwise logical negation.
    The identity [lognot a]=[-a-1] always hold.
*)

external shift_left: t -> int -> t = "ml_z_shift_left" "ml_as_z_shift_left"
(** Shifts to the left.
    Equivalent to a multiplication by a power of 2.
    The second argument must be non-negative.
*)

external shift_right: t -> int -> t = "ml_z_shift_right" "ml_as_z_shift_right"
(** Shifts to the right.
    This is an arithmetic shift,
    equivalent to a division by a power of 2 with rounding towards -oo.
    The second argument must be non-negative.
*)

val to_string: t -> string
val of_string: string -> t

external to_int64: t -> int64 = "ml_z_to_int64"
(** Converts to a 64-bit integer. May raise [Overflow]. *)

external of_int64: int64 -> t = "ml_z_of_int64"
(** Converts from a 64-bit integer. *)

external to_int: t -> int = "ml_z_to_int"
(** Converts to a base integer. May raise an [Overflow]. *)

external of_int: int -> t = "ml_z_of_int" [@@ noalloc]
(** Converts from a base integer. *)

external equal: t -> t -> bool = "ml_z_equal" [@@ noalloc]
external compare: t -> t -> int = "ml_z_compare" [@@ noalloc]
