(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Protocol Environment - Arbitrary precision arithmetic. *)

type t
val zero: t
val one: t

val abs: t -> t
(** Absolute value. *)

val neg: t -> t
(** Unary negation. *)

val add: t -> t -> t
(** Addition. *)

val sub: t -> t -> t
(** Subtraction. *)

val mul: t -> t -> t
(** Multiplication. *)

val ediv_rem: t -> t -> (t * t)
(** Euclidean division and remainder.  [ediv_rem a b] returns a pair [(q, r)]
    such that [a = b * q + r] and [0 <= r < |b|].
    Raises [Division_by_zero] if [b = 0].
*)

val logand: t -> t -> t
(** Bitwise logical and. *)

val logor: t -> t -> t
(** Bitwise logical or. *)

val logxor: t -> t -> t
(** Bitwise logical exclusive or. *)

val lognot: t -> t
(** Bitwise logical negation.
    The identity [lognot a]=[-a-1] always hold.
*)

val shift_left: t -> int -> t
(** Shifts to the left.
    Equivalent to a multiplication by a power of 2.
    The second argument must be non-negative.
*)

val shift_right: t -> int -> t
(** Shifts to the right.
    This is an arithmetic shift,
    equivalent to a division by a power of 2 with rounding towards -oo.
    The second argument must be non-negative.
*)

val to_string: t -> string
val of_string: string -> t

val to_int64: t -> int64
(** Converts to a 64-bit integer. May raise [Overflow]. *)

val of_int64: int64 -> t
(** Converts from a 64-bit integer. *)

val to_int: t -> int
(** Converts to a base integer. May raise an [Overflow]. *)

val of_int: int -> t
(** Converts from a base integer. *)

val to_bits: ?pad_to:int -> t -> MBytes.t
val of_bits: MBytes.t -> t

val equal: t -> t -> bool
val compare: t -> t -> int

val numbits: t -> int
(** Returns the number of significant bits in the given number.
    If [x] is zero, [numbits x] returns 0.  Otherwise,
    [numbits x] returns a positive integer [n] such that
    [2^{n-1} <= |x| < 2^n].  Note that [numbits] is defined
    for negative arguments, and that [numbits (-x) = numbits x]. *)
