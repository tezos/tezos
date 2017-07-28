(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** The types for arbitraty precision integers in Michelson.
    The type variable ['t] is always [n] or [z],
    [n num] and [z num] are incompatible.

    This is internally a [Z.t].
    This module mostly adds signedness preservation guarantees. *)
type 't num

(** Flag for natural numbers. *)
and n = Natural_tag

(** Flag for relative numbers. *)
and z = Integer_tag

(** Natural zero. *)
val zero_n : n num

(** Relative zero. *)
val zero : z num

(** Compare two numbers as if they were *)
val compare : 'a num -> 'a num -> int

(** Conversion to an OCaml [string] in decimal notation. *)
val to_string : _ num -> string

(** Conversion from an OCaml [string].
    Returns [None] in case of an invalid notation.
    Supports [+] and [-] sign modifiers, and [0x], [0o] and [0b] base modifiers. *)
val of_string : string -> z num option

(** Conversion to an OCaml [int64], returns [None] on overflow. *)
val to_int64 : _ num -> int64 option

(** Conversion from an OCaml [int]. *)
val of_int64 : int64 -> z num

(** Conversion to an OCaml [int], returns [None] on overflow. *)
val to_int : _ num -> int option

(** Conversion from an OCaml [int64]. *)
val of_int : int -> z num

(** Addition between naturals. *)
val add_n : n num -> n num -> n num

(** Multiplication between naturals. *)
val mul_n : n num -> n num -> n num

(** Euclidean division between naturals.
    [ediv_n n d] returns [None] if divisor is zero,
    or [Some (q, r)] where [n = d * q + r] and [[0 <= r < d]] otherwise. *)
val ediv_n:  n num -> n num -> (n num * n num) option

(** Sign agnostic addition.
    Use {!add_n} when working with naturals to preserve the sign. *)
val add : _ num -> _ num -> z num

(** Sign agnostic subtraction.
    Use {!sub_n} when working with naturals to preserve the sign. *)
val sub : _ num -> _ num -> z num

(** Sign agnostic multiplication.
    Use {!mul_n} when working with naturals to preserve the sign. *)
val mul : _ num -> _ num -> z num

(** Sign agnostic euclidean division.
    [ediv n d] returns [None] if divisor is zero,
    or [Some (q, r)] where [n = d * q + r] and [[0 <= r < |d|]] otherwise.
    Use {!ediv_n} when working with naturals to preserve the sign. *)
val ediv:  _ num -> _ num -> (z num * n num) option

(** Compute the absolute value of a relative, turning it into a natural. *)
val abs : z num -> n num

(** Negates a number. *)
val neg : _ num -> z num

(** Turns a natural into a relative, not changing its value. *)
val int : n num -> z num

(** Reverses each bit in the representation of the number.
    Also applies to the sign. *)
val lognot : _ num -> z num


(** Shifts the natural to the left of a number of bits between 0 and 256.
    Returns [None] if the amount is too high. *)
val shift_left_n : n num -> n num -> n num option

(** Shifts the natural to the right of a number of bits between 0 and 256.
    Returns [None] if the amount is too high. *)
val shift_right_n : n num -> n num -> n num option

(** Shifts the number to the left of a number of bits between 0 and 256.
    Returns [None] if the amount is too high. *)
val shift_left : 'a num -> n num -> 'a num option

(** Shifts the number to the right of a number of bits between 0 and 256.
    Returns [None] if the amount is too high. *)
val shift_right : 'a num -> n num -> 'a num option

(** Applies a boolean or operation to each bit. *)
val logor : n num -> n num -> n num

(** Applies a boolean and operation to each bit. *)
val logand : n num -> n num -> n num

(** Applies a boolean xor operation to each bit. *)
val logxor : n num -> n num -> n num
