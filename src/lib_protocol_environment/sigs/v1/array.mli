(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* TEZOS CHANGES

   * Import version 4.04.0
   * Remove unsafe functions
   * Remove deprecated functions
   * Remove set, copy, fill, blit, sort, stable_sort, fast_sort:
     mutability
   * iter, iteri, iter2: imperative
   * mem, memq: Use (=) and (==)

*)

(** Array operations. *)

external length : 'a array -> int = "%array_length"
(** Return the length (number of elements) of the given array. *)

external get : 'a array -> int -> 'a = "%array_safe_get"
(** [Array.get a n] returns the element number [n] of array [a].
    The first element has number 0.
    The last element has number [Array.length a - 1].
    You can also write [a.(n)] instead of [Array.get a n].

    Raise [Invalid_argument "index out of bounds"]
    if [n] is outside the range 0 to [(Array.length a - 1)]. *)

external make : int -> 'a -> 'a array = "caml_make_vect"
(** [Array.make n x] returns a fresh array of length [n],
    initialized with [x].
    All the elements of this new array are initially
    physically equal to [x] (in the sense of the [==] predicate).
    Consequently, if [x] is mutable, it is shared among all elements
    of the array, and modifying [x] through one of the array entries
    will modify all other entries at the same time.

    Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length].
    If the value of [x] is a floating-point number, then the maximum
    size is only [Sys.max_array_length / 2].*)

external create_float: int -> float array = "caml_make_float_vect"
(** [Array.create_float n] returns a fresh float array of length [n],
    with uninitialized data.
    @since 4.03 *)

val init : int -> (int -> 'a) -> 'a array
(** [Array.init n f] returns a fresh array of length [n],
    with element number [i] initialized to the result of [f i].
    In other terms, [Array.init n f] tabulates the results of [f]
    applied to the integers [0] to [n-1].

    Raise [Invalid_argument] if [n < 0] or [n > Sys.max_array_length].
    If the return type of [f] is [float], then the maximum
    size is only [Sys.max_array_length / 2].*)

val make_matrix : int -> int -> 'a -> 'a array array
(** [Array.make_matrix dimx dimy e] returns a two-dimensional array
    (an array of arrays) with first dimension [dimx] and
    second dimension [dimy]. All the elements of this new matrix
    are initially physically equal to [e].
    The element ([x,y]) of a matrix [m] is accessed
    with the notation [m.(x).(y)].

    Raise [Invalid_argument] if [dimx] or [dimy] is negative or
    greater than [Sys.max_array_length].
    If the value of [e] is a floating-point number, then the maximum
    size is only [Sys.max_array_length / 2]. *)

val append : 'a array -> 'a array -> 'a array
(** [Array.append v1 v2] returns a fresh array containing the
    concatenation of the arrays [v1] and [v2]. *)

val concat : 'a array list -> 'a array
(** Same as [Array.append], but concatenates a list of arrays. *)

val sub : 'a array -> int -> int -> 'a array
(** [Array.sub a start len] returns a fresh array of length [len],
    containing the elements number [start] to [start + len - 1]
    of array [a].

    Raise [Invalid_argument "Array.sub"] if [start] and [len] do not
    designate a valid subarray of [a]; that is, if
    [start < 0], or [len < 0], or [start + len > Array.length a]. *)

val to_list : 'a array -> 'a list
(** [Array.to_list a] returns the list of all the elements of [a]. *)

val of_list : 'a list -> 'a array
(** [Array.of_list l] returns a fresh array containing the elements
    of [l]. *)


(** {6 Iterators} *)


val map : ('a -> 'b) -> 'a array -> 'b array
(** [Array.map f a] applies function [f] to all the elements of [a],
    and builds an array with the results returned by [f]:
    [[| f a.(0); f a.(1); ...; f a.(Array.length a - 1) |]]. *)

val mapi : (int -> 'a -> 'b) -> 'a array -> 'b array
(** Same as {!Array.map}, but the
    function is applied to the index of the element as first argument,
    and the element itself as second argument. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
(** [Array.fold_left f x a] computes
    [f (... (f (f x a.(0)) a.(1)) ...) a.(n-1)],
    where [n] is the length of the array [a]. *)

val fold_right : ('b -> 'a -> 'a) -> 'b array -> 'a -> 'a
(** [Array.fold_right f a x] computes
    [f a.(0) (f a.(1) ( ... (f a.(n-1) x) ...))],
    where [n] is the length of the array [a]. *)


(** {6 Iterators on two arrays} *)


val map2 : ('a -> 'b -> 'c) -> 'a array -> 'b array -> 'c array
(** [Array.map2 f a b] applies function [f] to all the elements of [a]
    and [b], and builds an array with the results returned by [f]:
    [[| f a.(0) b.(0); ...; f a.(Array.length a - 1) b.(Array.length b - 1)|]].
    Raise [Invalid_argument] if the arrays are not the same size.
    @since 4.03.0 *)


(** {6 Array scanning} *)


val for_all : ('a -> bool) -> 'a array -> bool
(** [Array.for_all p [|a1; ...; an|]] checks if all elements of the array
    satisfy the predicate [p]. That is, it returns
    [(p a1) && (p a2) && ... && (p an)].
    @since 4.03.0 *)

val exists : ('a -> bool) -> 'a array -> bool
(** [Array.exists p [|a1; ...; an|]] checks if at least one element of
    the array satisfies the predicate [p]. That is, it returns
    [(p a1) || (p a2) || ... || (p an)].
    @since 4.03.0 *)


