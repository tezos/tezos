(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Data queues similar to the [Pipe] module in Jane Street's [Async]
    library. They are implemented with [Queue]s, limited in size, and
    use lwt primitives for concurrent access. *)

type 'a t
(** Type of queues holding values of type ['a]. *)

val create : ?size:(int * ('a -> int)) -> unit -> 'a t
(** [create ~size:(max_size, compute_size)] is an empty queue that can
    hold max [size] bytes of data, using [compute_size] to compute the
    size of a datum. If want to count allocated bytes precisely, you
    need to add [push_overhead] to the result of[compute_size].
    When no [size] argument is provided, the queue is unbounded. *)

val push : 'a t -> 'a -> unit Lwt.t
(** [push q v] is a thread that blocks while [q] contains more
    than [size] elements, then adds [v] at the end of [q]. *)

val pop : 'a t -> 'a Lwt.t
(** [pop q] is a thread that blocks while [q] is empty, then
    removes and returns the first element in [q]. *)

val pop_with_timeout : unit Lwt.t -> 'a t -> 'a option Lwt.t
(** [pop t q] is a thread that blocks while [q] is empty, then
    removes and returns the first element [v] in [q] and
    to return [Some v], unless no message could be popped
    in [t] seconds, in which case it returns [None].
    As concurrent readers are allowed, [None] does not
    necessarily mean that no value has been pushed. *)

val pop_all : 'a t -> 'a list Lwt.t
(** [pop_all q] is a thread that blocks while [q] is empty, then
    removes and returns all the element in [q] (in the order they
    were inserted). *)

val pop_all_now : 'a t -> 'a list
(** [pop_all_now q] returns all the element in [q] (in the order they
    were inserted), or [[]] if [q] is empty. *)

val peek : 'a t -> 'a Lwt.t
(** [peek] is like [pop] except it does not removes the first
    element. *)

val peek_all : 'a t -> 'a list
(** [peek_all q] returns the elements in the [q] (oldest first),
    or [[]] if empty. *)

val values_available : 'a t -> unit Lwt.t
(** [values_available] is like [peek] but it ignores the value
    returned. *)

val push_now : 'a t -> 'a -> bool
(** [push_now q v] adds [v] at the ends of [q] immediately and returns
    [false] if [q] is currently full, [true] otherwise. *)

exception Full

val push_now_exn : 'a t -> 'a -> unit
(** [push_now q v] adds [v] at the ends of [q] immediately or
    raise [Full] if [q] is currently full. *)

val safe_push_now : 'a t -> 'a -> unit
(** [safe_push_now q v] may adds [v] at the ends of [q]. It does
    nothing if the queue is fulled or closed. *)

val pop_now : 'a t -> 'a option
(** [pop_now q] maybe removes and returns the first element in [q] if
    [q] contains at least one element. *)

exception Empty

val pop_now_exn : 'a t -> 'a
(** [pop_now_exn q] removes and returns the first element in [q] if
    [q] contains at least one element, or raise [Empty] otherwise. *)

val length : 'a t -> int
(** [length q] is the number of elements in [q]. *)

val is_empty : 'a t -> bool
(** [is_empty q] is [true] if [q] is empty, [false] otherwise. *)

val empty : 'a t -> unit Lwt.t
(** [empty q] returns when [q] becomes empty. *)

val iter : 'a t -> f:('a -> unit Lwt.t) -> unit Lwt.t
(** [iter q ~f] pops all elements of [q] and applies [f] on them. *)

exception Closed

val close : 'a t -> unit
(** [close q] the write end of [q]:

    * Future write attempts will fail with [Closed].
    * If there are reads blocked, they will unblock and fail with [Closed].
    * Future read attempts will drain the data until there is no data left.

    Thus, after a pipe has been closed, reads never block.
    Close is idempotent.
*)

val push_overhead: int
(** The allocated size in bytes when pushing in the queue. *)
