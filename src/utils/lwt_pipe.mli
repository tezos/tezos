(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Data queues similar to the [Pipe] module in Jane Street's [Async]
    library. They are implemented with [Queue]s, limited in size, and
    use lwt primitives for concurrent access. *)

type 'a t
(** Type of queues holding values of type ['a]. *)

val create : size:int -> 'a t
(** [create ~size] is an empty queue that can hold max [size]
    elements. *)

val push : 'a t -> 'a -> unit Lwt.t
(** [push q v] is a thread that blocks while [q] contains more
    than [size] elements, then adds [v] at the end of [q]. *)

val pop : 'a t -> 'a Lwt.t
(** [pop q] is a thread that blocks while [q] is empty, then
    removes and returns the first element in [q]. *)

val peek : 'a t -> 'a Lwt.t
(** [peek] is like [pop] except it does not removes the first
    element. *)

val values_available : 'a t -> unit Lwt.t
(** [values_available] is like [peek] but it ignores the value
    returned. *)

val push_now : 'a t -> 'a -> bool
(** [push_now q v] adds [v] at the ends of [q] immediately and returns
    [false] if [q] is currently full, [true] otherwise. *)

val pop_now : 'a t -> 'a option
(** [pop_now q] maybe removes and returns the first element in [q] if
    [q] contains at least one element. *)

val pop_now_exn : 'a t -> 'a
(** [pop_now_exn q] removes and returns the first element in [q] if
    [q] contains at least one element, or raise [Empty] otherwise. *)

val length : 'a t -> int
(** [length q] is the number of elements in [q]. *)

val is_empty : 'a t -> bool
(** [is_empty q] is [true] if [q] is empty, [false] otherwise. *)

