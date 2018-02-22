(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Imperative Ring Buffer *)

(** An imperative ring buffer: a mutable structure that holds at most
    a fixed number of values of a same type. Values are never removed,
    once the limit is reached, adding a value replaces the oldest one
    in the ring buffer.  *)
exception Empty

type 'a t

(** Allocates a ring buffer for a given number of values. *)
val create : int -> 'a t

(** Adds a value, dropping the oldest present one if full. *)
val add : 'a t -> 'a -> unit

(** Same as {!add}, but returns the dropped value if any. *)
val add_and_return_erased : 'a t -> 'a -> 'a option

(** Adds the values of a list, in order. *)
val add_list : 'a t -> 'a list -> unit

(** Removes all values in the ring buffer. *)
val clear : 'a t -> unit

(** Retrieves the most recent value, or [None] when empty. *)
val last : 'a t -> 'a option

(** Same as {!last}, but raises {!Empty} when empty. *)
val last_exn : 'a t -> 'a

(** Iterates over the elements, oldest to newest. *)
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b

(** Retrieves the elements as a list, oldest first.. *)
val elements : 'a t -> 'a list

(** Ring Buffer Table *)
module type TABLE = sig
  type t
  type v

  (** [create size] inizialize an empty ring *)
  val create : int -> t

  (** [retest t] remore all bindings from the current ring *)
  val clear : t -> unit

  (** [add t v] add a value to the ring. If the ring already contains size elements,
      the first element is removed and [v] is added. *)
  val add : t -> v -> unit

  (** [mem t v] check if v is in the ring. O(1) *)
  val mem : t -> v -> bool

  (** [remove t v] remove one element from the table *)
  val remove : t -> v -> unit

  (** [elements t] return the list of elements currently in the ring *)
  val elements : t -> v list

end

module MakeTable (V: Hashtbl.HashedType) : TABLE with type v = V.t
