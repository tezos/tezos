(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* This module implements imperative sets as hash tables. 
   Operations like union, intersection or difference are not provided, 
   since there is no way to implement them easily (i.e. more easily than
   iterating over sets). *)

(*s Generic interface *)

type 'a t
(* The type of sets. Elements have type ['a]. *)

val create : int -> 'a t
(* [Hashset.create n] creates a new, empty set.
   For best results, [n] should be on the
   order of the expected number of elements that will be in
   the set.  The internal structure grows as needed, so [n] is just an
   initial guess. *)

val clear : 'a t -> unit
(* Empty a set. *)

val add : 'a t -> 'a -> unit
(* [Hashset.add s x] adds [x] into the set [s]. *)

val copy : 'a t -> 'a t
(* Return a copy of the given set. *)

val mem : 'a t -> 'a -> bool
(* [Hashset.mem s x] checks if [x] belongs to [s]. *)

val remove : 'a t -> 'a -> unit
(* [Hashset.remove s x] removes [x] from [s].
   It does nothing if [x] does not belong to [s]. *)

val cardinal : 'a t -> int
(* [Hashset.cardinal s] returns the cardinal of [s]. *)

val iter : ('a -> unit) -> 'a t -> unit
(* [Hashset.iter f s] applies [f] to all elements in [s]. *)

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(* [Hashset.fold f s init] computes
   [(f eN ... (f e1 init)...)],
   where [e1 ... eN] are the elements in [s].
   The order in which the elements are passed to [f] is unspecified. *)


(*s Functorial interface *)

module type HashedType =
  sig
    type t
      (* The type of the elements. *)
    val equal : t -> t -> bool
      (* The equality predicate used to compare elements. *)
    val hash : t -> int
      (* A hashing function on elements. It must be such that if two elements are
          equal according to [equal], then they have identical hash values
          as computed by [hash].
          Examples: suitable ([equal], [hash]) pairs for arbitrary element
          types include
          ([(=)], {!Hashset.hash}) for comparing objects by structure, and
          ([(==)], {!Hashset.hash}) for comparing objects by addresses
          (e.g. for mutable or cyclic keys). *)
   end

(* The input signature of the functor {!Hashset.Make}. *)

module type S =
  sig
    type elt
    type t
    val create : int -> t
    val clear : t -> unit
    val copy : t -> t
    val add : t -> elt -> unit
    val remove : t -> elt -> unit
    val mem : t -> elt -> bool
    val cardinal : t -> int
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  end
(* The output signature of the functor {!Hashset.Make}. *)

module Make (H : HashedType) : S with type elt = H.t
(* Functor building an implementation of the hashtable structure.
    The functor [Hashset.Make] returns a structure containing
    a type [elt] of elements and a type [t] of hash sets.
    The operations perform similarly to those of the generic
    interface, but use the hashing and equality functions
    specified in the functor argument [H] instead of generic
    equality and hashing. *)

