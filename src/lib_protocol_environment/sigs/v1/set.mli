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

(** Sets over ordered types.

    This module implements the set data structure, given a total ordering
    function over the set elements. All operations over sets
    are purely applicative (no side-effects).
    The implementation uses balanced binary trees, and is therefore
    reasonably efficient: insertion and membership take time
    logarithmic in the size of the set, for instance.

    The {!Make} functor constructs implementations for any type, given a
    [compare] function.
    For instance:
    {[
      module IntPairs =
      struct
        type t = int * int
        let compare (x0,y0) (x1,y1) =
          match Pervasives.compare x0 x1 with
            0 -> Pervasives.compare y0 y1
          | c -> c
      end

      module PairsSet = Set.Make(IntPairs)

      let m = PairsSet.(empty |> add (2,3) |> add (5,7) |> add (11,13))
    ]}

    This creates a new module [PairsSet], with a new type [PairsSet.t]
    of sets of [int * int].
*)

module type OrderedType =
sig
  type t
  (** The type of the set elements. *)

  val compare : t -> t -> int
  (** A total ordering function over the set elements.
      This is a two-argument function [f] such that
      [f e1 e2] is zero if the elements [e1] and [e2] are equal,
      [f e1 e2] is strictly negative if [e1] is smaller than [e2],
      and [f e1 e2] is strictly positive if [e1] is greater than [e2].
      Example: a suitable ordering function is the generic structural
      comparison function {!Pervasives.compare}. *)
end
(** Input signature of the functor {!Set.Make}. *)

module Make (Ord : OrderedType) : S.SET with type elt = Ord.t
(** Functor building an implementation of the set structure
    given a totally ordered type. *)
