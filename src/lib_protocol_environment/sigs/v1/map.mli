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

(** Association tables over ordered types.

    This module implements applicative association tables, also known as
    finite maps or dictionaries, given a total ordering function
    over the keys.
    All operations over maps are purely applicative (no side-effects).
    The implementation uses balanced binary trees, and therefore searching
    and insertion take time logarithmic in the size of the map.

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

      module PairsMap = Map.Make(IntPairs)

      let m = PairsMap.(empty |> add (0,1) "hello" |> add (1,0) "world")
    ]}

    This creates a new module [PairsMap], with a new type ['a PairsMap.t]
    of maps from [int * int] to ['a]. In this example, [m] contains [string]
    values so its type is [string PairsMap.t].
*)

module type OrderedType =
sig
  type t
  (** The type of the map keys. *)

  val compare : t -> t -> int
  (** A total ordering function over the keys.
      This is a two-argument function [f] such that
      [f e1 e2] is zero if the keys [e1] and [e2] are equal,
      [f e1 e2] is strictly negative if [e1] is smaller than [e2],
      and [f e1 e2] is strictly positive if [e1] is greater than [e2].
      Example: a suitable ordering function is the generic structural
      comparison function {!Pervasives.compare}. *)
end
(** Input signature of the functor {!Map.Make}. *)

module Make (Ord : OrderedType) : S.MAP with type key = Ord.t
(** Functor building an implementation of the map structure
    given a totally ordered type. *)
