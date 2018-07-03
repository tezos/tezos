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

(** [remove nb list] remove the first [nb] elements from the list [list]. *)
val remove: int -> 'a list -> 'a list

(** [repeat n x] is a list of [n] [x]'s **)
val repeat: int -> 'a -> 'a list

(** [shift (hd :: tl)] computes [tl @ [hd]] *)
val shift : 'a list -> 'a list

(** [product a b] computes the cartesian product of two lists [a] and [b]. *)
val product : 'a list -> 'b list -> ('a * 'b) list

(** [take_n n l] returns the [n] first elements of [l]. When [compare]
    is provided, it returns the [n] greatest element of [l]. *)
val take_n: ?compare:('a -> 'a -> int) -> int -> 'a list -> 'a list

val split_n: int -> 'a list -> 'a list * 'a list

(** Bounded sequence: keep only the [n] greatest elements. *)
module Bounded(E: Set.OrderedType) : sig
  type t
  val create: int -> t
  val insert: E.t -> t -> unit
  val get: t -> E.t list
end

(** [select n l] is ([n]th element of [l], [l] without that element) **)
val select: int -> 'a list -> 'a * 'a list


(** [filter_map f l] is [[y for x in l where (f x) = Some y]] **)
val filter_map: ('a -> 'b option) -> 'a list -> 'b list

(** [rev_sub l n] is [List.rev l] capped to max [n] elements *)
val rev_sub : 'a list -> int -> 'a list

(** [sub l n] is [l] capped to max [n] elements *)
val sub: 'a list -> int -> 'a list

(** Like [List.hd], but [Some hd] or [None] if empty **)
val hd_opt: 'a list -> 'a option

(** Last elt of list, or raise Not_found if empty **)
val last_exn: 'a list -> 'a

(** [merge_filter2 ~compare ~f l1 l2] merges two lists ordered by [compare]
    and whose items can be merged with [f]. Item is discarded or kept whether
    [f] returns [Some] or [None] *)
val merge_filter2 :
  ?finalize:('a list -> 'a list) ->
  ?compare:('a -> 'a -> int) ->
  ?f:('a option -> 'a option -> 'a option) ->
  'a list -> 'a list ->
  'a list

(** [merge2 ~compare ~f l1 l2] merges two lists ordered by [compare] and
    whose items can be merged with [f] *)
val merge2 :
  ?finalize:('a list -> 'a list) ->
  ?compare:('a -> 'a -> int) ->
  ?f:('a -> 'a -> 'a) ->
  'a list -> 'a list ->
  'a list

