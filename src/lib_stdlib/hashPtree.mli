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

(* Hash Consed Patricia Trees *)

module type Value = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

module type Bits = sig
  type t

  val lnot : t -> t
  val (land) : t -> t -> t
  val (lxor) : t -> t -> t
  val (lor) : t -> t -> t
  val (lsr) : t -> int -> t
  val (lsl) : t -> int -> t
  val pred : t -> t

  val less_than : t -> t -> bool

  val highest_bit : t -> t
  val equal : t -> t -> bool
  val hash : t -> int
  val zero : t
  val one : t

  val size : int
end

module type Size = sig
  val size : int
end

module Bits(S:Size) : sig
  include Bits
  val of_z : Z.t -> t
  val to_z : t -> Z.t
end

module type S = sig
  type key
  type value
  type mask
  type t

  val equal : t -> t -> bool

  val empty : t
  val singleton : key:key -> value:value -> mask:mask -> t

  (** [add combine ~key ~value ?mask t]
      Add a new key in the tree. If mask is specified, then we consider the whole
      subtree stemming from key.

      Assumes that forall x, [combine x x = x]
  *)
  val add : (value -> value -> value) -> key:key -> value:value ->
    ?mask:mask -> t -> t

  (** [remove key t] Remove the entire subtree speficied by the mask associated with
      key in the tree. Otherwise remove only the key *)
  val remove : key -> t -> t

  (** [remove_exact key t] Remove the largest subtree
      stemming from key. Otherwise remove only the key *)
  val remove_exact : key -> t -> t

  val remove_prefix : key -> mask -> t -> t

  (** [mem key t] return true if the entire subtree speficied by the mask associated with
      key is in the tree *)
  val mem : key -> t -> bool

  (** [mem_exact key t] return true if the largest subtree stemming from key is in the tree *)
  val mem_exact : key -> t -> bool

  val find : key -> t -> value option

  (** [let new_tree = replace_subtree ~replaced value tree]
      If replaced is a subtree of tree (for instance provided
      by Map_reduce.reduce)
      let n and m be the smallest integers such that for all
      keys part of replaced, n is smaller and n + 2^m is strictly larger.
      Then new_tree is the map such that for each key, n <= key < n + 2^m,
      [find key new_tree] is [Some value] *)
  val replace_subtree : replaced:t -> value -> t -> t

  val fold : (key -> mask -> value -> 'a -> 'a) -> t -> 'a -> 'a

  module type Map_Reduce = sig
    type result
    val default : result
    val map : t -> key -> value -> result
    val reduce : t -> result -> result -> result
  end
  module Map_Reduce(M:Map_Reduce) : sig
    (** run has a constant amortized complexity *)
    val run : t -> M.result

    (** [filter f t] assumes that the composition of [f] and [reduce]
        is monotonic i.e.
        for any [t], if [f (reduce t x y) = true] then [f x = true]
        and [f y = true].

        For efficiency reason, you should also ensure that
        if [f (reduce t x y) = false] then either [f x = false] or
        [f y = false].
        It is not required for correctness, but is needed to get a
        constant amortized complexity.
    *)
    val filter : (M.result -> bool) -> t -> t
  end

end

module Make_LE(V:Value) : S with type key = int and type value = V.t and type mask = int
module Make_BE(V:Value) : S with type key = int and type value = V.t and type mask = int
module Make_BE_gen(V:Value)(B:Bits) : S with type key = B.t and type value = V.t and type mask = B.t
module Make_BE_sized(V:Value)(S:Size) : S with type key = Bits(S).t and type value = V.t and type mask = Bits(S).t
