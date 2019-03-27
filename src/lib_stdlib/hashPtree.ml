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

module Ptree_sig = struct
  module type Value = sig

    type t
    val equal : t -> t -> bool
    val hash : t -> int

  end

  type prefix_order =
    | Equal
    | Shorter
    | Longer
    | Different

  module type Prefix = sig
    type key    (* bit sequence *)
    type prefix (* prefix of a bit sequence *)
    type mask   (* integer length of a bit sequence *)

    val equal_key : key -> key -> bool
    val equal_mask : mask -> mask -> bool
    val equal_prefix : prefix -> prefix -> bool

    val hash_key : key -> int
    val hash_mask : mask -> int
    val hash_prefix : prefix -> int

    val full_length_mask : mask

    val strictly_shorter_mask : mask -> mask -> bool

    val key_prefix : key -> prefix
    (* Full length prefix *)
    val prefix_key : prefix -> mask -> key
    (* Some key matching the prefix with the given mask *)

    val match_prefix : key:key -> prefix:prefix -> mask:mask -> bool
    (* Does the prefix of length [mask] of [key] equals to [prefix] *)

    val select_bit : prefix:prefix -> mask:mask -> bool
    (* Get the bit of [prefix] at position [mask] assumes that [mask] is
       less than the length of prefix *)

    val common_mask : prefix -> prefix -> mask
    (* The length of the common part of given prefixes *)

    val apply_mask : prefix -> mask -> prefix
    (* Cut the prefix to the given length *)

    val compare_prefix : mask -> prefix -> mask -> prefix -> prefix_order
    (* [compare_prefix m1 p1 m2 p2]:
       let p1' (resp p2') be the sub-prefix of length m1 of p1 (resp m2 of p2)
       The result is
         Equal if p1' equal p2'
         Shorter if p1' is a prefix of p2'
         Longer if p2' is a prefix of p1'
         Different if those not ordered
    *)
  end

  module type S = sig

    type key
    type prefix
    type mask
    type value

    type not_empty = TNot_empty
    type empty     = TEmpty

    type _ t = private
      | Leaf : {
          mutable id: int; (* Mutable to get a good sharing semantics *)
          mask : mask;
          key : key;
          value : value;
        } -> not_empty t
      | Node : {
          mutable id : int;
          mask : mask;
          prefix : prefix;
          true_ : not_empty t;
          false_ : not_empty t;
        } -> not_empty t
      | Empty : empty t

    val leaf : key:key -> mask:mask -> value -> not_empty t
    val node : prefix:prefix -> mask:mask -> true_:not_empty t -> false_:not_empty t -> not_empty t
    val empty : empty t

    val equal : not_empty t -> not_empty t -> bool

    val fast_partial_equal : not_empty t -> not_empty t -> bool
    (* if [fast_partial_equal x y] is true, then [equal x y] is true,
       but if fast_partial_equal returns false, nothing can be
       asserted. *)

    val id : not_empty t -> int
  end
end

module Shared_tree : sig

  module Hash_consed_tree(P:Ptree_sig.Prefix)(V:Ptree_sig.Value) : Ptree_sig.S
    with type value = V.t
     and type key = P.key
     and type prefix = P.prefix
     and type mask = P.mask

  module Simple_tree(P:Ptree_sig.Prefix)(V:sig type t val equal : t -> t -> bool  end) : Ptree_sig.S
    with type value = V.t
     and type key = P.key
     and type prefix = P.prefix
     and type mask = P.mask

end = struct
  open Ptree_sig

(*
  type int2 = { mutable i1 : int; mutable i2 : int }
  let h2 = { i1 = 0; i2 = 0 }
  let hash2int x1 x2 =
    h2.i1 <- x1; h2.i2 <- x2;
    Hashtbl.hash h2
*)
  type int3 = { mutable i1 : int; mutable i2 : int; mutable i3 : int }
  let h3 = { i1 = 0; i2 = 0; i3 = 0 }
  let hash3int x1 x2 x3 =
    h3.i1 <- x1; h3.i2 <- x2; h3.i3 <- x3;
    Hashtbl.hash h3

  type int4 = { mutable i1 : int; mutable i2 : int; mutable i3 : int; mutable i4 : int }
  let h4 = { i1 = 0; i2 = 0; i3 = 0; i4 = 0 }
  let hash4int x1 x2 x3 x4 =
    h4.i1 <- x1; h4.i2 <- x2; h4.i3 <- x3; h4.i4 <- x4;
    Hashtbl.hash h4


  module Hash_consed_tree(P:Prefix)(V:Value) : S
    with type value = V.t
     and type key = P.key
     and type prefix = P.prefix
     and type mask = P.mask
  = struct

    type key = P.key
    type mask = P.mask
    type prefix = P.prefix
    type value = V.t

    type not_empty = TNot_empty
    type empty     = TEmpty

    type _ t =
      | Leaf : {
          mutable id: int; (* Mutable to get a good sharing semantics *)
          mask : mask;
          key : key;
          value : value;
        } -> not_empty t
      | Node : {
          mutable id : int;
          mask : mask;
          prefix : prefix;
          true_ : not_empty t;
          false_ : not_empty t;
        } -> not_empty t
      | Empty : empty t

    let id : not_empty t -> int = function
      | Leaf { id ; _ } -> id
      | Node { id ; _ } -> id

    let set_id (n : not_empty t) id = match n with
      | Leaf r -> r.id <- id
      | Node r -> r.id <- id

    (*let mask : not_empty t -> mask = function
      | Leaf { mask ; _ } -> mask
      | Node { mask ; _ } -> mask
    *)
    (* let prefix_table = WeakPrefixTbl.create 20 *)

    module Tree :
      Hashtbl.HashedType with type t = not_empty t
    = struct

      type nonrec t = not_empty t

      let equal (t1 : t) (t2 : t) = match t1, t2 with
        | Leaf _, Node _ | Node _, Leaf _-> false
        | Leaf { key = p1; value = v1; mask = m1 ; _ },
          Leaf { key = p2; value = v2; mask = m2 ; _ } ->
            P.equal_key p1 p2 && P.equal_mask m1 m2 && V.equal v1 v2
        | Node { prefix = p1; mask = m1; true_ = t1; false_ = f1 ; _ },
          Node { prefix = p2; mask = m2; true_ = t2; false_ = f2 ; _ } ->
            (* Assumes that only the head can be unshared: this means
               that structural equality implies physical one on children *)
            P.equal_prefix p1 p2 &&
            P.equal_mask m1 m2 && t1 == t2 && f1 == f2

      let hash : t -> int = function
        | Leaf { key; value; mask ; _ } ->
            hash3int (P.hash_key key) (V.hash value) (P.hash_mask mask)
        | Node { mask; prefix; true_; false_ ; _ } ->
            hash4int
              (P.hash_mask mask) (P.hash_prefix prefix)
              (id true_) (id false_)

    end

    module WeakTreeTbl = Weak.Make(Tree)

    (* Or move that to a state ? *)
    let weak_tree_tbl = WeakTreeTbl.create 10

    let next =
      let r = ref 0 in
      fun () -> incr r; !r

    let leaf ~key ~mask value =
      let l = Leaf { id = 0; key; value; mask } in
      match WeakTreeTbl.find_opt weak_tree_tbl l with
      | None ->
          set_id l (next ());
          WeakTreeTbl.add weak_tree_tbl l;
          l
      | Some l -> l

    let node ~prefix ~mask ~true_ ~false_ =
      let l = Node { id = 0; mask; prefix; true_; false_ } in
      match WeakTreeTbl.find_opt weak_tree_tbl l with
      | None ->
          set_id l (next ());
          WeakTreeTbl.add weak_tree_tbl l;
          l
      | Some l -> l

    let empty = Empty

    let equal (x:not_empty t) (y:not_empty t) =
      x == y

    let fast_partial_equal = equal

  end [@@inline]

  module Simple_tree(P:Ptree_sig.Prefix)(V:sig type t val equal : t -> t -> bool end) : S
    with type value = V.t
     and type key = P.key
     and type prefix = P.prefix
     and type mask = P.mask
  = struct

    type key = P.key
    type mask = P.mask
    type prefix = P.prefix
    type value = V.t

    type not_empty = TNot_empty
    type empty     = TEmpty

    type _ t =
      | Leaf : {
          mutable id: int; (* Mutable to get a good sharing semantics *)
          mask : mask;
          key : key;
          value : value;
        } -> not_empty t
      | Node : {
          mutable id : int;
          mask : mask;
          prefix : prefix;
          true_ : not_empty t;
          false_ : not_empty t;
        } -> not_empty t
      | Empty : empty t

    let id : not_empty t -> int = function
      | Leaf { id ; _ } -> id
      | Node { id ; _ } -> id

    (*let set_id (n : not_empty t) id = match n with
      | Leaf r -> r.id <- id
      | Node r -> r.id <- id

      let mask : not_empty t -> mask = function
      | Leaf { mask ; _ } -> mask
      | Node { mask ; _ } -> mask
    *)
    let leaf ~key ~mask value =
      Leaf { id = 0; key; value; mask }

    let node ~prefix ~mask ~true_ ~false_ =
      Node { id = 0; mask; prefix; true_; false_ }

    let empty = Empty

    let rec equal_not_empty (x:not_empty t) (y:not_empty t) =
      x == y ||
      match x, y with
      | Leaf l1, Leaf l2 ->
          P.equal_key l1.key l2.key &&
          V.equal l1.value l2.value
      | Node n1, Node n2 ->
          P.equal_prefix n1.prefix n2.prefix &&
          P.equal_mask n1.mask n2.mask &&
          equal_not_empty n1.true_ n2.true_ &&
          equal_not_empty n1.false_ n2.false_
      | Node _, Leaf _ | Leaf _, Node _ -> false

    let equal : type a b. a t -> b t -> bool = fun x y ->
      match x, y with
      | Empty, Empty -> true
      | Leaf _, Leaf _ ->
          equal_not_empty x y
      | Node _, Node _ ->
          equal_not_empty x y
      | _, _ ->
          false

    let fast_partial_equal (x:not_empty t) (y:not_empty t) =
      x == y

  end [@@inline]

end

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

module Bits(S:Size) = struct
  type t = Z.t
  let size = S.size
  let higher_bit = Z.shift_left Z.one size
  let mask = Z.pred higher_bit

  let mark n = Z.logor higher_bit n
  let unmark n = Z.logxor higher_bit n

  let one = mark Z.one
  let zero = higher_bit
  let hash = Z.hash
  let equal = Z.equal
  let less_than = Z.lt

  let highest_bit_unmarked n =
    if Z.equal Z.zero n then
      Z.zero
    else
      Z.(Z.one lsl (Pervasives.pred (numbits n)))

  let highest_bit n = mark (highest_bit_unmarked (unmark n))

  let lnot x = Z.logor (Z.lognot x) higher_bit
  let (land) = Z.logand
  let (lxor) a b = Z.logor (Z.logxor a b) higher_bit
  let (lor) = Z.logor
  let (lsr) a n =
    Z.logor
      (Z.shift_right_trunc (Z.logxor a higher_bit) n)
      higher_bit
  let (lsl) a n =
    Z.logor
      (Z.logand (Z.shift_left a n) mask)
      higher_bit

  let pred = Z.pred

  let of_z n = mark n
  let to_z n = unmark n
end

module BE_gen_prefix(Bits:Bits) : Ptree_sig.Prefix
  with type key = Bits.t
   and type prefix = Bits.t
   and type mask = Bits.t
= struct
  type key = Bits.t
  type mask = Bits.t (* Only a single bit set *)
  type prefix = Bits.t

  let equal_key = Bits.equal
  let equal_mask = Bits.equal
  let equal_prefix = Bits.equal

  let hash_key x = Bits.hash x
  let hash_mask x = Bits.hash x
  let hash_prefix x = Bits.hash x

  open Bits

  let full_length_mask = Bits.one

  let strictly_shorter_mask (m1:mask) m2 =
    Bits.less_than m2 m1

  let select_bit ~prefix ~mask =
    not (Bits.equal (prefix land mask) Bits.zero)

  let apply_mask prefix mask =
    prefix land (lnot (pred mask))

  let match_prefix ~key ~prefix ~mask =
    equal_prefix (apply_mask key mask) prefix

  let common_mask p0 p1 =
    (Bits.highest_bit (* [@inlined] *)) (p0 lxor p1)

  let key_prefix x = x
  let prefix_key p _m = p

  let smaller_set_mask m1 m2 =
    (lnot (pred m1))
    land
    (lnot (pred m2))

  let compare_prefix m1 p1 m2 p2 =
    let min_mask = smaller_set_mask m1 m2 in
    let applied_p1 = p1 land min_mask in
    let applied_p2 = p2 land min_mask in
    if applied_p1 = applied_p2 then
      if m1 > m2 then Ptree_sig.Shorter
      else if m1 < m2 then Ptree_sig.Longer
      else Ptree_sig.Equal
    else
      Ptree_sig.Different
end


module LE_prefix : Ptree_sig.Prefix
  with type key = int
   and type prefix = int
   and type mask = int
= struct
  type key = int
  type mask = int (* Only a single bit set *)
  type prefix = int

  let equal_key = (==)
  let equal_mask = (==)
  let equal_prefix = (==)

  let hash_key x = x
  let hash_mask x = x
  let hash_prefix x = x

  let full_length_mask = (-1) lxor ((-1) lsr 1)

  let strictly_shorter_mask (m1:mask) m2 =
    m1 < m2

  let select_bit ~prefix ~mask = (prefix land mask) != 0

  let apply_mask prefix mask = prefix land (mask-1)
  let match_prefix ~key ~prefix ~mask =
    (apply_mask key mask) == prefix

  let lowest_bit x = x land (-x)
  let common_mask p0 p1 = lowest_bit (p0 lxor p1)

  let key_prefix x = x
  let prefix_key p _m = p

  let smaller_set_mask m1 m2 = (m1-1) land (m2-1)

  let compare_prefix m1 p1 m2 p2 =
    let min_mask = smaller_set_mask m1 m2 in
    let applied_p1 = p1 land min_mask in
    let applied_p2 = p2 land min_mask in
    if applied_p1 = applied_p2 then
      if m1 < m2 then Ptree_sig.Shorter
      else if m1 > m2 then Ptree_sig.Longer
      else Ptree_sig.Equal
    else
      Ptree_sig.Different
end

module BE_prefix : Ptree_sig.Prefix
  with type key = int
   and type prefix = int
   and type mask = int
= struct
  type key = int
  type mask = int (* Only a single bit set *)
  type prefix = int

  let equal_key = (==)
  let equal_mask = (==)
  let equal_prefix = (==)

  let hash_key x = x
  let hash_mask x = x
  let hash_prefix x = x

  let full_length_mask = 1

  let strictly_shorter_mask (m1:mask) m2 =
    m1 > m2

  let select_bit ~prefix ~mask = (prefix land mask) != 0

  module Nativeint_infix = struct
    let (lor) = Nativeint.logor
    (*let (lsl) = Nativeint.shift_left*)
    let (lsr) = Nativeint.shift_right_logical
    (*let (asr) = Nativeint.shift_right*)
    let (land) = Nativeint.logand
    let (lnot) = Nativeint.lognot
    let (lxor) = Nativeint.logxor
    let (-) = Nativeint.sub
  end

  let apply_mask prefix mask =
    let open Nativeint_infix in
    let prefix = Nativeint.of_int prefix in
    let mask = Nativeint.of_int mask in
    Nativeint.to_int
      (
        prefix land
        (lnot (mask - 1n))
      )

  let match_prefix ~key ~prefix ~mask =
    (apply_mask key mask) == prefix

  let highest_bit x =
    Nativeint_infix.(
      let x = x lor (x lsr 1) in
      let x = x lor (x lsr 2) in
      let x = x lor (x lsr 4) in
      let x = x lor (x lsr 8) in
      let x = x lor (x lsr 16) in
      let x =
        if Sys.word_size > 32 then
          x lor (x lsr 32)
        else
          x
      in
      Nativeint.to_int (x - (x lsr 1))
    )

  let common_mask p0 p1 =
    let open Nativeint_infix in
    let p0 = Nativeint.of_int p0 in
    let p1 = Nativeint.of_int p1 in
    highest_bit (p0 lxor p1)

  let key_prefix x = x
  let prefix_key p _m = p

  let smaller_set_mask m1 m2 =
    let open Nativeint_infix in
    (lnot (m1 - 1n))
    land
    (lnot (m2 - 1n))

  let compare_prefix m1 p1 m2 p2 =
    let open Nativeint_infix in
    let m1 = Nativeint.of_int m1 in
    let m2 = Nativeint.of_int m2 in
    let p1 = Nativeint.of_int p1 in
    let p2 = Nativeint.of_int p2 in
    let min_mask = smaller_set_mask m1 m2 in
    let applied_p1 = p1 land min_mask in
    let applied_p2 = p2 land min_mask in
    if applied_p1 = applied_p2 then
      if m1 > m2 then Ptree_sig.Shorter
      else if m1 < m2 then Ptree_sig.Longer
      else Ptree_sig.Equal
    else
      Ptree_sig.Different
end

module Make(P:Ptree_sig.Prefix)(V:Value) = struct

  module T = Shared_tree.Hash_consed_tree(P)(V)

  type t = E : 'a T.t -> t [@@ocaml.unboxed]
  type key = T.key
  type value = T.value
  type mask = T.mask
(*
  let (=) = `Do_not_use_polymorphic_equality
  let (<=) = `Do_not_use_polymorphic_comparison
  let (>=) = `Do_not_use_polymorphic_comparison
  let (<) = `Do_not_use_polymorphic_comparison
  let (>) = `Do_not_use_polymorphic_comparison
  let compare = `Do_not_use_polymorphic_comparison
   *)
  let equal (E t1) (E t2) =
    match t1, t2 with
    | T.Empty, T.Empty -> true
    | T.Empty, T.Leaf _ -> false
    | T.Empty, T.Node _ -> false
    | T.Leaf _, T.Empty -> false
    | T.Node _, T.Empty -> false
    | T.Node _, T.Node _ -> T.equal t1 t2
    | T.Node _, T.Leaf _ -> T.equal t1 t2
    | T.Leaf _, T.Node _ -> T.equal t1 t2
    | T.Leaf _, T.Leaf _ -> T.equal t1 t2

  let select_key_bit k m =
    P.select_bit ~prefix:(P.key_prefix k) ~mask:m

  let matching_key k1 k2 mask =
    let p1 = P.apply_mask (P.key_prefix k1) mask in
    let p2 = P.apply_mask (P.key_prefix k2) mask in
    P.equal_prefix p1 p2

  let rec mem : type k. key -> k T.t -> bool = fun k -> function
    | T.Empty ->
        false
    | T.Leaf { key; mask ; _} ->
        matching_key key k mask
    | T.Node { prefix = _; mask; true_; false_ ; _ } ->
        mem k
          (if select_key_bit k mask then true_ else false_)

  let rec mem_exact : type k. key -> k T.t -> bool = fun k -> function
    | T.Empty ->
        false
    | T.Leaf { key; mask ; _ } ->
        P.equal_key k key && P.equal_mask mask P.full_length_mask
    | T.Node { prefix = _; mask; true_; false_ ; _ } ->
        mem_exact k
          (if select_key_bit k mask then true_ else false_)

  let rec find_ne k (t: T.not_empty T.t) = match t with
    | T.Leaf { key; value; mask ; _ } ->
        if matching_key key k mask then
          Some value
        else
          None
    | T.Node { prefix = _; mask; true_; false_ ; _ } ->
        find_ne k
          (if select_key_bit k mask then true_ else false_)

  let find : type k. key -> k T.t -> value option = fun k -> function
    | T.Empty ->
        None
    | T.Leaf _ as t ->
        find_ne k t
    | T.Node _ as t ->
        find_ne k t

  let singleton ~key ~value ~mask =
    T.leaf ~key value ~mask

  let join ~mask p0 t0 p1 t1 =
    (* assumes p0 <> p1 *)
    let c_mask = P.common_mask p0 p1 in
    let mask = if P.strictly_shorter_mask c_mask mask then c_mask else mask in
    let prefix = P.apply_mask p1 mask in
    let true_, false_ =
      if P.select_bit ~prefix:p0 ~mask then
        t0, t1
      else
        t1, t0
    in
    T.node ~prefix ~mask ~true_ ~false_

  let rebuild_ne_branch node prefix mask ~node_true ~node_false ~true_ ~false_ =
    if T.fast_partial_equal node_true true_ &&
       T.fast_partial_equal node_false false_ then
      node
    else
      T.node ~prefix ~mask ~true_ ~false_

  let rec add_ne combine ~key ~value ?(mask=P.full_length_mask) t =
    match t with
    | T.Leaf leaf ->
        if P.equal_key key leaf.key && P.equal_mask leaf.mask P.full_length_mask then
          if value == leaf.value then
            t
          else
            T.leaf ~key (combine value leaf.value) ~mask
        else if
          P.strictly_shorter_mask leaf.mask mask &&
          P.match_prefix ~key ~prefix:(P.key_prefix leaf.key) ~mask:leaf.mask then
          (* The previous leaf shadows the new one: no modification *)
          t
        else if
          P.strictly_shorter_mask mask leaf.mask &&
          P.match_prefix ~key:leaf.key ~prefix:(P.key_prefix key) ~mask then
          (* The new leaf shadows the previous one: replace *)
          T.leaf ~key (combine value leaf.value) ~mask
        else
          join ~mask
            (P.key_prefix key) (T.leaf ~key value ~mask)
            (P.key_prefix leaf.key) t
    | T.Node node ->
        if P.match_prefix ~key ~prefix:node.prefix ~mask:node.mask then
          let true_, false_ =
            if select_key_bit key node.mask then
              add_ne combine ~key ~value ~mask node.true_, node.false_
            else
              node.true_, add_ne combine ~key ~value ~mask node.false_
          in
          rebuild_ne_branch t node.prefix node.mask
            ~node_false:node.false_ ~node_true:node.true_
            ~true_ ~false_
        else
          join ~mask
            (P.key_prefix key) (T.leaf ~key value ~mask)
            node.prefix t

  let add : type k.
    (value -> value -> value) -> key:key -> value:value ->
    ?mask:P.mask -> k T.t ->
    T.not_empty T.t = fun combine ~key ~value ?(mask=P.full_length_mask) ->
    function
    | T.Empty ->
        singleton ~key ~value ~mask

    (* Should be merged by matcher *)
    | T.Leaf _ as t ->
        add_ne combine ~key ~value ~mask t
    | T.Node _ as t ->
        add_ne combine ~key ~value ~mask t

  let empty = E T.empty

  let rebuild_branch
      node prefix mask ~node_true ~node_false
      ~true_:(E true_) ~false_:(E false_) =
    match true_, false_ with
    | T.Empty, T.Empty ->
        empty
    | T.Empty, t ->
        E t
    | t, T.Empty ->
        E t
    | T.Leaf _ as true_, (T.Leaf _ as false_) ->
        E (rebuild_ne_branch node prefix mask ~node_true ~node_false ~true_ ~false_)
    | T.Leaf _ as true_, (T.Node _ as false_) ->
        E (rebuild_ne_branch node prefix mask ~node_true ~node_false ~true_ ~false_)
    | T.Node _ as true_, (T.Leaf _ as false_) ->
        E (rebuild_ne_branch node prefix mask ~node_true ~node_false ~true_ ~false_)
    | T.Node _ as true_, (T.Node _ as false_) ->
        E (rebuild_ne_branch node prefix mask ~node_true ~node_false ~true_ ~false_)

  let rec remove_ne : key -> T.not_empty T.t -> t =
    fun key t ->
      match t with
      | T.Leaf leaf ->
          if matching_key leaf.key key leaf.mask then
            E T.empty
          else
            E t
      | T.Node node ->
          if P.match_prefix ~key ~prefix:node.prefix ~mask:node.mask then
            let true_, false_ =
              if select_key_bit key node.mask then
                remove_ne key node.true_, E node.false_
              else
                E node.true_, remove_ne key node.false_
            in
            rebuild_branch t node.prefix node.mask
              ~node_true:node.true_ ~node_false:node.false_ ~true_ ~false_
          else
            E t

  let remove key (E t) =
    match t with
    | T.Empty ->
        empty
    | T.Leaf _ as t ->
        remove_ne key t
    | T.Node _ as t ->
        remove_ne key t

  let rec remove_prefix_ne : key -> mask -> T.not_empty T.t -> t =
    fun key mask t ->
      match t with
      | T.Leaf leaf ->
          if matching_key key leaf.key mask then
            E T.empty
          else
            E t
      | T.Node node ->
          match P.compare_prefix mask (P.key_prefix key) node.mask node.prefix with
          | Different ->
              E t
          | Equal ->
              E T.empty
          | Shorter ->
              E T.empty
          | Longer ->
              let true_, false_ =
                if select_key_bit key node.mask then
                  remove_prefix_ne key mask node.true_, E node.false_
                else
                  E node.true_, remove_prefix_ne key mask node.false_
              in
              rebuild_branch t node.prefix node.mask
                ~node_true:node.true_ ~node_false:node.false_ ~true_ ~false_

  let remove_prefix key mask (E t) =
    match t with
    | T.Empty ->
        empty
    | T.Leaf _ as t ->
        remove_prefix_ne key mask t
    | T.Node _ as t ->
        remove_prefix_ne key mask t

  let rec remove_ne_exact : key -> T.not_empty T.t -> t =
    fun key t ->
      match t with
      | T.Leaf leaf ->
          if P.equal_key leaf.key key && P.equal_mask leaf.mask P.full_length_mask then
            E T.empty
          else
            E t
      | T.Node node ->
          if P.match_prefix ~key ~prefix:node.prefix ~mask:node.mask then
            let true_, false_ =
              if select_key_bit key node.mask then
                remove_ne_exact key node.true_, E node.false_
              else
                E node.true_, remove_ne_exact key node.false_
            in
            rebuild_branch t node.prefix node.mask
              ~node_true:node.true_ ~node_false:node.false_ ~true_ ~false_
          else
            E t

  let remove_exact key (E t) =
    match t with
    | T.Empty ->
        empty
    | T.Leaf _ as t ->
        remove_ne_exact key t
    | T.Node _ as t ->
        remove_ne_exact key t

  let rec replace_subtree_ne ~key ~id value t =
    match t with
    | T.Leaf leaf ->
        if leaf.id == id then
          T.leaf ~key:leaf.key ~mask:leaf.mask value
        else
          t
    | T.Node node ->
        if node.id == id then
          T.leaf ~key:(P.prefix_key node.prefix node.mask) ~mask:node.mask value
        else
        if P.match_prefix ~key ~prefix:node.prefix ~mask:node.mask then
          let true_, false_ =
            if select_key_bit key node.mask then
              replace_subtree_ne ~key ~id value node.true_, node.false_
            else
              node.true_, replace_subtree_ne ~key ~id value node.false_
          in
          rebuild_ne_branch t node.prefix node.mask
            ~node_true:node.true_ ~node_false:node.false_ ~true_ ~false_
        else
          t

  let replace_subtree ~replaced:(E replaced) value t =
    let replace_subtree_aux ~key ~id value (E t) =
      match t with
      | T.Empty ->
          empty
      | T.Leaf _ as t ->
          E (replace_subtree_ne ~key ~id value t)
      | T.Node _ as t ->
          E (replace_subtree_ne ~key ~id value t)
    in
    match replaced with
    | T.Empty ->
        t
    | T.Leaf leaf ->
        replace_subtree_aux ~key:leaf.key ~id:leaf.id value t
    | T.Node node ->
        replace_subtree_aux
          ~key:(P.prefix_key node.prefix node.mask)
          ~id:node.id value t


  let rec fold_ne : (key -> mask -> value -> 'a -> 'a) -> T.not_empty T.t -> 'a -> 'a =
    fun f t acc ->
      match t with
      | T.Leaf {key; mask; value; _} ->
          f key mask value acc
      | T.Node node ->
          let acc = fold_ne f node.false_ acc in
          fold_ne f node.true_ acc

  let fold f (E t) acc =
    match t with
    | T.Empty ->
        acc
    | T.Leaf _ as t ->
        fold_ne f t acc
    | T.Node _ as t ->
        fold_ne f t acc

  module T_id = struct
    type t = T.not_empty T.t

    let hash = T.id
    let equal t1 t2 = T.id t1 == T.id t2
  end
  module Map_cache = Ephemeron.K1.Make(T_id)

  module type Map_Reduce = sig
    type result
    val default : result
    val map : t -> key -> T.value -> result
    val reduce : t -> result -> result -> result
  end
  module Map_Reduce(M:Map_Reduce) = struct
    let cache : M.result Map_cache.t = Map_cache.create 10

    let rec map_reduce_ne t =
      match Map_cache.find_opt cache t with
      | Some v -> v
      | None ->
          let v =
            match t with
            | T.Leaf leaf ->
                M.map (E t) leaf.key leaf.value
            | T.Node node ->
                let v_true = map_reduce_ne node.true_ in
                let v_false = map_reduce_ne node.false_ in
                M.reduce (E t) v_true v_false
          in
          Map_cache.add cache t v;
          v

    let run (E t) = match t with
      | T.Empty ->
          M.default
      | T.Leaf _ as t ->
          map_reduce_ne t
      | T.Node _ as t ->
          map_reduce_ne t

    let rec filter_ne f t =
      let result = map_reduce_ne t in
      if f result then
        E t
      else
        match t with
        | T.Leaf _ ->
            empty
        | T.Node node ->
            let true_ = filter_ne f node.true_ in
            let false_ = filter_ne f node.false_ in
            rebuild_branch t node.prefix node.mask
              ~node_true:node.true_ ~node_false:node.false_ ~true_ ~false_

    let filter f (E t) = match t with
      | T.Empty ->
          empty
      | T.Leaf _ as t ->
          filter_ne f t
      | T.Node _ as t ->
          filter_ne f t

  end

  (* Packing in the existential *)

  let mem key (E t) =
    mem key t

  let mem_exact key (E t) =
    mem_exact key t

  let find key (E t) =
    find key t

  let singleton ~key ~value ~mask =
    E (singleton ~key ~value ~mask)

  let add combine ~key ~value ?mask (E t) =
    E (add combine ~key ~value ?mask t)

end [@@inline]

module type S = sig
  type key
  type value
  type mask
  type t

  val equal : t -> t -> bool

  val empty : t
  val singleton : key:key -> value:value -> mask:mask -> t
  val add : (value -> value -> value) -> key:key -> value:value ->
    ?mask:mask -> t -> t
  val remove : key -> t -> t
  val remove_exact : key -> t -> t
  val remove_prefix : key -> mask -> t -> t
  val mem : key -> t -> bool
  val mem_exact : key -> t -> bool
  val find : key -> t -> value option
  val replace_subtree : replaced:t -> value -> t -> t
  val fold : (key -> mask -> value -> 'a -> 'a) -> t -> 'a -> 'a

  module type Map_Reduce = sig
    type result
    val default : result
    val map : t -> key -> value -> result
    val reduce : t -> result -> result -> result
  end
  module Map_Reduce(M:Map_Reduce) : sig
    val run : t -> M.result
    val filter : (M.result -> bool) -> t -> t
  end

end

module Make_LE(V:Value) = Make(LE_prefix)(V)
module Make_BE(V:Value) = Make(BE_prefix)(V)
module Make_BE_gen(V:Value)(B:Bits) = Make(BE_gen_prefix(B))(V)
module Make_BE_sized(V:Value)(S:Size) = Make(BE_gen_prefix(Bits(S)))(V)
