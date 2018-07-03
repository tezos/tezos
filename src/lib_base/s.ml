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

module type T = sig

  type t
  include Compare.S with type t := t

  val pp: Format.formatter -> t -> unit

  val encoding: t Data_encoding.t
  val to_bytes: t -> MBytes.t
  val of_bytes: MBytes.t -> t option

end

module type HASHABLE = sig

  include T

  type hash
  val hash: t -> hash
  val hash_raw: MBytes.t -> hash

end

module type SET = sig
  type elt
  type t
  val empty: t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val singleton: elt -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val iter: (elt -> unit) -> t -> unit
  val map: (elt -> elt) -> t -> t
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (elt -> bool) -> t -> bool
  val exists: (elt -> bool) -> t -> bool
  val filter: (elt -> bool) -> t -> t
  val partition: (elt -> bool) -> t -> t * t
  val cardinal: t -> int
  val elements: t -> elt list
  val min_elt_opt: t -> elt option
  val max_elt_opt: t -> elt option
  val choose_opt: t -> elt option
  val split: elt -> t -> t * bool * t
  val find_opt: elt -> t -> elt option
  val find_first_opt: (elt -> bool) -> t -> elt option
  val find_last_opt: (elt -> bool) -> t -> elt option
  val of_list: elt list -> t
end

module type MAP = sig
  type key
  type (+'a) t
  val empty: 'a t
  val is_empty: 'a t -> bool
  val mem: key -> 'a t -> bool
  val add: key -> 'a -> 'a t -> 'a t
  val update: key -> ('a option -> 'a option) -> 'a t -> 'a t
  val singleton: key -> 'a -> 'a t
  val remove: key -> 'a t -> 'a t
  val merge:
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val union: (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all: (key -> 'a -> bool) -> 'a t -> bool
  val exists: (key -> 'a -> bool) -> 'a t -> bool
  val filter: (key -> 'a -> bool) -> 'a t -> 'a t
  val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal: 'a t -> int
  val bindings: 'a t -> (key * 'a) list
  val min_binding_opt: 'a t -> (key * 'a) option
  val max_binding_opt: 'a t -> (key * 'a) option
  val choose_opt: 'a t -> (key * 'a) option
  val split: key -> 'a t -> 'a t * 'a option * 'a t
  val find_opt: key -> 'a t -> 'a option
  val find_first_opt: (key -> bool) -> 'a t -> (key * 'a) option
  val find_last_opt: (key -> bool) -> 'a t -> (key * 'a) option
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
end
