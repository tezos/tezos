(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Labs, Inc. <contact@nomadic-labs.com>          *)
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
module type S =
sig
  type 'a t
  type key

  val create: int -> 'a t
  (** [create n] is a table with at most [n] elements except when it has more. *)

  val add: 'a t -> key -> 'a -> unit
  (** [add t k v] adds a mapping from key [k] to value [v] in the table.
      NOTE: when n values are bound to the same key, it may count as up to n
      elements.
      However, NOTE: when n values are bound to the same key, only the last
      binding can be found with [find_opt] or traversed with [fold]. *)

  val add_and_return_erased: 'a t -> key -> 'a -> key option

  val iter: (key -> 'a -> unit) -> 'a t -> unit

  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold f t acc] folds the function [f] and value [acc] through the recently
      added elements of [t]. It never folds over more elements than the size
      bound of the table, even if the table temporarily holds more elements. *)

  val find_opt: 'a t -> key -> 'a option
  (** [find_opt t k] is [Some v] if [k] is bound to [v] in [t] and [None]
      otherwise. A key [k] is bound to a value [v] in [t] if [add t k v] has been
      called and not too many other bindings have been added since then. *)

  val remove: 'a t -> key -> unit
  (** [remove t k] removes the binding from [key] to the associated element in
      [t]. Note that you may still be able to find the element using [find_opt]
      for some time. *)

  val length: 'a t -> int
  (** [length t] is the number of elements currently in [t], including those
      that may be garbage collected. *)

end


module Make (K: Hashtbl.HashedType): S with type key = K.t
(** A bounded table which optimistically cheats on the bound and sometimes
    counts wrong.
    Specifically, the table retains a bounded number of elements. It will also
    retain more if given more than that, but it will always drop back to the
    bound if the garbage collector intervenes. *)
