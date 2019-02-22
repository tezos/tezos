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

module Make (K: Hashtbl.HashedType): sig

  (** A bounded table which optimistically cheats on the bound and sometimes
      counts wrong.
      Specifically, the table retains a bounded number of elements. It will also
      retain more if given more than that, but it will always drop back to the
      bound if the garbage collector intervenes.
      In addition, repeated bindings on the same key use several slots when
      counting the number of elements even though only the last binding is ever
      accessible. *)
  type 'a t

  (** [create n] is a table with at most [n] elements except when it has more. *)
  val create: int -> 'a t

  (** [add t k v] adds a mapping from key [k] to value [v] in the table.
      NOTE: when n values are bound to the same key, it may count as up to n
      elements.
      However, NOTE: when n values are bound to the same key, only the last
      binding can be found with [find_opt] or traversed with [fold]. *)
  val add: 'a t -> K.t -> 'a -> unit

  (** [fold f t acc] folds over the bindings in [t] starting with [acc]. *)
  val fold: (K.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** [find_opt t k] is [Some v] if [k] is bound to [v] in [t] and [None]
      otherwise. A key [k] is bound to a value [v] in [t] if [add t k v] has been
      called and not too many other bindings have been added since then. *)
  val find_opt: 'a t -> K.t -> 'a option

end
