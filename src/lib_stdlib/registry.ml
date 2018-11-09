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

module type S = sig
  type k
  type v
  val register: k -> v -> unit
  val alter: k -> (v -> v) -> unit
  val remove: k -> unit
  val query: k -> v option
  val iter_p: (k -> v -> unit Lwt.t) -> unit Lwt.t
  val fold: (k -> v -> 'a -> 'a) -> 'a -> 'a
end

module Make (M: sig type v include Map.OrderedType end) : S
  with type k = M.t
   and type v = M.v =
struct

  module Reg = Map.Make(M)
  type v = M.v
  type k = Reg.key
  let registry: v Reg.t ref = ref Reg.empty
  let register k v = registry := Reg.add k v !registry
  let alter k f =
    match Reg.find_opt k !registry with
    | None -> ()
    | Some v -> registry := Reg.add k (f v) !registry
  let remove k = registry := Reg.remove k !registry
  let query k = Reg.find_opt k !registry
  let iter_p f = Lwt.join (Reg.fold (fun k v acc -> (f k v) :: acc) !registry [])
  let fold f a = Reg.fold f !registry a

end

