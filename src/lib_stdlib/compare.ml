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

module type COMPARABLE = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type t
  val (=) : t -> t -> bool
  val (<>) : t -> t -> bool
  val (<) : t -> t -> bool
  val (<=) : t -> t -> bool
  val (>=) : t -> t -> bool
  val (>) : t -> t -> bool
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val max : t -> t -> t
  val min : t -> t -> t
end

module Make (P : COMPARABLE) = struct
  include P
  let compare = compare
  let (=) a b = compare a b = 0
  let (<>) a b = compare a b <> 0
  let (<) a b = compare a b < 0
  let (<=) a b = compare a b <= 0
  let (>=) a b = compare a b >= 0
  let (>) a b = compare a b > 0
  let equal = (=)
  let max x y = if x >= y then x else y
  let min x y = if x <= y then x else y
end

module List (P : COMPARABLE) = struct
  type t = P.t list
  let rec compare xs ys =
    match xs, ys with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys ->
        let hd = P.compare x y in
        if hd <> 0 then hd else compare xs ys
  let (=) xs ys = compare xs ys = 0
  let (<>) xs ys = compare xs ys <> 0
  let (<) xs ys = compare xs ys < 0
  let (<=) xs ys = compare xs ys <= 0
  let (>=) xs ys = compare xs ys >= 0
  let (>) xs ys = compare xs ys > 0
  let equal = (=)
  let max x y = if x >= y then x else y
  let min x y = if x <= y then x else y
end

module Option (P : COMPARABLE) = struct
  type t = P.t option
  let compare xs ys =
    match xs, ys with
    | None, None -> 0
    | None, _ -> -1
    | _, None -> 1
    | Some x, Some y -> P.compare x y
  let (=) xs ys = compare xs ys = 0
  let (<>) xs ys = compare xs ys <> 0
  let (<) xs ys = compare xs ys < 0
  let (<=) xs ys = compare xs ys <= 0
  let (>=) xs ys = compare xs ys >= 0
  let (>) xs ys = compare xs ys > 0
  let equal = (=)
  let max x y = if x >= y then x else y
  let min x y = if x <= y then x else y
end

module Char = Make (Char)
module Bool = Make (struct type t = bool let compare = Pervasives.compare end)
module Int = Make (struct type t = int let compare = Pervasives.compare end)
module Int32 = Make (Int32)
module Int64 = Make (Int64)

module MakeUnsigned (Int : S) (Z : sig val zero : Int.t end) = struct
  type t = Int.t
  let compare va vb =
    Int.(if va >= Z.zero then if vb >= Z.zero then compare va vb else -1
         else if vb >= Z.zero then 1 else compare va vb)
  let (=) = ((=) : t -> t -> bool)
  let (<>) = ((<>) : t -> t -> bool)
  let (<) a b =
    Int.(if Z.zero <= a then
           (a < b || b < Z.zero)
         else
           (b < Z.zero && a < b))
  let (<=) a b =
    Int.(if Z.zero <= a then
           (a <= b || b < Z.zero)
         else
           (b < Z.zero && a <= b))
  let (>=) a b = (<=) b a
  let (>) a b = (<) b a
  let equal = (=)
  let max x y = if x >= y then x else y
  let min x y = if x <= y then x else y
end

module Uint32 = MakeUnsigned (Int32) (struct let zero = 0l end)
module Uint64 = MakeUnsigned (Int64) (struct let zero = 0L end)

module Float = Make (struct type t = float let compare = Pervasives.compare end)
module String = Make (String)

module Z = Make (Z)
