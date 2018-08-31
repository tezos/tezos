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

(* let re_trailing_null =
   Re_pcre.regexp "^(.*?)\000*$"

   let remove_trailing_null s =
   Re.get (Re.exec re_trailing_null s) 1 *)

let remove_trailing_null s =
  let n = String.length s in
  let i = ref (n-1) in
  while (!i >= 0) && (String.get s !i = '\000') do
    i := !i - 1
  done; String.sub s 0 (!i+1)

let serialize z =
  let n =
    if Z.(lt z zero) then
      Z.(neg (add (add z z) one))
    else
      Z.(add z z)
  in
  n |> Z.to_bits |> remove_trailing_null

let deserialize z =
  let n = Z.of_bits z in
  let z = Z.shift_right_trunc n 1 in
  if Z.(n land one = zero) then z else Z.neg z

let leq a b = (Z.compare a b) <= 0

let geq a b = (Z.compare a b) >= 0

let lt  a b = (Z.compare a b) < 0

let gt  a b = (Z.compare a b) > 0

let (<) = lt
let (>) = gt
let (<=) = leq
let (>=) = geq

let zero = Z.zero
let one = Z.one

let invert a n =
  try
    Some (Z.invert a n)
  with
    Division_by_zero -> None
