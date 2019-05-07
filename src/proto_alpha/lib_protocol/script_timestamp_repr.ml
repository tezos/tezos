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

type t = Z.t

let compare = Z.compare

let of_int64 = Z.of_int64

let of_string x =
  match Time_repr.of_notation x with
  | None ->
      begin try Some (Z.of_string x)
        with _ -> None
      end
  | Some time ->
      Some (of_int64 (Time_repr.to_seconds time))

let to_notation x =
  try
    let notation = Time_repr.to_notation (Time.of_seconds (Z.to_int64 x)) in
    if String.equal notation "out_of_range"
    then None
    else Some notation
  with _ -> None

let to_num_str = Z.to_string

let to_string x =
  match to_notation x with
  | None -> to_num_str x
  | Some s -> s

let diff x y = Script_int_repr.of_zint @@ Z.sub x y

let sub_delta t delta = Z.sub t (Script_int_repr.to_zint delta)

let add_delta t delta =
  Z.add t (Script_int_repr.to_zint delta)

let to_zint x = x
let of_zint x = x
