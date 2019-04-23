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

include Time
type time = t

type error += Timestamp_add (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"timestamp_add"
    ~title:"Timestamp add"
    ~description:"Overflow when adding timestamps."
    ~pp:(fun ppf () ->
        Format.fprintf ppf "Overflow when adding timestamps.")
    Data_encoding.empty
    (function Timestamp_add -> Some () | _ -> None)
    (fun () -> Timestamp_add)

let of_seconds s =
  try Some (of_seconds (Int64.of_string s))
  with _ -> None
let to_seconds = to_seconds
let to_seconds_string s = Int64.to_string (to_seconds s)

let pp = pp_hum

let (+?) x y =
  (* TODO check overflow *)
  try ok (add x (Period_repr.to_seconds y))
  with _exn -> Error [ Timestamp_add ]
