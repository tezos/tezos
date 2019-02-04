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

type t = Int64.t
type period = t
include (Compare.Int64 : Compare.S with type t := t)
let encoding = Data_encoding.int64

let pp ppf v = Format.fprintf ppf "%Ld" v

type error += (* `Permanent *)
  | Malformed_period
  | Invalid_arg

let () =
  let open Data_encoding in
  (* Malformed period *)
  register_error_kind
    `Permanent
    ~id:"malformed_period"
    ~title:"Malformed period"
    ~description:"Period is negative."
    ~pp:(fun ppf () -> Format.fprintf ppf "Malformed period")
    empty
    (function Malformed_period -> Some () | _ -> None)
    (fun () -> Malformed_period) ;
  (* Invalid arg *)
  register_error_kind
    `Permanent
    ~id:"invalid_arg"
    ~title:"Invalid arg"
    ~description:"Negative multiple of periods are not allowed."
    ~pp:(fun ppf () -> Format.fprintf ppf "Invalid arg")
    empty
    (function Invalid_arg -> Some () | _ -> None)
    (fun () -> Invalid_arg)

let of_seconds t =
  if Compare.Int64.(t >= 0L)
  then ok t
  else error Malformed_period
let to_seconds t = t
let of_seconds_exn t =
  match of_seconds t with
  | Ok t -> t
  | _ -> invalid_arg "Period.of_seconds_exn"

let mult i p =
  (* TODO check overflow *)
  if Compare.Int32.(i < 0l)
  then error Invalid_arg
  else ok (Int64.mul (Int64.of_int32 i) p)

let one_second = of_seconds_exn 1L
let one_minute = of_seconds_exn 60L
let one_hour = of_seconds_exn 3600L
