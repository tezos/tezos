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

type t = {
  total_sent : int64 ;
  total_recv : int64 ;
  current_inflow : int ;
  current_outflow : int ;
}

let empty = {
  total_sent = 0L ;
  total_recv = 0L ;
  current_inflow = 0 ;
  current_outflow = 0 ;
}

let print_size ppf sz =
  let ratio n = (float_of_int sz /. float_of_int (1 lsl n)) in
  if sz < 1 lsl 10 then
    Format.fprintf ppf "%d B" sz
  else if sz < 1 lsl 20 then
    Format.fprintf ppf "%.2f kiB" (ratio 10)
  else
    Format.fprintf ppf "%.2f MiB" (ratio 20)

let print_size64 ppf sz =
  let open Int64 in
  let ratio n = (to_float sz /. float_of_int (1 lsl n)) in
  if sz < shift_left 1L 10 then
    Format.fprintf ppf "%Ld B" sz
  else if sz < shift_left 1L 20 then
    Format.fprintf ppf "%.2f kiB" (ratio 10)
  else if sz < shift_left 1L 30 then
    Format.fprintf ppf "%.2f MiB" (ratio 20)
  else if sz < shift_left 1L 40 then
    Format.fprintf ppf "%.2f GiB" (ratio 30)
  else
    Format.fprintf ppf "%.2f TiB" (ratio 40)

let pp ppf stat =
  Format.fprintf ppf
    "↗ %a (%a/s) ↘ %a (%a/s)"
    print_size64 stat.total_sent print_size stat.current_outflow
    print_size64 stat.total_recv print_size stat.current_inflow

let encoding =
  let open Data_encoding in
  conv
    (fun { total_sent ; total_recv ; current_inflow ; current_outflow } ->
       (total_sent, total_recv, current_inflow, current_outflow))
    (fun (total_sent, total_recv, current_inflow, current_outflow) ->
       { total_sent ; total_recv ; current_inflow ; current_outflow })
    (obj4
       (req "total_sent" int64)
       (req "total_recv" int64)
       (req "current_inflow" int31)
       (req "current_outflow" int31))
