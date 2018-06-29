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

type read_error =
  | Not_enough_data
  | Extra_bytes
  | No_case_matched
  | Unexpected_tag of int
  | Invalid_size of int
  | Invalid_int of { min : int ; v : int ; max : int }
  | Invalid_float of { min : float ; v : float ; max : float }
  | Trailing_zero
  | Size_limit_exceeded
  | List_too_long
  | Array_too_long

let pp_read_error ppf = function
  | Not_enough_data ->
      Format.fprintf ppf "Not enough data"
  | Extra_bytes ->
      Format.fprintf ppf "Extra bytes"
  | No_case_matched ->
      Format.fprintf ppf "No case matched"
  | Unexpected_tag tag ->
      Format.fprintf ppf "Unexpected tag %d" tag
  | Invalid_size sz ->
      Format.fprintf ppf "Invalid size %d" sz
  | Invalid_int { min ; v ; max}  ->
      Format.fprintf ppf "Invalid int (%d <= %d <= %d) " min v max
  | Invalid_float { min ; v ; max}  ->
      Format.fprintf ppf "Invalid float (%f <= %f <= %f) " min v max
  | Trailing_zero ->
      Format.fprintf ppf "Trailing zero in Z"
  | Size_limit_exceeded ->
      Format.fprintf ppf "Size limit exceeded"
  | List_too_long ->
      Format.fprintf ppf "List length limit exceeded"
  | Array_too_long ->
      Format.fprintf ppf "Array length limit exceeded"

exception Read_error of read_error

type write_error =
  | Size_limit_exceeded
  | No_case_matched
  | Invalid_int of { min : int ; v : int ; max : int }
  | Invalid_float of { min : float ; v : float ; max : float }
  | Invalid_bytes_length of { expected : int ; found : int }
  | Invalid_string_length of { expected : int ; found : int }
  | Invalid_natural
  | List_too_long
  | Array_too_long

let pp_write_error ppf = function
  | Size_limit_exceeded ->
      Format.fprintf ppf "Size limit exceeded"
  | No_case_matched ->
      Format.fprintf ppf "No case matched"
  | Invalid_int { min ; v ; max}  ->
      Format.fprintf ppf "Invalid int (%d <= %d <= %d) " min v max
  | Invalid_float { min ; v ; max}  ->
      Format.fprintf ppf "Invalid float (%f <= %f <= %f) " min v max
  | Invalid_bytes_length { expected ; found } ->
      Format.fprintf ppf
        "Invalid bytes length (expected: %d ; found %d)"
        expected found
  | Invalid_string_length { expected ; found } ->
      Format.fprintf ppf
        "Invalid string length (expected: %d ; found %d)"
        expected found
  | Invalid_natural ->
      Format.fprintf ppf "Negative natural"
  | List_too_long ->
      Format.fprintf ppf "List length limit exceeded"
  | Array_too_long ->
      Format.fprintf ppf "Array length limit exceeded"

exception Write_error of write_error
