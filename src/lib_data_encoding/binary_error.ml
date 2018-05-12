(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type read_error =
  | Not_enough_data
  | Extra_bytes
  | No_case_matched
  | Unexpected_tag of int
  | Invalid_size of int
  | Invalid_int of { min : int ; v : int ; max : int }
  | Invalid_float of { min : float ; v : float ; max : float }
  | Trailing_zero

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

exception Read_error of read_error
