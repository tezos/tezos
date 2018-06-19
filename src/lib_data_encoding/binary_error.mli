(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** This is for use *within* the data encoding library only. Instead, you should
    use the corresponding module intended for use: {Data_encoding.Binary}. *)

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
exception Read_error of read_error
val pp_read_error: Format.formatter -> read_error -> unit

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

val pp_write_error : Format.formatter -> write_error -> unit

exception Write_error of write_error
