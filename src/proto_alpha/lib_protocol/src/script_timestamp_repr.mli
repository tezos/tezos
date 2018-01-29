(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Script_int_repr

type t

val of_int64 : int64 -> t

val compare : t -> t -> int

(* Convert a timestamp to a notation if possible *)
val to_notation : t -> string option
(* Convert a timestamp to a string representation of the seconds *)
val to_num_str : t -> string
(* Convert to a notation if possible, or num if not *)
val to_string : t -> string
val of_string : string -> t option

val diff : t -> t -> z num

val add_delta : t -> z num -> t

val sub_delta : t -> z num -> t

val to_zint : t -> Z.t
