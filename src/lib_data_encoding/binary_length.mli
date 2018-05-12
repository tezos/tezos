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

val length : 'a Encoding.t -> 'a -> int
val fixed_length : 'a Encoding.t -> int option
val fixed_length_exn : 'a Encoding.t -> int
