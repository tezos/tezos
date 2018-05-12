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

val read: 'a Encoding.t -> MBytes.t -> int -> int -> (int * 'a) option
val of_bytes: 'a Encoding.t -> MBytes.t -> 'a option
val of_bytes_exn: 'a Encoding.t -> MBytes.t -> 'a
