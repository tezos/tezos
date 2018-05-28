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

val write : 'a Encoding.t -> 'a -> MBytes.t -> int -> int -> int option
val to_bytes_exn : 'a Encoding.t -> 'a -> MBytes.t
val to_bytes : 'a Encoding.t -> 'a -> MBytes.t option
