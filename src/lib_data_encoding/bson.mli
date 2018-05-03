(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** This is for use *within* the data encoding library only. Instead, you should
    use the corresponding module intended for use: {Data_encoding.Bson}. *)

type bson = Json_repr_bson.bson
type t = bson
val construct : 't Encoding.t -> 't -> bson
val destruct : 't Encoding.t -> bson -> 't
