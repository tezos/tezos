(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type bson = Json_repr_bson.bson
type t = bson

(** Construct a BSON object from an encoding. *)
val construct : 't Encoding.t -> 't -> bson

(** Destruct a BSON object into a value.
    Fail with an exception if the JSON object and encoding do not match.. *)
val destruct : 't Encoding.t -> bson -> 't
