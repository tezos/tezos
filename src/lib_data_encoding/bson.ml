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

let construct e v = Json_repr_bson.Json_encoding.construct (Json.convert e) v
let destruct e v = Json_repr_bson.Json_encoding.destruct (Json.convert e) v
