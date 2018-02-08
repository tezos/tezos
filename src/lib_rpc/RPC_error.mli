(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

val service:
  ([ `POST ], unit, unit, unit, unit, Json_schema.schema) RPC_service.t
val encoding: error list Data_encoding.t
val wrap: 'a Data_encoding.t -> 'a tzresult Data_encoding.encoding
