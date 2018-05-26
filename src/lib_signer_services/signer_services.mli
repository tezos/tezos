(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val sign :
  ([ `POST ], unit, unit * Signature.Public_key_hash.t,
   unit, MBytes.t, Signature.t) RPC_service.t

val public_key :
  ([ `GET ], unit, unit * Signature.Public_key_hash.t,
   unit, unit, Signature.Public_key.t) RPC_service.t
