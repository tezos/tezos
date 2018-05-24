(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_signer_remote_messages

val sign :
  ([ `POST ], unit, unit, unit, Sign.Request.t, Sign.Response.t) RPC_service.t

val public_key :
  ([ `POST ], unit, unit, unit, Public_key.Request.t, Public_key.Response.t) RPC_service.t

type path = string * string

val base : path -> (string * int) tzresult Lwt.t

val call :
  path -> ([ `POST ], unit, unit, unit, 'p, 'r) RPC_service.t -> 'p -> 'r tzresult Lwt.t
