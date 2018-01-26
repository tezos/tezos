(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val contents:
  ([ `POST ], unit,
   unit * Protocol_hash.t, unit, unit,
   Protocol.t, unit) RPC_service.t

type list_param = {
  contents: bool option ;
  monitor: bool option ;
}

val list:
  ([ `POST ], unit,
   unit, unit, list_param,
   (Protocol_hash.t * Protocol.t option) list, unit) RPC_service.t
