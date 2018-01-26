(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val forge_block_header:
  ([ `POST ], unit,
   unit, unit, Block_header.t,
   MBytes.t, unit) RPC_service.t

type inject_block_param = {
  raw: MBytes.t ;
  blocking: bool ;
  force: bool ;
  net_id: Net_id.t option ;
  operations: Operation.t list list ;
}

val inject_block:
  ([ `POST ], unit,
   unit, unit, inject_block_param,
   Block_hash.t tzresult, unit) RPC_service.t

val inject_operation:
  ([ `POST ], unit,
   unit, unit, (MBytes.t * bool * Net_id.t option),
   Operation_hash.t tzresult, unit) RPC_service.t

val inject_protocol:
  ([ `POST ], unit,
   unit, unit, (Protocol.t * bool * bool option),
   Protocol_hash.t tzresult, unit) RPC_service.t

val bootstrapped:
  ([ `POST ], unit,
   unit, unit, unit,
   Block_hash.t * Time.t, unit) RPC_service.t

val complete:
  ([ `POST ], unit,
   unit * string, unit, unit,
   string list, unit) RPC_service.t

val describe: (unit, unit) RPC_service.description_service
