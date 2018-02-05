(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type block = [
  | `Genesis
  | `Head of int | `Prevalidation
  | `Test_head of int | `Test_prevalidation
  | `Hash of Block_hash.t
]
val blocks_arg : block RPC_arg.arg

val parse_block: string -> (block, string) result
val to_string: block -> string

type block_info = {
  hash: Block_hash.t ;
  net_id: Net_id.t ;
  level: Int32.t ;
  proto_level: int ; (* uint8 *)
  predecessor: Block_hash.t ;
  timestamp: Time.t ;
  validation_passes: int ; (* uint8 *)
  operations_hash: Operation_list_list_hash.t ;
  fitness: MBytes.t list ;
  context: Context_hash.t ;
  data: MBytes.t ;
  operations: (Operation_hash.t * Operation.t) list list option ;
  protocol: Protocol_hash.t ;
  test_network: Test_network_status.t ;
}

val info:
  ([ `POST ], unit,
   unit * block, unit, bool,
   block_info, unit) RPC_service.t
val net_id:
  ([ `POST ], unit,
   unit * block, unit, unit,
   Net_id.t, unit) RPC_service.t
val level:
  ([ `POST ], unit,
   unit * block, unit, unit,
   Int32.t, unit) RPC_service.t
val predecessor:
  ([ `POST ], unit,
   unit * block, unit, unit,
   Block_hash.t, unit) RPC_service.t
val predecessors:
  ([ `POST ], unit,
   unit * block , unit, int,
   Block_hash.t list, unit) RPC_service.t
val hash:
  ([ `POST ], unit,
   unit * block, unit, unit,
   Block_hash.t, unit) RPC_service.t
val timestamp:
  ([ `POST ], unit,
   unit * block, unit, unit,
   Time.t, unit) RPC_service.t
val fitness:
  ([ `POST ], unit,
   unit * block, unit, unit,
   MBytes.t list, unit) RPC_service.t
val context:
  ([ `POST ], unit,
   unit * block, unit, unit,
   Context_hash.t, unit) RPC_service.t

type operations_param = {
  contents: bool ;
  monitor: bool ;
}
val operations:
  ([ `POST ], unit,
   unit * block, unit, operations_param,
   (Operation_hash.t * Operation.t option) list list, unit) RPC_service.t

val protocol:
  ([ `POST ], unit,
   unit * block, unit, unit,
   Protocol_hash.t, unit) RPC_service.t
val test_network:
  ([ `POST ], unit,
   unit * block, unit, unit,
   Test_network_status.t, unit) RPC_service.t
val pending_operations:
  ([ `POST ], unit,
   unit * block, unit, unit,
   error Preapply_result.t * Operation.t Operation_hash.Map.t, unit) RPC_service.t

type list_param = {
  include_ops: bool ;
  length: int option ;
  heads: Block_hash.t list option ;
  monitor: bool option ;
  delay: int option ;
  min_date: Time.t option;
  min_heads: int option;
}
val list:
  ([ `POST ], unit,
   unit, unit, list_param,
   block_info list list, unit) RPC_service.t

val list_invalid:
  ([ `POST ], unit,
   unit, unit, unit,
   (Block_hash.t * int32 * error list) list, unit) RPC_service.t

val unmark_invalid:
  ([ `POST ], unit, unit, unit, Block_hash.t, unit tzresult, unit) RPC_service.t

type preapply_param = {
  timestamp: Time.t ;
  proto_header: MBytes.t ;
  operations: Operation.t list list ;
  sort_operations: bool ;
}

type preapply_result = {
  shell_header: Block_header.shell_header ;
  operations: error Preapply_result.t list ;
}
val preapply:
  ([ `POST ], unit,
   unit * block, unit, preapply_param,
   preapply_result tzresult, unit) RPC_service.t

val complete:
  ([ `POST ], unit,
   (unit * block) * string, unit, unit,
   string list, unit) RPC_service.t

val proto_path: (unit, unit * block) RPC_path.path
