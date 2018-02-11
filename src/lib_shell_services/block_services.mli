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

type preapply_result = {
  shell_header: Block_header.shell_header ;
  operations: error Preapply_result.t list ;
}

open RPC_context

val net_id:
  #simple -> block -> Net_id.t tzresult Lwt.t
val level:
  #simple -> block -> Int32.t tzresult Lwt.t
val predecessor:
  #simple -> block -> Block_hash.t tzresult Lwt.t
val predecessors:
  #simple -> block -> int -> Block_hash.t list tzresult Lwt.t
val hash:
  #simple -> block -> Block_hash.t tzresult Lwt.t
val timestamp:
  #simple -> block -> Time.t tzresult Lwt.t
val fitness:
  #simple -> block -> MBytes.t list tzresult Lwt.t
val operations:
  #simple -> ?contents:bool ->
  block -> (Operation_hash.t * Operation.t option) list list tzresult Lwt.t
val protocol:
  #simple -> block -> Protocol_hash.t tzresult Lwt.t
val test_network:
  #simple -> block -> Test_network_status.t tzresult Lwt.t

val pending_operations:
  #simple -> block ->
  (error Preapply_result.t * Operation.t Operation_hash.Map.t) tzresult Lwt.t

val info:
  #simple ->
  ?include_ops:bool -> block -> block_info tzresult Lwt.t

val list:
  ?include_ops:bool -> ?length:int -> ?heads:Block_hash.t list ->
  ?delay:int -> ?min_date:Time.t -> ?min_heads:int ->
  #simple ->
  block_info list list tzresult Lwt.t

val monitor:
  ?include_ops:bool -> ?length:int -> ?heads:Block_hash.t list ->
  ?delay:int -> ?min_date:Time.t -> ?min_heads:int ->
  #streamed ->
  (block_info list list Lwt_stream.t * stopper) tzresult Lwt.t

val preapply:
  #simple -> block ->
  ?timestamp:Time.t ->
  ?sort:bool ->
  proto_header:MBytes.t ->
  Operation.t list list -> preapply_result tzresult Lwt.t

val complete:
  #simple -> block -> string -> string list tzresult Lwt.t

val monitor_prevalidated_operations:
  ?contents:bool ->
  #streamed ->
  ((Operation_hash.t * Operation.t option) list list Lwt_stream.t * stopper) tzresult Lwt.t

val unmark_invalid:
  #simple -> Block_hash.t -> unit Error_monad.tzresult Lwt.t
val list_invalid:
  #simple -> (Block_hash.t * int32 * error list) list tzresult Lwt.t


module S : sig

  val blocks_arg : block RPC_arg.arg

  val info:
    ([ `POST ], unit,
     unit * block, unit, bool,
     block_info) RPC_service.t
  val net_id:
    ([ `POST ], unit,
     unit * block, unit, unit,
     Net_id.t) RPC_service.t
  val level:
    ([ `POST ], unit,
     unit * block, unit, unit,
     Int32.t) RPC_service.t
  val predecessor:
    ([ `POST ], unit,
     unit * block, unit, unit,
     Block_hash.t) RPC_service.t
  val predecessors:
    ([ `POST ], unit,
     unit * block , unit, int,
     Block_hash.t list) RPC_service.t
  val hash:
    ([ `POST ], unit,
     unit * block, unit, unit,
     Block_hash.t) RPC_service.t
  val timestamp:
    ([ `POST ], unit,
     unit * block, unit, unit,
     Time.t) RPC_service.t
  val fitness:
    ([ `POST ], unit,
     unit * block, unit, unit,
     MBytes.t list) RPC_service.t
  val context:
    ([ `POST ], unit,
     unit * block, unit, unit,
     Context_hash.t) RPC_service.t

  type operations_param = {
    contents: bool ;
    monitor: bool ;
  }
  val operations:
    ([ `POST ], unit,
     unit * block, unit, operations_param,
     (Operation_hash.t * Operation.t option) list list) RPC_service.t

  val protocol:
    ([ `POST ], unit,
     unit * block, unit, unit,
     Protocol_hash.t) RPC_service.t
  val test_network:
    ([ `POST ], unit,
     unit * block, unit, unit,
     Test_network_status.t) RPC_service.t
  val pending_operations:
    ([ `POST ], unit,
     unit * block, unit, unit,
     error Preapply_result.t * Operation.t Operation_hash.Map.t) RPC_service.t

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
     block_info list list) RPC_service.t

  val list_invalid:
    ([ `POST ], unit,
     unit, unit, unit,
     (Block_hash.t * int32 * error list) list) RPC_service.t

  val unmark_invalid:
    ([ `POST ], unit,
     unit * Block_hash.t, unit, unit,
     unit) RPC_service.t

  type preapply_param = {
    timestamp: Time.t ;
    proto_header: MBytes.t ;
    operations: Operation.t list list ;
    sort_operations: bool ;
  }

  val preapply:
    ([ `POST ], unit,
     unit * block, unit, preapply_param,
     preapply_result) RPC_service.t

  val complete:
    ([ `POST ], unit,
     (unit * block) * string, unit, unit,
     string list) RPC_service.t

  val proto_path: (unit, unit * block) RPC_path.path

end
