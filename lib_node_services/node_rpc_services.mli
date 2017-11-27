(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Error : sig
  val service: (unit, unit, unit, Json_schema.schema) RPC.service
  val encoding: error list Data_encoding.t
  val wrap: 'a Data_encoding.t -> 'a tzresult Data_encoding.encoding
end

module Blocks : sig

  type block = [
    | `Genesis
    | `Head of int | `Prevalidation
    | `Test_head of int | `Test_prevalidation
    | `Hash of Block_hash.t
  ]
  val blocks_arg : block RPC.Arg.arg

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
    data: MBytes.t ;
    operations: (Operation_hash.t * Operation.t) list list option ;
    protocol: Protocol_hash.t ;
    test_network: Test_network_status.t ;
  }

  val info:
    (unit, unit * block, bool, block_info) RPC.service
  val net_id:
    (unit, unit * block, unit, Net_id.t) RPC.service
  val level:
    (unit, unit * block, unit, Int32.t) RPC.service
  val predecessor:
    (unit, unit * block, unit, Block_hash.t) RPC.service
  val predecessors:
    (unit, unit * block , int, Block_hash.t list) RPC.service
  val hash:
    (unit, unit * block, unit, Block_hash.t) RPC.service
  val timestamp:
    (unit, unit * block, unit, Time.t) RPC.service
  val fitness:
    (unit, unit * block, unit, MBytes.t list) RPC.service

  type operations_param = {
    contents: bool ;
    monitor: bool ;
  }
  val operations:
    (unit, unit * block, operations_param,
     (Operation_hash.t * Operation.t option) list list) RPC.service

  val protocol:
    (unit, unit * block, unit, Protocol_hash.t) RPC.service
  val test_network:
    (unit, unit * block, unit, Test_network_status.t) RPC.service
  val pending_operations:
    (unit, unit * block, unit,
     error Preapply_result.t * Operation.t Operation_hash.Map.t) RPC.service

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
    (unit, unit, list_param, block_info list list) RPC.service

  val list_invalid:
    (unit, unit, unit, (Block_hash.t * int32 * error list) list) RPC.service

  type preapply_param = {
    timestamp: Time.t ;
    proto_header: MBytes.t ;
    operations: Operation.t list ;
    sort_operations: bool ;
  }

  type preapply_result = {
    shell_header: Block_header.shell_header ;
    operations: error Preapply_result.t ;
  }
  val preapply:
    (unit, unit * block, preapply_param, preapply_result tzresult) RPC.service

  val complete: (unit, (unit * block) * string, unit, string list) RPC.service

  val proto_path: (unit, unit * block) RPC.Path.path


end

module Protocols : sig

  val contents:
    (unit, unit * Protocol_hash.t, unit, Protocol.t) RPC.service

  type list_param = {
    contents: bool option ;
    monitor: bool option ;
  }

  val list:
    (unit, unit,
     list_param,
     (Protocol_hash.t * Protocol.t option) list) RPC.service

end

module Network : sig

  val stat :
    (unit, unit, unit, P2p_types.Stat.t) RPC.service

  val versions :
    (unit, unit, unit, P2p_types.Version.t list) RPC.service

  val events :
    (unit, unit, unit, P2p_types.Connection_pool_log_event.t) RPC.service

  val connect :
    (unit, unit * P2p_types.Point.t, float, unit tzresult) RPC.service

  module Connection : sig

    val list :
      (unit, unit, unit, P2p_types.Connection_info.t list) RPC.service

    val info :
      (unit, unit * P2p_types.Peer_id.t, unit,
       P2p_types.Connection_info.t option) RPC.service

    val kick :
      (unit, unit * P2p_types.Peer_id.t, bool, unit) RPC.service

  end

  module Point : sig
    val list :
      (unit, unit, P2p_types.Point_state.t list,
       (P2p_types.Point.t * P2p_types.Point_info.t) list) RPC.service
    val info :
      (unit, unit * P2p_types.Point.t, unit, P2p_types.Point_info.t option) RPC.service
    val events :
      (unit, unit * P2p_types.Point.t, bool,
       P2p_connection_pool_types.Point_info.Event.t list) RPC.service
  end

  module Peer_id : sig

    val list :
      (unit, unit, P2p_types.Peer_state.t list,
       (P2p_types.Peer_id.t * P2p_types.Peer_info.t) list) RPC.service

    val info :
      (unit, unit * P2p_types.Peer_id.t, unit,
       P2p_types.Peer_info.t option) RPC.service

    val events :
      (unit, unit * P2p_types.Peer_id.t, bool,
       P2p_connection_pool_types.Peer_info.Event.t list) RPC.service

  end

end

val forge_block_header:
  (unit, unit, Block_header.t, MBytes.t) RPC.service

type inject_block_param = {
  raw: MBytes.t ;
  blocking: bool ;
  force: bool ;
  net_id: Net_id.t option ;
  operations: Operation.t list list ;
}

val inject_block:
  (unit, unit, inject_block_param, Block_hash.t tzresult) RPC.service

val inject_operation:
  (unit, unit,
   (MBytes.t * bool * Net_id.t option * bool option),
   Operation_hash.t tzresult) RPC.service

val inject_protocol:
  (unit, unit,
   (Protocol.t * bool * bool option),
   Protocol_hash.t tzresult) RPC.service

val bootstrapped: (unit, unit, unit, Block_hash.t * Time.t) RPC.service

val complete: (unit, unit * string, unit, string list) RPC.service

val describe: (unit, unit) RPC.Service.description_service
