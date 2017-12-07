(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Error : sig
  val service:
    ([ `POST ], unit, unit, unit, unit, Json_schema.schema, unit) RPC.Service.t
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
    ([ `POST ], unit,
     unit * block, unit, bool,
     block_info, unit) RPC.Service.t
  val net_id:
    ([ `POST ], unit,
     unit * block, unit, unit,
     Net_id.t, unit) RPC.Service.t
  val level:
    ([ `POST ], unit,
     unit * block, unit, unit,
     Int32.t, unit) RPC.Service.t
  val predecessor:
    ([ `POST ], unit,
     unit * block, unit, unit,
     Block_hash.t, unit) RPC.Service.t
  val predecessors:
    ([ `POST ], unit,
     unit * block , unit, int,
     Block_hash.t list, unit) RPC.Service.t
  val hash:
    ([ `POST ], unit,
     unit * block, unit, unit,
     Block_hash.t, unit) RPC.Service.t
  val timestamp:
    ([ `POST ], unit,
     unit * block, unit, unit,
     Time.t, unit) RPC.Service.t
  val fitness:
    ([ `POST ], unit,
     unit * block, unit, unit,
     MBytes.t list, unit) RPC.Service.t

  type operations_param = {
    contents: bool ;
    monitor: bool ;
  }
  val operations:
    ([ `POST ], unit,
     unit * block, unit, operations_param,
     (Operation_hash.t * Operation.t option) list list, unit) RPC.Service.t

  val protocol:
    ([ `POST ], unit,
     unit * block, unit, unit,
     Protocol_hash.t, unit) RPC.Service.t
  val test_network:
    ([ `POST ], unit,
     unit * block, unit, unit,
     Test_network_status.t, unit) RPC.Service.t
  val pending_operations:
    ([ `POST ], unit,
     unit * block, unit, unit,
     error Preapply_result.t * Operation.t Operation_hash.Map.t, unit) RPC.Service.t

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
     block_info list list, unit) RPC.Service.t

  val list_invalid:
    ([ `POST ], unit,
     unit, unit, unit,
     (Block_hash.t * int32 * error list) list, unit) RPC.Service.t

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
    ([ `POST ], unit,
     unit * block, unit, preapply_param,
     preapply_result tzresult, unit) RPC.Service.t

  val complete:
    ([ `POST ], unit,
     (unit * block) * string, unit, unit,
     string list, unit) RPC.Service.t

  val proto_path: (unit, unit * block) RPC.Path.path


end

module Protocols : sig

  val contents:
    ([ `POST ], unit,
     unit * Protocol_hash.t, unit, unit,
     Protocol.t, unit) RPC.Service.t

  type list_param = {
    contents: bool option ;
    monitor: bool option ;
  }

  val list:
    ([ `POST ], unit,
     unit, unit, list_param,
     (Protocol_hash.t * Protocol.t option) list, unit) RPC.Service.t

end

module Network : sig

  val stat :
    ([ `POST ], unit,
     unit, unit, unit,
     P2p_types.Stat.t, unit) RPC.Service.t

  val versions :
    ([ `POST ], unit,
     unit, unit, unit,
     P2p_types.Version.t list, unit) RPC.Service.t

  val events :
    ([ `POST ], unit,
     unit, unit, unit,
     P2p_types.Connection_pool_log_event.t, unit) RPC.Service.t

  val connect :
    ([ `POST ], unit,
     unit * P2p_types.Point.t, unit, float,
     unit tzresult, unit) RPC.Service.t

  module Connection : sig

    val list :
      ([ `POST ], unit,
       unit, unit, unit,
       P2p_types.Connection_info.t list, unit) RPC.Service.t

    val info :
      ([ `POST ], unit,
       unit * P2p_types.Peer_id.t, unit, unit,
       P2p_types.Connection_info.t option, unit) RPC.Service.t

    val kick :
      ([ `POST ], unit,
       unit * P2p_types.Peer_id.t, unit, bool,
       unit, unit) RPC.Service.t

  end

  module Point : sig
    val list :
      ([ `POST ], unit,
       unit, unit, P2p_types.Point_state.t list,
       (P2p_types.Point.t * P2p_types.Point_info.t) list, unit) RPC.Service.t
    val info :
      ([ `POST ], unit,
       unit * P2p_types.Point.t, unit, unit,
       P2p_types.Point_info.t option, unit) RPC.Service.t
    val events :
      ([ `POST ], unit,
       unit * P2p_types.Point.t, unit, bool,
       P2p_connection_pool_types.Point_info.Event.t list, unit) RPC.Service.t
  end

  module Peer_id : sig

    val list :
      ([ `POST ], unit,
       unit, unit, P2p_types.Peer_state.t list,
       (P2p_types.Peer_id.t * P2p_types.Peer_info.t) list, unit) RPC.Service.t

    val info :
      ([ `POST ], unit,
       unit * P2p_types.Peer_id.t, unit, unit,
       P2p_types.Peer_info.t option, unit) RPC.Service.t

    val events :
      ([ `POST ], unit,
       unit * P2p_types.Peer_id.t, unit, bool,
       P2p_connection_pool_types.Peer_info.Event.t list, unit) RPC.Service.t

  end

end

val forge_block_header:
  ([ `POST ], unit,
   unit, unit, Block_header.t,
   MBytes.t, unit) RPC.Service.t

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
   Block_hash.t tzresult, unit) RPC.Service.t

val inject_operation:
  ([ `POST ], unit,
   unit, unit, (MBytes.t * bool * Net_id.t option * bool option),
   Operation_hash.t tzresult, unit) RPC.Service.t

val inject_protocol:
  ([ `POST ], unit,
   unit, unit, (Protocol.t * bool * bool option),
   Protocol_hash.t tzresult, unit) RPC.Service.t

val bootstrapped:
  ([ `POST ], unit,
   unit, unit, unit,
   Block_hash.t * Time.t, unit) RPC.Service.t

val complete:
  ([ `POST ], unit,
   unit * string, unit, unit,
   string list, unit) RPC.Service.t

val describe: (unit, unit) RPC.Service.description_service
