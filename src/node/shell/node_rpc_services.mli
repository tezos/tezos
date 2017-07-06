(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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

type operation = Distributed_db.operation =
  | Blob of Operation.t
  | Hash of Operation_hash.t

val operation_encoding: operation Data_encoding.t

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
    operations_hash: Operation_list_list_hash.t ;
    fitness: MBytes.t list ;
    data: MBytes.t ;
    operations: Operation_hash.t list list option ;
    protocol: Protocol_hash.t ;
    test_network: Context.test_network;
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
    (unit, unit * block, unit, Context.test_network) RPC.service
  val pending_operations:
    (unit, unit * block, unit,
     error Prevalidation.preapply_result * Hash.Operation_hash.Set.t) RPC.service

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

  type preapply_param = {
    timestamp: Time.t ;
    proto_header: MBytes.t ;
    operations: operation list ;
    sort_operations: bool ;
  }

  type preapply_result = {
    shell_header: Block_header.shell_header ;
    operations: error Prevalidation.preapply_result ;
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
    (unit, unit, unit, P2p.Stat.t) RPC.service

  val versions :
    (unit, unit, unit, P2p.Version.t list) RPC.service

  val events :
    (unit, unit, unit, P2p.RPC.Event.t) RPC.service

  val connect :
    (unit, unit * P2p.Point.t, float, unit tzresult) RPC.service

  module Connection : sig
    val list :
      (unit, unit, unit, P2p.Connection_info.t list) RPC.service
    val info :
      (unit, unit * P2p.Peer_id.t, unit, P2p.Connection_info.t option) RPC.service
    val kick :
      (unit, unit * P2p.Peer_id.t, bool, unit) RPC.service
  end

  module Point : sig
    val list :
      (unit, unit, P2p.RPC.Point.state list,
       (P2p.Point.t * P2p.RPC.Point.info) list) RPC.service
    val info :
      (unit, unit * P2p.Point.t, unit, P2p.RPC.Point.info option) RPC.service
    val events :
      (unit, unit * P2p.Point.t, bool, P2p.RPC.Point.Event.t list) RPC.service
  end

  module Peer_id : sig
    val list :
      (unit, unit, P2p.RPC.Peer_id.state list,
       (P2p.Peer_id.t * P2p.RPC.Peer_id.info) list) RPC.service
    val info :
      (unit, unit * P2p.Peer_id.t, unit, P2p.RPC.Peer_id.info option) RPC.service
    val events :
      (unit, unit * P2p.Peer_id.t, bool, P2p.RPC.Peer_id.Event.t list) RPC.service
  end

end

val forge_block_header:
  (unit, unit, Block_header.t, MBytes.t) RPC.service

val validate_block:
  (unit, unit, Net_id.t * Block_hash.t, unit tzresult) RPC.service

type inject_block_param = {
  raw: MBytes.t ;
  blocking: bool ;
  force: bool ;
  operations: operation list list ;
}

val inject_block:
  (unit, unit, inject_block_param, Block_hash.t tzresult) RPC.service

val inject_operation:
  (unit, unit,
   (MBytes.t * bool * bool option), Operation_hash.t tzresult) RPC.service

val inject_protocol:
  (unit, unit,
   (Protocol.t * bool * bool option),
   Protocol_hash.t tzresult) RPC.service

val bootstrapped: (unit, unit, unit, Block_hash.t * Time.t) RPC.service

val complete: (unit, unit * string, unit, string list) RPC.service

val describe:
  (unit, unit, bool option, RPC.Description.directory_descr) RPC.service
