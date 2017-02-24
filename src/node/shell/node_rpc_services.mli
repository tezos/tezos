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

module Blocks : sig

  type block = [
    | `Genesis
    | `Head of int | `Prevalidation
    | `Test_head of int | `Test_prevalidation
    | `Hash of Block_hash.t
    ]
  val blocks_arg : block RPC.Arg.arg

  val parse_block: string -> (block, string) result
  type net = State.Net_id.t = Id of Block_hash.t

  type block_info = {
    hash: Block_hash.t ;
    predecessor: Block_hash.t ;
    fitness: MBytes.t list ;
    timestamp: Time.t ;
    protocol: Protocol_hash.t option ;
    operations: Operation_hash.t list option ;
    net: net ;
    test_protocol: Protocol_hash.t option ;
    test_network: (net * Time.t) option ;
  }

  val info:
    (unit, unit * block, bool, block_info) RPC.service
  val net:
    (unit, unit * block, unit, net) RPC.service
  val predecessor:
    (unit, unit * block, unit, Block_hash.t) RPC.service
  val hash:
    (unit, unit * block, unit, Block_hash.t) RPC.service
  val timestamp:
    (unit, unit * block, unit, Time.t) RPC.service
  val fitness:
    (unit, unit * block, unit, MBytes.t list) RPC.service
  val operations:
    (unit, unit * block, unit, Operation_hash.t list) RPC.service
  val protocol:
    (unit, unit * block, unit, Protocol_hash.t) RPC.service
  val test_protocol:
    (unit, unit * block, unit, Protocol_hash.t option) RPC.service
  val test_network:
    (unit, unit * block, unit, (net * Time.t) option) RPC.service
  val pending_operations:
    (unit, unit * block, unit,
     error Updater.preapply_result * Hash.Operation_hash.Set.t) RPC.service

  type list_param = {
    operations: bool option ;
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
    operations: Operation_hash.t list ;
    sort: bool ;
    timestamp: Time.t option ;
  }
  type preapply_result = {
    operations: error Updater.preapply_result ;
    fitness: MBytes.t list ;
    timestamp: Time.t ;
  }
  val preapply:
    (unit, unit * block, preapply_param, preapply_result tzresult) RPC.service

  val complete: (unit, (unit * block) * string, unit, string list) RPC.service

  val proto_path: (unit, unit * block) RPC.Path.path

end

module Operations : sig
  val bytes:
    (unit, unit * Operation_hash.t, unit, State.Operation.t) RPC.service
  type list_param = {
    contents: bool option ;
    monitor: bool option ;
  }
  val list:
    (unit, unit,
     list_param, (Operation_hash.t * Store.Operation.t option) list) RPC.service
end

module Protocols : sig
  val bytes:
    (unit, unit * Protocol_hash.t, unit, Tezos_compiler.Protocol.t) RPC.service
  type list_param = {
    contents: bool option ;
    monitor: bool option ;
  }
  val list:
    (unit, unit,
     list_param,
     (Protocol_hash.t * Tezos_compiler.Protocol.t option) list) RPC.service
end

module Network : sig
  val stat :
    (unit, unit, unit, P2p.Stat.t) RPC.service

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
    val infos :
      (unit, unit, P2p.RPC.Point.state list,
       (P2p.Point.t * P2p.RPC.Point.info) list) RPC.service
    val info :
      (unit, unit * P2p.Point.t, unit, P2p.RPC.Point.info option) RPC.service
    val events :
      (unit, unit * P2p.Point.t, bool, P2p.RPC.Point.Event.t list) RPC.service
  end

  module Peer_id : sig
    val infos :
      (unit, unit, P2p.RPC.Peer_id.state list,
       (P2p.Peer_id.t * P2p.RPC.Peer_id.info) list) RPC.service
    val info :
      (unit, unit * P2p.Peer_id.t, unit, P2p.RPC.Peer_id.info option) RPC.service
    val events :
      (unit, unit * P2p.Peer_id.t, bool, P2p.RPC.Peer_id.Event.t list) RPC.service
  end
end

val forge_block:
  (unit, unit,
   Updater.Net_id.t option * Block_hash.t option * Time.t option *
   Fitness.fitness * Operation_hash.t list * MBytes.t,
   MBytes.t) RPC.service

val validate_block:
  (unit, unit, Blocks.net * Block_hash.t, unit tzresult) RPC.service

val inject_block:
  (unit, unit,
   (MBytes.t * bool * bool option),
   Block_hash.t tzresult) RPC.service

val inject_operation:
  (unit, unit,
   (MBytes.t * bool * bool option), Operation_hash.t tzresult) RPC.service

val inject_protocol:
  (unit, unit,
   (Tezos_compiler.Protocol.t * bool * bool option),
   Protocol_hash.t tzresult) RPC.service

val complete: (unit, unit * string, unit, string list) RPC.service

val describe:
  (unit, unit, bool option, RPC.Description.directory_descr) RPC.service
