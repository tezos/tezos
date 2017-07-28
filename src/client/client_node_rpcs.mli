(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_rpcs

val errors:
  config -> Json_schema.schema tzresult Lwt.t

val forge_block_header:
  config ->
  Block_header.t ->
  MBytes.t tzresult Lwt.t

val validate_block:
  config ->
  Net_id.t -> Block_hash.t ->
  unit tzresult Lwt.t

type operation =
  | Blob of Operation.t
  | Hash of Operation_hash.t

val operation_encoding: operation Data_encoding.t

val inject_block:
  config ->
  ?async:bool -> ?force:bool ->
  MBytes.t -> operation list list ->
  Block_hash.t tzresult Lwt.t
(** [inject_block cctxt ?async ?force raw_block] tries to inject
    [raw_block] inside the node. If [?async] is [true], [raw_block]
    will be validated before the result is returned. If [?force] is
    true, the block will be injected even on non strictly increasing
    fitness. *)

val inject_operation:
  config ->
  ?async:bool -> ?force:bool ->
  MBytes.t ->
  Operation_hash.t tzresult Lwt.t

val inject_protocol:
  config ->
  ?async:bool -> ?force:bool ->
  Protocol.t ->
  Protocol_hash.t tzresult Lwt.t

module Blocks : sig

  type block = Node_rpc_services.Blocks.block

  val net_id:
    config ->
    block -> Net_id.t tzresult Lwt.t
  val level:
    config ->
    block -> Int32.t tzresult Lwt.t
  val predecessor:
    config ->
    block -> Block_hash.t tzresult Lwt.t
  val predecessors:
    config ->
    block -> int -> Block_hash.t list tzresult Lwt.t
  val hash:
    config ->
    block -> Block_hash.t tzresult Lwt.t
  val timestamp:
    config ->
    block -> Time.t tzresult Lwt.t
  val fitness:
    config ->
    block -> MBytes.t list tzresult Lwt.t
  val operations:
    config ->
    ?contents:bool ->
    block -> (Operation_hash.t * Operation.t option) list list tzresult Lwt.t
  val protocol:
    config ->
    block -> Protocol_hash.t tzresult Lwt.t
  val test_network:
    config ->
    block -> Context.test_network tzresult Lwt.t

  val pending_operations:
    config ->
    block ->
    (error Prevalidation.preapply_result * Operation_hash.Set.t) tzresult Lwt.t

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
    config ->
    ?include_ops:bool -> block -> block_info tzresult Lwt.t

  val list:
    config ->
    ?include_ops:bool -> ?length:int -> ?heads:Block_hash.t list ->
    ?delay:int -> ?min_date:Time.t -> ?min_heads:int ->
    unit -> block_info list list tzresult Lwt.t

  val monitor:
    config ->
    ?include_ops:bool -> ?length:int -> ?heads:Block_hash.t list ->
    ?delay:int -> ?min_date:Time.t -> ?min_heads:int ->
    unit -> block_info list list tzresult Lwt_stream.t tzresult Lwt.t

  type preapply_result = {
    shell_header: Block_header.shell_header ;
    operations: error Prevalidation.preapply_result ;
  }

  val preapply:
    config ->
    block ->
    ?timestamp:Time.t ->
    ?sort:bool ->
    proto_header:MBytes.t ->
    operation list -> preapply_result tzresult Lwt.t

end

module Operations : sig

  val monitor:
    config ->
    ?contents:bool ->
    unit ->
    (Operation_hash.t * Operation.t option) list list tzresult Lwt_stream.t tzresult Lwt.t

end

module Protocols : sig

  val contents:
    config ->
    Protocol_hash.t -> Protocol.t tzresult Lwt.t

  val list:
    config ->
    ?contents:bool -> unit ->
    (Protocol_hash.t * Protocol.t option) list tzresult Lwt.t

end

val bootstrapped:
  config -> (Block_hash.t * Time.t) tzresult Lwt_stream.t tzresult Lwt.t

module Network : sig

  val stat:
    config -> P2p_types.Stat.t tzresult Lwt.t

  val connections:
    config -> P2p_types.Connection_info.t list tzresult Lwt.t

  val peers:
    config -> (P2p.Peer_id.t * P2p.RPC.Peer_id.info) list tzresult Lwt.t

  val points:
    config -> (P2p.Point.t * P2p.RPC.Point.info) list tzresult Lwt.t

end

val complete:
  config ->
  ?block:Blocks.block -> string -> string list tzresult Lwt.t

val describe:
  config ->
  ?recurse:bool -> string list ->
  RPC.Description.directory_descr tzresult Lwt.t
