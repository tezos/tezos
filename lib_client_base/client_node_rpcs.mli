(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_rpcs

val errors:
  #rpc_sig -> Json_schema.schema tzresult Lwt.t

val forge_block_header:
  #rpc_sig ->
  Block_header.t ->
  MBytes.t tzresult Lwt.t

val inject_block:
  #rpc_sig ->
  ?async:bool -> ?force:bool -> ?net_id:Net_id.t ->
  MBytes.t -> Operation.t list list ->
  Block_hash.t tzresult Lwt.t
(** [inject_block cctxt ?async ?force raw_block] tries to inject
    [raw_block] inside the node. If [?async] is [true], [raw_block]
    will be validated before the result is returned. If [?force] is
    true, the block will be injected even on non strictly increasing
    fitness. *)

val inject_operation:
  #rpc_sig ->
  ?async:bool -> ?force:bool -> ?net_id:Net_id.t ->
  MBytes.t ->
  Operation_hash.t tzresult Lwt.t

val inject_protocol:
  #rpc_sig ->
  ?async:bool -> ?force:bool ->
  Protocol.t ->
  Protocol_hash.t tzresult Lwt.t

module Blocks : sig

  type block = Node_rpc_services.Blocks.block

  val net_id:
    #rpc_sig ->
    block -> Net_id.t tzresult Lwt.t
  val level:
    #rpc_sig ->
    block -> Int32.t tzresult Lwt.t
  val predecessor:
    #rpc_sig ->
    block -> Block_hash.t tzresult Lwt.t
  val predecessors:
    #rpc_sig ->
    block -> int -> Block_hash.t list tzresult Lwt.t
  val hash:
    #rpc_sig ->
    block -> Block_hash.t tzresult Lwt.t
  val timestamp:
    #rpc_sig ->
    block -> Time.t tzresult Lwt.t
  val fitness:
    #rpc_sig ->
    block -> MBytes.t list tzresult Lwt.t
  val operations:
    #rpc_sig ->
    ?contents:bool ->
    block -> (Operation_hash.t * Operation.t option) list list tzresult Lwt.t
  val protocol:
    #rpc_sig ->
    block -> Protocol_hash.t tzresult Lwt.t
  val test_network:
    #rpc_sig ->
    block -> Test_network_status.t tzresult Lwt.t

  val pending_operations:
    #rpc_sig ->
    block ->
    (error Preapply_result.t * Operation.t Operation_hash.Map.t) tzresult Lwt.t

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
    #rpc_sig ->
    ?include_ops:bool -> block -> block_info tzresult Lwt.t

  val list:
    #rpc_sig ->
    ?include_ops:bool -> ?length:int -> ?heads:Block_hash.t list ->
    ?delay:int -> ?min_date:Time.t -> ?min_heads:int ->
    unit -> block_info list list tzresult Lwt.t

  val monitor:
    #rpc_sig ->
    ?include_ops:bool -> ?length:int -> ?heads:Block_hash.t list ->
    ?delay:int -> ?min_date:Time.t -> ?min_heads:int ->
    unit -> block_info list list tzresult Lwt_stream.t tzresult Lwt.t

  type preapply_result = {
    shell_header: Block_header.shell_header ;
    operations: error Preapply_result.t ;
  }

  val preapply:
    #rpc_sig ->
    block ->
    ?timestamp:Time.t ->
    ?sort:bool ->
    proto_header:MBytes.t ->
    Operation.t list -> preapply_result tzresult Lwt.t

end

module Operations : sig

  val monitor:
    #rpc_sig ->
    ?contents:bool ->
    unit ->
    (Operation_hash.t * Operation.t option) list list tzresult Lwt_stream.t tzresult Lwt.t

end

module Protocols : sig

  val contents:
    #rpc_sig ->
    Protocol_hash.t -> Protocol.t tzresult Lwt.t

  val list:
    #rpc_sig ->
    ?contents:bool -> unit ->
    (Protocol_hash.t * Protocol.t option) list tzresult Lwt.t

end

val bootstrapped:
  #rpc_sig -> (Block_hash.t * Time.t) tzresult Lwt_stream.t tzresult Lwt.t

module Network : sig

  open P2p_types

  val stat:
    #rpc_sig -> Stat.t tzresult Lwt.t

  val connections:
    #rpc_sig -> Connection_info.t list tzresult Lwt.t

  val peers:
    #rpc_sig -> (Peer_id.t * P2p_types.Peer_info.t) list tzresult Lwt.t

  val points:
    #rpc_sig -> (Point.t * P2p_types.Point_info.t) list tzresult Lwt.t

end

val complete:
  #rpc_sig ->
  ?block:Blocks.block -> string -> string list tzresult Lwt.t

val describe:
  #rpc_sig ->
  ?recurse:bool -> string list ->
  Data_encoding.json_schema RPC.Description.directory tzresult Lwt.t
