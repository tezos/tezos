(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val errors:
  #Client_rpcs.ctxt -> Json_schema.schema tzresult Lwt.t

val forge_block_header:
  #Client_rpcs.ctxt ->
  Block_header.t ->
  MBytes.t tzresult Lwt.t

val inject_block:
  #Client_rpcs.ctxt ->
  ?async:bool -> ?force:bool -> ?net_id:Net_id.t ->
  MBytes.t -> Operation.t list list ->
  Block_hash.t tzresult Lwt.t
(** [inject_block cctxt ?async ?force raw_block] tries to inject
    [raw_block] inside the node. If [?async] is [true], [raw_block]
    will be validated before the result is returned. If [?force] is
    true, the block will be injected even on non strictly increasing
    fitness. *)

val inject_operation:
  #Client_rpcs.ctxt ->
  ?async:bool -> ?net_id:Net_id.t ->
  MBytes.t ->
  Operation_hash.t tzresult Lwt.t

val inject_protocol:
  #Client_rpcs.ctxt ->
  ?async:bool -> ?force:bool ->
  Protocol.t ->
  Protocol_hash.t tzresult Lwt.t

module Blocks : sig

  type block = Block_services.block

  val net_id:
    #Client_rpcs.ctxt ->
    block -> Net_id.t tzresult Lwt.t
  val level:
    #Client_rpcs.ctxt ->
    block -> Int32.t tzresult Lwt.t
  val predecessor:
    #Client_rpcs.ctxt ->
    block -> Block_hash.t tzresult Lwt.t
  val predecessors:
    #Client_rpcs.ctxt ->
    block -> int -> Block_hash.t list tzresult Lwt.t
  val hash:
    #Client_rpcs.ctxt ->
    block -> Block_hash.t tzresult Lwt.t
  val timestamp:
    #Client_rpcs.ctxt ->
    block -> Time.t tzresult Lwt.t
  val fitness:
    #Client_rpcs.ctxt ->
    block -> MBytes.t list tzresult Lwt.t
  val operations:
    #Client_rpcs.ctxt ->
    ?contents:bool ->
    block -> (Operation_hash.t * Operation.t option) list list tzresult Lwt.t
  val protocol:
    #Client_rpcs.ctxt ->
    block -> Protocol_hash.t tzresult Lwt.t
  val test_network:
    #Client_rpcs.ctxt ->
    block -> Test_network_status.t tzresult Lwt.t

  val pending_operations:
    #Client_rpcs.ctxt ->
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
    context: Context_hash.t ;
    data: MBytes.t ;
    operations: (Operation_hash.t * Operation.t) list list option ;
    protocol: Protocol_hash.t ;
    test_network: Test_network_status.t ;
  }

  val info:
    #Client_rpcs.ctxt ->
    ?include_ops:bool -> block -> block_info tzresult Lwt.t

  val list:
    #Client_rpcs.ctxt ->
    ?include_ops:bool -> ?length:int -> ?heads:Block_hash.t list ->
    ?delay:int -> ?min_date:Time.t -> ?min_heads:int ->
    unit -> block_info list list tzresult Lwt.t

  val monitor:
    #Client_rpcs.ctxt ->
    ?include_ops:bool -> ?length:int -> ?heads:Block_hash.t list ->
    ?delay:int -> ?min_date:Time.t -> ?min_heads:int ->
    unit -> block_info list list Lwt_stream.t tzresult Lwt.t

  type preapply_result = {
    shell_header: Block_header.shell_header ;
    operations: error Preapply_result.t list ;
  }

  val preapply:
    #Client_rpcs.ctxt ->
    block ->
    ?timestamp:Time.t ->
    ?sort:bool ->
    proto_header:MBytes.t ->
    Operation.t list list -> preapply_result tzresult Lwt.t

end

module Operations : sig

  val monitor:
    #Client_rpcs.ctxt ->
    ?contents:bool ->
    unit ->
    (Operation_hash.t * Operation.t option) list list Lwt_stream.t tzresult Lwt.t

end

module Protocols : sig

  val contents:
    #Client_rpcs.ctxt ->
    Protocol_hash.t -> Protocol.t tzresult Lwt.t

  val list:
    #Client_rpcs.ctxt ->
    ?contents:bool -> unit ->
    (Protocol_hash.t * Protocol.t option) list tzresult Lwt.t

end

val bootstrapped:
  #Client_rpcs.ctxt -> (Block_hash.t * Time.t) Lwt_stream.t tzresult Lwt.t

module Network : sig

  val stat:
    #Client_rpcs.ctxt -> P2p_stat.t tzresult Lwt.t

  val connections:
    #Client_rpcs.ctxt -> P2p_connection.Info.t list tzresult Lwt.t

  val peers:
    #Client_rpcs.ctxt -> (P2p_peer.Id.t * P2p_peer.Info.t) list tzresult Lwt.t

  val points:
    #Client_rpcs.ctxt -> (P2p_point.Id.t * P2p_point.Info.t) list tzresult Lwt.t

end

val complete:
  #Client_rpcs.ctxt ->
  ?block:Blocks.block -> string -> string list tzresult Lwt.t

val describe:
  #Client_rpcs.ctxt ->
  ?recurse:bool -> string list ->
  Data_encoding.json_schema RPC_description.directory tzresult Lwt.t
