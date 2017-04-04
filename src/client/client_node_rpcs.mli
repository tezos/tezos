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

val forge_block:
  config ->
  ?net:Net_id.t ->
  ?predecessor:Block_hash.t ->
  ?timestamp:Time.t ->
  Fitness.fitness ->
  Operation_list_list_hash.t ->
  MBytes.t ->
  MBytes.t tzresult Lwt.t
(** [forge_block cctxt ?net ?predecessor ?timestamp fitness ops
    proto_hdr] returns the serialization of a block header with
    [proto_hdr] as protocol-specific part. The arguments [?net] and
    [?predecessor] are infered from the current head of main network,
    and [?timestamp] defaults to [Time.now ()]. *)

val validate_block:
  config ->
  Net_id.t -> Block_hash.t ->
  unit tzresult Lwt.t

val inject_block:
  config ->
  ?async:bool -> ?force:bool ->
  MBytes.t -> Operation_hash.t list list ->
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
  Tezos_compiler.Protocol.t ->
  Protocol_hash.t tzresult Lwt.t

module Blocks : sig

  type block = [
    | `Genesis
    | `Head of int | `Prevalidation
    | `Test_head of int | `Test_prevalidation
    | `Hash of Block_hash.t
    ]

  val net:
    config ->
    block -> Net_id.t tzresult Lwt.t
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
    block -> Operation_hash.t list list tzresult Lwt.t
  val protocol:
    config ->
    block -> Protocol_hash.t tzresult Lwt.t
  val test_protocol:
    config ->
    block -> Protocol_hash.t option tzresult Lwt.t
  val test_network:
    config ->
    block -> (Net_id.t * Time.t) option tzresult Lwt.t

  val pending_operations:
    config ->
    block ->
    (error Updater.preapply_result * Operation_hash.Set.t) tzresult Lwt.t

  type block_info = {
    hash: Block_hash.t ;
    predecessor: Block_hash.t ;
    fitness: MBytes.t list ;
    timestamp: Time.t ;
    protocol: Protocol_hash.t option ;
    operations_hash: Operation_list_list_hash.t ;
    operations: Operation_hash.t list list option ;
    data: MBytes.t option ;
    net: Net_id.t ;
    test_protocol: Protocol_hash.t option ;
    test_network: (Net_id.t * Time.t) option ;
  }

  val info:
    config ->
    ?operations:bool -> ?data:bool -> block -> block_info tzresult Lwt.t

  val list:
    config ->
    ?operations:bool -> ?data:bool -> ?length:int -> ?heads:Block_hash.t list ->
    ?delay:int -> ?min_date:Time.t -> ?min_heads:int ->
    unit -> block_info list list tzresult Lwt.t

  val monitor:
    config ->
    ?operations:bool -> ?data:bool -> ?length:int -> ?heads:Block_hash.t list ->
    ?delay:int -> ?min_date:Time.t -> ?min_heads:int ->
    unit -> block_info list list tzresult Lwt_stream.t tzresult Lwt.t

  type preapply_result = {
    operations: error Updater.preapply_result ;
    fitness: MBytes.t list ;
    timestamp: Time.t ;
  }

  val preapply:
    config ->
    block ->
    ?timestamp:Time.t ->
    ?sort:bool ->
    Hash.Operation_hash.t list -> preapply_result tzresult Lwt.t

end

module Operations : sig

  val contents:
    config ->
    Operation_hash.t list -> Store.Operation.t list tzresult Lwt.t

  val monitor:
    config ->
    ?contents:bool -> unit ->
    (Operation_hash.t * Store.Operation.t option) list list tzresult
      Lwt_stream.t tzresult Lwt.t

end

module Protocols : sig

  val contents:
    config ->
    Protocol_hash.t -> Store.Protocol.t tzresult Lwt.t

  val list:
    config ->
    ?contents:bool -> unit ->
    (Protocol_hash.t * Store.Protocol.t option) list tzresult Lwt.t

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
