(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t

type config = {
  genesis: State.Net.genesis ;
  store_root: string ;
  context_root: string ;
  patch_context: (Context.t -> Context.t Lwt.t) option ;
  p2p: (P2p.config * P2p.limits) option ;
  test_network_max_tll: int option ;
  bootstrap_threshold: int ;
}

and timeout = {
  operation: float ;
  block_header: float ;
  block_operations: float ;
  protocol: float ;
  new_head_request: float ;
}

val create: config -> timeout -> t tzresult Lwt.t

module RPC : sig

  type block = Node_rpc_services.Blocks.block
  type block_info = Node_rpc_services.Blocks.block_info

  val inject_block:
    t -> ?force:bool ->
    MBytes.t -> Distributed_db.operation list list ->
    (Block_hash.t * unit tzresult Lwt.t) tzresult Lwt.t
  (** [inject_block node ?force bytes] tries to insert [bytes]
      (supposedly the serialization of a block header) inside
      [node]. If [?force] is true, the block will be inserted even on
      non strictly increasing fitness. *)

  val inject_operation:
    t -> ?force:bool -> MBytes.t ->
    (Operation_hash.t * unit tzresult Lwt.t) Lwt.t
  val inject_protocol:
    t -> ?force:bool -> Protocol.t ->
    (Protocol_hash.t * unit tzresult Lwt.t) Lwt.t

  val raw_block_info:
    t -> Block_hash.t -> block_info Lwt.t
  val block_header_watcher:
    t -> (Block_hash.t * Block_header.t) Lwt_stream.t * Watcher.stopper
  val block_watcher:
    t -> (block_info Lwt_stream.t * Watcher.stopper)
  val heads: t -> block_info Block_hash.Map.t Lwt.t

  val predecessors:
    t -> int -> Block_hash.t -> Block_hash.t list Lwt.t

  val list:
    t -> int -> Block_hash.t list -> block_info list list Lwt.t

  val block_info:
    t -> block -> block_info Lwt.t

  val operation_hashes:
    t -> block -> Operation_hash.t list list Lwt.t
  val operations:
    t -> block -> Operation.t list list Lwt.t
  val operation_watcher:
    t -> (Operation_hash.t * Operation.t) Lwt_stream.t * Watcher.stopper

  val pending_operations:
    t -> block -> (error Prevalidation.preapply_result * Operation_hash.Set.t) Lwt.t

  val protocols:
    t -> Protocol_hash.t list Lwt.t
  val protocol_content:
    t -> Protocol_hash.t -> Protocol.t tzresult Lwt.t
  val protocol_watcher:
    t -> (Protocol_hash.t * Protocol.t) Lwt_stream.t * Watcher.stopper

  val context_dir:
    t -> block -> 'a RPC.directory option Lwt.t

  val preapply:
    t -> block ->
    timestamp:Time.t -> proto_header:MBytes.t ->
    sort_operations:bool -> Distributed_db.operation list ->
    (Block_header.shell_header * error Prevalidation.preapply_result) tzresult Lwt.t

  val context_dir:
    t -> block -> 'a RPC.directory option Lwt.t

  val complete:
    t -> ?block:block -> string -> string list Lwt.t

  val bootstrapped:
    t -> (Block_hash.t * Time.t) RPC.Answer.stream

  module Network : sig

    val stat : t -> P2p.Stat.t
    val watch : t -> P2p.RPC.Event.t Lwt_stream.t * Watcher.stopper
    val connect : t -> P2p.Point.t -> float -> unit tzresult Lwt.t

    module Connection : sig
      val info : t -> P2p.Peer_id.t -> P2p.Connection_info.t option
      val kick : t -> P2p.Peer_id.t -> bool -> unit Lwt.t
      val list : t -> P2p.Connection_info.t list
      val count : t -> int
    end

    module Peer_id : sig
      val list : t ->
        P2p.RPC.Peer_id.state list -> (P2p.Peer_id.t * P2p.RPC.Peer_id.info) list
      val info : t -> P2p.Peer_id.t -> P2p.RPC.Peer_id.info option
      val events : t -> P2p.Peer_id.t -> P2p.RPC.Peer_id.Event.t list
      val watch : t -> P2p.Peer_id.t ->
        P2p.RPC.Peer_id.Event.t Lwt_stream.t * Watcher.stopper
    end

    module Point : sig
      val list : t ->
        P2p.RPC.Point.state list -> (P2p.Point.t * P2p.RPC.Point.info) list
      val info : t -> P2p.Point.t -> P2p.RPC.Point.info option
      val events : t -> P2p.Point.t -> P2p.RPC.Point.Event.t list
      val watch : t -> P2p.Point.t ->
        P2p.RPC.Point.Event.t Lwt_stream.t * Watcher.stopper
    end

  end

end

val shutdown: t -> unit Lwt.t
