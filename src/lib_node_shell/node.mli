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
  block_header: float ;
  block_operations: float ;
  protocol: float ;
  new_head_request: float ;
}
and prevalidator_limits = {
  max_refused_operations: int ;
  operation_timeout: float
}

val create: config -> timeout -> prevalidator_limits -> t tzresult Lwt.t

module RPC : sig

  type block = Node_rpc_services.Blocks.block
  type block_info = Node_rpc_services.Blocks.block_info

  val inject_block:
    t -> ?force:bool -> ?net_id:Net_id.t ->
    MBytes.t -> Operation.t list list ->
    (Block_hash.t * unit tzresult Lwt.t) tzresult Lwt.t
  (** [inject_block node ?force bytes] tries to insert [bytes]
      (supposedly the serialization of a block header) inside
      [node]. If [?force] is true, the block will be inserted even on
      non strictly increasing fitness. *)

  val inject_operation:
    t -> ?net_id:Net_id.t -> MBytes.t ->
    (Operation_hash.t * unit tzresult Lwt.t) Lwt.t
  val inject_protocol:
    t -> ?force:bool -> Protocol.t ->
    (Protocol_hash.t * unit tzresult Lwt.t) Lwt.t

  val raw_block_info:
    t -> Block_hash.t -> block_info Lwt.t
  val block_header_watcher:
    t -> (Block_hash.t * Block_header.t) Lwt_stream.t * Lwt_watcher.stopper
  val block_watcher:
    t -> (block_info Lwt_stream.t * Lwt_watcher.stopper)
  val heads: t -> block_info Block_hash.Map.t Lwt.t

  val predecessors:
    t -> int -> Block_hash.t -> Block_hash.t list Lwt.t

  val list:
    t -> int -> Block_hash.t list -> block_info list list Lwt.t

  val list_invalid:
    t -> (Block_hash.t * int32 * error list) list Lwt.t

  val unmark_invalid:
    t -> Block_hash.t -> unit tzresult Lwt.t

  val block_info:
    t -> block -> block_info Lwt.t

  val operation_hashes:
    t -> block -> Operation_hash.t list list Lwt.t
  val operations:
    t -> block -> Operation.t list list Lwt.t
  val operation_watcher:
    t -> (Operation_hash.t * Operation.t) Lwt_stream.t * Lwt_watcher.stopper

  val pending_operations:
    t -> block ->
    (error Preapply_result.t * Operation.t Operation_hash.Map.t) Lwt.t

  val protocols:
    t -> Protocol_hash.t list Lwt.t
  val protocol_content:
    t -> Protocol_hash.t -> Protocol.t tzresult Lwt.t
  val protocol_watcher:
    t -> (Protocol_hash.t * Protocol.t) Lwt_stream.t * Lwt_watcher.stopper

  val context_dir:
    t -> block -> 'a RPC_directory.t option Lwt.t

  val preapply:
    t -> block ->
    timestamp:Time.t -> proto_header:MBytes.t ->
    sort_operations:bool -> Operation.t list list ->
    (Block_header.shell_header * error Preapply_result.t list) tzresult Lwt.t

  val context_dir:
    t -> block -> 'a RPC_directory.t option Lwt.t

  val complete:
    t -> ?block:block -> string -> string list Lwt.t

  val bootstrapped:
    t -> (Block_hash.t * Time.t) RPC_answer.stream

  module Network : sig

    open P2p_types

    val stat : t -> Stat.t

    val watch :
      t ->
      P2p_types.Connection_pool_log_event.t Lwt_stream.t * Lwt_watcher.stopper
    val connect : t -> Point.t -> float -> unit tzresult Lwt.t

    module Connection : sig
      val info : t -> Peer_id.t -> Connection_info.t option
      val kick : t -> Peer_id.t -> bool -> unit Lwt.t
      val list : t -> Connection_info.t list
      val count : t -> int
    end

    module Point : sig

      val info :
        t -> Point.t -> P2p_types.Point_info.t option

      val list :
        ?restrict: P2p_types.Point_state.t list ->
        t -> (Point.t * P2p_types.Point_info.t) list

      val events :
        ?max:int -> ?rev:bool -> t -> Point.t ->
        P2p_connection_pool_types.Point_info.Event.t list

      val watch :
        t -> Point.t ->
        P2p_connection_pool_types.Point_info.Event.t Lwt_stream.t * Lwt_watcher.stopper

    end

    module Peer_id : sig

      val info :
        t -> Peer_id.t -> P2p_types.Peer_info.t option

      val list :
        ?restrict: P2p_types.Peer_state.t list ->
        t -> (Peer_id.t * P2p_types.Peer_info.t) list

      val events :
        ?max: int -> ?rev: bool ->
        t -> Peer_id.t ->
        P2p_connection_pool_types.Peer_info.Event.t list

      val watch :
        t -> Peer_id.t ->
        P2p_connection_pool_types.Peer_info.Event.t Lwt_stream.t * Lwt_watcher.stopper

    end

  end

end

val shutdown: t -> unit Lwt.t
