(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val errors:
  Client_commands.context ->
  Json_schema.schema Lwt.t

val forge_block:
  Client_commands.context ->
  ?net:Updater.Net_id.t ->
  ?predecessor:Block_hash.t ->
  ?timestamp:Time.t ->
  Fitness.fitness ->
  Operation_hash.t list ->
  MBytes.t ->
  MBytes.t Lwt.t
(** [forge_block cctxt ?net ?predecessor ?timestamp fitness ops
    proto_hdr] returns the serialization of a block header with
    [proto_hdr] as protocol-specific part. The arguments [?net] and
    [?predecessor] are infered from the current head of main network,
    and [?timestamp] defaults to [Time.now ()]. *)

val validate_block:
  Client_commands.context ->
  Updater.Net_id.t -> Block_hash.t ->
  unit tzresult Lwt.t

val inject_block:
  Client_commands.context ->
  ?wait:bool -> ?force:bool ->
  MBytes.t ->
  Block_hash.t tzresult Lwt.t
(** [inject_block cctxt ?wait ?force raw_block] tries to inject
    [raw_block] inside the node. If [?wait] is [true], [raw_block]
    will be validated before the result is returned. If [?force] is
    true, the block will be injected even on non strictly increasing
    fitness. *)

val inject_operation:
  Client_commands.context ->
  ?wait:bool -> ?force:bool ->
  MBytes.t ->
  Operation_hash.t tzresult Lwt.t

val inject_protocol:
  Client_commands.context ->
  ?wait:bool -> ?force:bool ->
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
    Client_commands.context ->
    block -> Updater.Net_id.t Lwt.t
  val predecessor:
    Client_commands.context ->
    block -> Block_hash.t Lwt.t
  val predecessors:
    Client_commands.context ->
    block -> int -> Block_hash.t list Lwt.t
  val hash:
    Client_commands.context ->
    block -> Block_hash.t Lwt.t
  val timestamp:
    Client_commands.context ->
    block -> Time.t Lwt.t
  val fitness:
    Client_commands.context ->
    block -> MBytes.t list Lwt.t
  val operations:
    Client_commands.context ->
    block -> Operation_hash.t list Lwt.t
  val protocol:
    Client_commands.context ->
    block -> Protocol_hash.t Lwt.t
  val test_protocol:
    Client_commands.context ->
    block -> Protocol_hash.t option Lwt.t
  val test_network:
    Client_commands.context ->
    block -> (Updater.Net_id.t * Time.t) option Lwt.t

  val pending_operations:
    Client_commands.context ->
    block -> (error Updater.preapply_result * Operation_hash.Set.t) Lwt.t

  type block_info = {
    hash: Block_hash.t ;
    predecessor: Block_hash.t ;
    fitness: MBytes.t list ;
    timestamp: Time.t ;
    protocol: Protocol_hash.t option ;
    operations: Operation_hash.t list option ;
    net: Updater.Net_id.t ;
    test_protocol: Protocol_hash.t option ;
    test_network: (Updater.Net_id.t * Time.t) option ;
  }

  val info:
    Client_commands.context ->
    ?operations:bool -> block -> block_info Lwt.t

  val list:
    Client_commands.context ->
    ?operations:bool -> ?length:int -> ?heads:Block_hash.t list ->
    ?delay:int -> ?min_date:Time.t -> ?min_heads:int ->
    unit -> block_info list list Lwt.t

  val monitor:
    Client_commands.context ->
    ?operations:bool -> ?length:int -> ?heads:Block_hash.t list ->
    ?delay:int -> ?min_date:Time.t -> ?min_heads:int ->
    unit -> block_info list list Lwt_stream.t Lwt.t

  type preapply_result = {
    operations: error Updater.preapply_result ;
    fitness: MBytes.t list ;
    timestamp: Time.t ;
  }

  val preapply:
    Client_commands.context ->
    block ->
    ?timestamp:Time.t ->
    ?sort:bool ->
    Hash.Operation_hash.t list -> preapply_result tzresult Lwt.t

end

module Operations : sig
  val monitor:
    Client_commands.context ->
    ?contents:bool -> unit ->
    (Operation_hash.t * Store.Operation.t option) list Lwt_stream.t Lwt.t
end

module Protocols : sig
  val bytes:
    Client_commands.context ->
    Protocol_hash.t -> Store.Protocol.t Lwt.t

  val list:
    Client_commands.context ->
    ?contents:bool -> unit ->
    (Protocol_hash.t * Store.Protocol.t option) list Lwt.t
end

val bootstrapped:
  Client_commands.context -> (Block_hash.t * Time.t) Lwt_stream.t Lwt.t

module Network : sig
  val stat:
    Client_commands.context -> P2p_types.Stat.t Lwt.t
  val connections:
    Client_commands.context -> P2p_types.Connection_info.t list Lwt.t
  val peers:
    Client_commands.context -> (P2p.Peer_id.t * P2p.RPC.Peer_id.info) list Lwt.t
  val points:
    Client_commands.context -> (P2p.Point.t * P2p.RPC.Point.info) list Lwt.t
end

val complete:
  Client_commands.context ->
  ?block:Blocks.block -> string -> string list Lwt.t

val describe:
  Client_commands.context ->
  ?recurse:bool -> string list -> RPC.Description.directory_descr Lwt.t

(** Low-level *)

val get_json:
  Client_commands.context ->
  RPC.meth -> string list -> Data_encoding.json -> Data_encoding.json Lwt.t

val call_service0:
  Client_commands.context ->
  (unit, unit, 'i, 'o) RPC.service -> 'i -> 'o Lwt.t
val call_service1:
  Client_commands.context ->
  (unit, unit * 'a, 'i, 'o) RPC.service -> 'a -> 'i -> 'o Lwt.t
val call_service2:
  Client_commands.context ->
  (unit, (unit * 'a) * 'b, 'i, 'o) RPC.service -> 'a -> 'b -> 'i -> 'o Lwt.t
