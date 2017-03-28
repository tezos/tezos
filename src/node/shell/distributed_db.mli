(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t
type db = t

module Message = Distributed_db_message
module Metadata = Distributed_db_metadata

type p2p = (Message.t, Metadata.t) P2p.net

val create: State.t -> p2p -> t
val shutdown: t -> unit Lwt.t

type net

val state: net -> State.Net.t

type callback = {
  notify_branch: P2p.Peer_id.t -> Block_hash.t list -> unit ;
  current_branch: int -> Block_hash.t list Lwt.t ;
  notify_head: P2p.Peer_id.t -> Block_hash.t -> Operation_hash.t list -> unit ;
  current_head: int -> (Block_hash.t * Operation_hash.t list) Lwt.t ;
  disconnection: P2p.Peer_id.t -> unit ;
}

val activate: callback:callback -> t -> State.Net.t -> net
val deactivate: net -> unit Lwt.t

module type DISTRIBUTED_DB = sig
  type t
  type key
  type value
  val known: t -> key -> bool Lwt.t
  val read: t -> key -> value option Lwt.t
  val read_exn: t -> key -> value Lwt.t
  val commit: t -> key -> unit Lwt.t
  val inject: t -> key -> value -> bool Lwt.t
  val watch: t -> (key * value) Lwt_stream.t * Watcher.stopper
  val prefetch: t -> ?peer:P2p.Peer_id.t -> key -> unit
  val fetch: t -> ?peer:P2p.Peer_id.t -> key -> value Lwt.t
end

module Operation :
  DISTRIBUTED_DB with type t = net
                  and type key := Operation_hash.t
                  and type value := Store.Operation.t

module Block_header :
  DISTRIBUTED_DB with type t = net
                  and type key := Block_hash.t
                  and type value := Store.Block_header.t

module Protocol :
  DISTRIBUTED_DB with type t = db
                  and type key := Protocol_hash.t
                  and type value := Tezos_compiler.Protocol.t

val broadcast_head:
  net -> Block_hash.t -> Operation_hash.t list -> unit

val inject_block:
  t -> MBytes.t -> (Block_hash.t * Store.Block_header.t) tzresult Lwt.t

val read_block:
  t -> Block_hash.t -> (net * Store.Block_header.t) option Lwt.t
val read_block_exn:
  t -> Block_hash.t -> (net * Store.Block_header.t) Lwt.t

val read_operation:
  t -> Operation_hash.t -> (net * Store.Operation.t) option Lwt.t
val read_operation_exn:
  t -> Operation_hash.t -> (net * Store.Operation.t) Lwt.t

val watch_block:
  t -> (Block_hash.t * Store.Block_header.t) Lwt_stream.t * Watcher.stopper
val watch_operation:
  t -> (Operation_hash.t * Store.Operation.t) Lwt_stream.t * Watcher.stopper
val watch_protocol:
  t -> (Protocol_hash.t * Store.Protocol.t) Lwt_stream.t * Watcher.stopper

module Raw : sig
  val encoding: Message.t P2p.Raw.t Data_encoding.t
  val supported_versions: P2p_types.Version.t list
end
