(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
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
val state: db -> State.t
val shutdown: t -> unit Lwt.t

type net_db

val net_state: net_db -> State.Net.t
val db: net_db -> db

type callback = {
  notify_branch:
    P2p.Peer_id.t -> Block_locator.t -> unit ;
  notify_head:
    P2p.Peer_id.t -> Block_header.t -> Mempool.t -> unit ;
  disconnection: P2p.Peer_id.t -> unit ;
}

val activate: t -> State.Net.t -> net_db
val set_callback: net_db -> callback -> unit
val deactivate: net_db -> unit Lwt.t

val disconnect: net_db -> P2p.Peer_id.t -> unit Lwt.t

val commit_block:
  net_db ->
  Block_hash.t ->
  Block_header.t -> Operation.t list list ->
  Updater.validation_result ->
  State.Block.t option tzresult Lwt.t

val commit_invalid_block:
  net_db ->
  Block_hash.t -> Block_header.t -> Error_monad.error list ->
  bool tzresult Lwt.t

val clear_operations: net_db -> Operation_hash.t list list -> unit

val inject_block_header:
  net_db -> Block_hash.t -> Block_header.t -> bool tzresult Lwt.t

val inject_operation:
  net_db -> Operation_hash.t -> Operation.t -> bool tzresult Lwt.t

val commit_protocol:
  db -> Protocol_hash.t -> Protocol.t -> bool tzresult Lwt.t
val inject_protocol:
  db -> Protocol_hash.t -> Protocol.t -> bool Lwt.t

val watch_block_header:
  t -> (Block_hash.t * Block_header.t) Lwt_stream.t * Watcher.stopper
val watch_operation:
  t -> (Operation_hash.t * Operation.t) Lwt_stream.t * Watcher.stopper
val watch_protocol:
  t -> (Protocol_hash.t * Protocol.t) Lwt_stream.t * Watcher.stopper


module type DISTRIBUTED_DB = sig
  type t
  type key
  type value
  type param
  val known: t -> key -> bool Lwt.t
  type error += Missing_data of key
  type error += Canceled of key
  type error += Timeout of key
  val read: t -> key -> value tzresult Lwt.t
  val read_opt: t -> key -> value option Lwt.t
  val read_exn: t -> key -> value Lwt.t
  val watch: t -> (key * value) Lwt_stream.t * Watcher.stopper
  val prefetch:
    t ->
    ?peer:P2p.Peer_id.t ->
    ?timeout:float ->
    key -> param -> unit
  val fetch:
    t ->
    ?peer:P2p.Peer_id.t ->
    ?timeout:float ->
    key -> param -> value tzresult Lwt.t
  val clear_or_cancel: t -> key -> unit
end

module Block_header :
  DISTRIBUTED_DB with type t = net_db
                  and type key := Block_hash.t
                  and type value := Block_header.t
                  and type param := unit

module Operations :
  DISTRIBUTED_DB with type t = net_db
                  and type key = Block_hash.t * int
                  and type value = Operation.t list
                  and type param := Operation_list_list_hash.t

val read_all_operations:
  net_db -> Block_hash.t -> int -> Operation.t list list tzresult Lwt.t


module Operation_hashes :
  DISTRIBUTED_DB with type t = net_db
                  and type key = Block_hash.t * int
                  and type value = Operation_hash.t list
                  and type param := Operation_list_list_hash.t

module Operation :
  DISTRIBUTED_DB with type t = net_db
                  and type key := Operation_hash.t
                  and type value := Operation.t
                  and type param := unit

module Protocol :
  DISTRIBUTED_DB with type t = db
                  and type key := Protocol_hash.t
                  and type value := Protocol.t
                  and type param := unit

module Raw : sig
  val encoding: Message.t P2p.Raw.t Data_encoding.t
  val supported_versions: P2p_types.Version.t list
end

module Request : sig
  val current_branch: net_db -> ?peer:P2p.Peer_id.t -> unit -> unit
  val current_head: net_db -> ?peer:P2p.Peer_id.t -> unit -> unit
end

module Advertise : sig
  val current_head:
    net_db -> ?peer:P2p.Peer_id.t ->
    ?mempool:Mempool.t -> State.Block.t -> unit
  val current_branch:
    net_db -> ?peer:P2p.Peer_id.t ->
    State.Block.t -> unit Lwt.t
end

