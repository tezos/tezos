(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Shell - High-level API for the Gossip network and local storage. *)

open Distributed_db_functors

type t
type db = t

module Message = Distributed_db_message
module Metadata = Distributed_db_metadata

type p2p = (Message.t, Metadata.t) P2p.net

val create: State.t -> p2p -> t
val state: db -> State.t
val shutdown: t -> unit Lwt.t

(** {1 Network database} *)

(** An instance of the distributed DB for a given network (mainnet,
    current testnet, ...) *)
type net_db

(** Activate a given network. The node will notify its neighbours that
    it now handles the given network and that it expects notification
    for new head or new operations. *)
val activate: t -> State.Net.t -> net_db

(** Look for the database of an active network. *)
val get_net: t -> Net_id.t -> net_db option

(** Deactivate a given network. The node will notify its neighbours
    that it does not care anymore about this network. *)
val deactivate: net_db -> unit Lwt.t

type callback = {
  notify_branch: P2p_peer.Id.t -> Block_locator.t -> unit ;
  notify_head: P2p_peer.Id.t -> Block_header.t -> Mempool.t -> unit ;
  disconnection: P2p_peer.Id.t -> unit ;
}

(** Register all the possible callback from the distributed DB to the
    validator. *)
val set_callback: net_db -> callback -> unit

(** Kick a given peer. *)
val disconnect: net_db -> P2p_peer.Id.t -> unit Lwt.t

(** Various accessors. *)
val net_state: net_db -> State.Net.t
val db: net_db -> db

(** {1 Sending messages} *)

module Request : sig

  (** Send to a given peer, or to all known active peers for the
      network, a friendly request "Hey, what's your current branch
      ?". The expected answer is a `Block_locator.t.`. *)
  val current_branch: net_db -> ?peer:P2p_peer.Id.t -> unit -> unit

  (** Send to a given peer, or to all known active peers for the
      given network, a friendly request "Hey, what's your current
      branch ?". The expected answer is a `Block_locator.t.`. *)
  val current_head: net_db -> ?peer:P2p_peer.Id.t -> unit -> unit

end

module Advertise : sig

  (** Notify a given peer, or all known active peers for the
      network, of a new head and possibly of new operations. *)
  val current_head:
    net_db -> ?peer:P2p_peer.Id.t ->
    ?mempool:Mempool.t -> State.Block.t -> unit

  (** Notify a given peer, or all known active peers for the
      network, of a new head and its sparse history. *)
  val current_branch:
    net_db -> ?peer:P2p_peer.Id.t ->
    Block_locator.t -> unit Lwt.t

end

(** {2 Block index} *)

(** Index of block headers. *)
module Block_header : sig
  type t = Block_header.t (* avoid shadowing. *)
  include DISTRIBUTED_DB with type t := net_db
                          and type key := Block_hash.t
                          and type value := Block_header.t
                          and type param := unit
end

(** Lookup for block header in any active networks *)
val read_block_header:
  db -> Block_hash.t -> (Net_id.t * Block_header.t) option Lwt.t

(** Index of all the operations of a given block (per validation pass). *)
module Operations :
  DISTRIBUTED_DB with type t := net_db
                  and type key = Block_hash.t * int
                  and type value = Operation.t list
                  and type param := Operation_list_list_hash.t

(** Index of all the hashes of operations of a given block (per
    validation pass). *)
module Operation_hashes :
  DISTRIBUTED_DB with type t := net_db
                  and type key = Block_hash.t * int
                  and type value = Operation_hash.t list
                  and type param := Operation_list_list_hash.t

(** Store on disk all the data associated to a valid block. *)
val commit_block:
  net_db ->
  Block_hash.t ->
  Block_header.t -> Operation.t list list ->
  Updater.validation_result ->
  State.Block.t option tzresult Lwt.t

(** Store on disk all the data associated to an invalid block. *)
val commit_invalid_block:
  net_db ->
  Block_hash.t -> Block_header.t -> Error_monad.error list ->
  bool tzresult Lwt.t

(** Monitor all the fetched block headers (for all activate networks). *)
val watch_block_header:
  t -> (Block_hash.t * Block_header.t) Lwt_stream.t * Lwt_watcher.stopper


(** {2 Operations index} *)

(** Index of operations (for the mempool). *)
module Operation : sig
  type t = Operation.t (* avoid shadowing. *)
  include DISTRIBUTED_DB with type t := net_db
                          and type key := Operation_hash.t
                          and type value := Operation.t
                          and type param := unit
end

(** Inject a new operation in the local index (memory only). *)
val inject_operation:
  net_db -> Operation_hash.t -> Operation.t -> bool Lwt.t

(** Monitor all the fetched operations (for all activate networks). *)
val watch_operation:
  t -> (Operation_hash.t * Operation.t) Lwt_stream.t * Lwt_watcher.stopper

(** {2 Protocol index} *)

(** Index of protocol sources. *)
module Protocol : sig
  type t = Protocol.t (* avoid shadowing. *)
  include DISTRIBUTED_DB with type t := db
                          and type key := Protocol_hash.t
                          and type value := Protocol.t
                          and type param := unit
end

(** Store on disk protocol sources. *)
val commit_protocol:
  db -> Protocol_hash.t -> Protocol.t -> bool tzresult Lwt.t

(**/**)

module Raw : sig
  val encoding: Message.t P2p.Raw.t Data_encoding.t
  val supported_versions: P2p_version.t list
end

