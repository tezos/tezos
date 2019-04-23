(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Tezos Shell - High-level API for the Gossip network and local storage. *)

open Distributed_db_functors

type t
type db = t

module Message = Distributed_db_message

type p2p = (Message.t, Peer_metadata.t, Connection_metadata.t) P2p.net

val create: State.t -> p2p -> t
val state: db -> State.t
val shutdown: t -> unit Lwt.t

(** {1 Network database} *)

(** An instance of the distributed DB for a given chain (mainchain,
    current testchain, ...) *)
type chain_db

(** Activate a given chain. The node will notify its neighbours that
    it now handles the given chain and that it expects notification
    for new head or new operations. *)
val activate: t -> State.Chain.t -> chain_db

(** Look for the database of an active chain. *)
val get_chain: t -> Chain_id.t -> chain_db option

(** Deactivate a given chain. The node will notify its neighbours
    that it does not care anymore about this chain. *)
val deactivate: chain_db -> unit Lwt.t

type callback = {
  notify_branch: P2p_peer.Id.t -> Block_locator.t -> unit ;
  notify_head: P2p_peer.Id.t -> Block_header.t -> Mempool.t -> unit ;
  disconnection: P2p_peer.Id.t -> unit ;
}

(** Register all the possible callback from the distributed DB to the
    validator. *)
val set_callback: chain_db -> callback -> unit

(** Kick a given peer. *)
val disconnect: chain_db -> P2p_peer.Id.t -> unit Lwt.t

(** Greylist a given peer. *)
val greylist: chain_db -> P2p_peer.Id.t -> unit Lwt.t

(** Various accessors. *)
val chain_state: chain_db -> State.Chain.t
val db: chain_db -> db

(** Return the peer id of the node *)
val my_peer_id: chain_db -> P2p_peer.Id.t

val get_peer_metadata: chain_db -> P2p_peer.Id.t -> Peer_metadata.t

(** {1 Sending messages} *)

module Request : sig

  (** Send to a given peer, or to all known active peers for the
      chain, a friendly request "Hey, what's your current branch
      ?". The expected answer is a `Block_locator.t.`. *)
  val current_branch: chain_db -> ?peer:P2p_peer.Id.t -> unit -> unit

  (** Send to a given peer, or to all known active peers for the
      given chain, a friendly request "Hey, what's your current
      branch ?". The expected answer is a `Block_locator.t.`. *)
  val current_head: chain_db -> ?peer:P2p_peer.Id.t -> unit -> unit

end

module Advertise : sig

  (** Notify a given peer, or all known active peers for the
      chain, of a new head and possibly of new operations. *)
  val current_head:
    chain_db -> ?peer:P2p_peer.Id.t ->
    ?mempool:Mempool.t -> State.Block.t -> unit

  (** Notify a given peer, or all known active peers for the
      chain, of a new head and its sparse history. *)
  val current_branch:
    ?peer:P2p_peer.Id.t -> chain_db -> unit Lwt.t

end

(** {2 Block index} *)

(** Index of block headers. *)
module Block_header : sig
  type t = Block_header.t (* avoid shadowing. *)
  include DISTRIBUTED_DB with type t := chain_db
                          and type key := Block_hash.t
                          and type value := Block_header.t
                          and type param := unit
end

(** Lookup for block header in any active chains *)
val read_block_header:
  db -> Block_hash.t -> (Chain_id.t * Block_header.t) option Lwt.t

(** Index of all the operations of a given block (per validation pass). *)
module Operations :
  DISTRIBUTED_DB with type t := chain_db
                  and type key = Block_hash.t * int
                  and type value = Operation.t list
                  and type param := Operation_list_list_hash.t

(** Index of all the hashes of operations of a given block (per
    validation pass). *)
module Operation_hashes :
  DISTRIBUTED_DB with type t := chain_db
                  and type key = Block_hash.t * int
                  and type value = Operation_hash.t list
                  and type param := Operation_list_list_hash.t

(** Store on disk all the data associated to a valid block. *)
val commit_block:
  chain_db ->
  Block_hash.t ->
  Block_header.t -> MBytes.t ->
  Operation.t list list -> MBytes.t list list ->
  State.Block.validation_store ->
  forking_testchain: bool ->
  State.Block.t option tzresult Lwt.t

(** Store on disk all the data associated to an invalid block. *)
val commit_invalid_block:
  chain_db ->
  Block_hash.t -> Block_header.t -> Error_monad.error list ->
  bool tzresult Lwt.t

(** Monitor all the fetched block headers (for all activate chains). *)
val watch_block_header:
  t -> (Block_hash.t * Block_header.t) Lwt_stream.t * Lwt_watcher.stopper


(** {2 Operations index} *)

(** Index of operations (for the mempool). *)
module Operation : sig
  type t = Operation.t (* avoid shadowing. *)
  include DISTRIBUTED_DB with type t := chain_db
                          and type key := Operation_hash.t
                          and type value := Operation.t
                          and type param := unit
end

(** Inject a new operation in the local index (memory only). *)
val inject_operation:
  chain_db -> Operation_hash.t -> Operation.t -> bool Lwt.t

(** Monitor all the fetched operations (for all activate chains). *)
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
  val chain_name: Distributed_db_version.name
  val distributed_db_versions: Distributed_db_version.t list
end
