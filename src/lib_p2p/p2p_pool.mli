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

(** Pool of connections. This module manages the connection pool that
    the peer-to-peer layer needs to maintain in order to function
    correctly.

    A pool and its connections are parametrized by the type of
    messages exchanged over the connection and the type of
    meta-information associated with a peer. The type
    [('msg, 'peer_meta,'conn_meta)
    connection] is a wrapper on top of [P2p_socket.t] that adds
    meta-informations, data-structures describing the detailed state of
    the peer and the connection, as well as a new message queue
    (referred to "app  message queue") that will only contain the
    messages from the internal [P2p_socket.t] that needs to be examined
    by the higher layers. Some messages are directly processed by an
    internal worker and thus never propagated above. *)

type 'msg encoding = Encoding : {
    tag: int ;
    title: string ;
    encoding: 'a Data_encoding.t ;
    wrap: 'a -> 'msg ;
    unwrap: 'msg -> 'a option ;
    max_length: int option ;
  } -> 'msg encoding

(** {1 Pool management} *)

type ('msg, 'peer_meta, 'conn_meta) t

type ('msg, 'peer_meta, 'conn_meta) pool = ('msg, 'peer_meta, 'conn_meta) t
(** The type of a pool of connections, parametrized by resp. the type
    of messages and the meta-informations associated to an identity and
    a connection. *)

type config = {

  identity : P2p_identity.t ;
  (** Our identity. *)

  proof_of_work_target : Crypto_box.target ;
  (** The proof of work target we require from peers. *)

  trusted_points : P2p_point.Id.t list ;
  (** List of hard-coded known peers to bootstrap the network from. *)

  peers_file : string ;
  (** The path to the JSON file where the metadata associated to
      peer_ids are loaded / stored. *)

  private_mode : bool ;
  (** If [true], only open outgoing/accept incoming connections
      to/from peers whose addresses are in [trusted_peers], and inform
      these peers that the identity of this node should be revealed to
      the rest of the network. *)

  listening_port : P2p_addr.port option ;
  (** If provided, it will be passed to [P2p_connection.authenticate]
      when we authenticate against a new peer. *)

  min_connections : int ;
  (** Strict minimum number of connections
      (triggers [LogEvent.too_few_connections]). *)

  max_connections : int ;
  (** Max number of connections. If it's reached, [connect] and
      [accept] will fail, i.e. not add more connections
      (also triggers [LogEvent.too_many_connections]). *)

  max_incoming_connections : int ;
  (** Max not-yet-authentified incoming connections.
      Above this number, [accept] will start dropping incoming
      connections. *)

  connection_timeout : Time.System.Span.t ;
  (** Maximum time allowed to the establishment of a connection. *)

  authentication_timeout : Time.System.Span.t ;
  (** Delay granted to a peer to perform authentication, in seconds. *)

  incoming_app_message_queue_size : int option ;
  (** Size of the message queue for user messages (messages returned
      by this module's [read] function. *)

  incoming_message_queue_size : int option ;
  (** Size of the incoming message queue internal of a peer's Reader
      (See [P2p_connection.accept]). *)

  outgoing_message_queue_size : int option ;
  (** Size of the outgoing message queue internal to a peer's Writer
      (See [P2p_connection.accept]). *)

  known_peer_ids_history_size : int ;
  (** Size of the known peer_ids log buffer (default: 50) *)

  known_points_history_size : int ;
  (** Size of the known points log buffer (default: 50) *)

  max_known_points : (int * int) option ;
  (** Parameters for the the garbage collection of known points. If
      None, no garbage collection is performed. Otherwise, the first
      integer of the couple limits the size of the "known points"
      table. When this number is reached, the table is expurged from
      disconnected points, older first, to try to reach the amount of
      connections indicated by the second integer. *)

  max_known_peer_ids : (int * int) option ;
  (** Like [max_known_points], but for known peer_ids. *)

  swap_linger : Time.System.Span.t ;
  (** Peer swapping does not occur more than once during a timespan of
      [spap_linger] seconds. *)

  binary_chunks_size : int option ;
  (** Size (in bytes) of binary blocks that are sent to other
      peers. Default value is 64 kB. *)
}

type 'peer_meta peer_meta_config = {
  peer_meta_encoding : 'peer_meta Data_encoding.t ;
  peer_meta_initial : unit -> 'peer_meta ;
  score : 'peer_meta -> float ;
}

type 'msg message_config = {
  encoding : 'msg encoding list ;
  chain_name : Distributed_db_version.name ;
  distributed_db_versions : Distributed_db_version.t list ;
}

val create:
  ?p2p_versions: P2p_version.t list ->
  config ->
  'peer_meta peer_meta_config ->
  'conn_meta P2p_socket.metadata_config ->
  'msg message_config ->
  P2p_io_scheduler.t ->
  ('msg, 'peer_meta,'conn_meta) pool Lwt.t
(** [create config meta_cfg msg_cfg io_sched] is a freshly minted
    pool. *)

val destroy: ('msg, 'peer_meta,'conn_meta) pool -> unit Lwt.t
(** [destroy pool] returns when member connections are either
    disconnected or canceled. *)

val active_connections: ('msg, 'peer_meta,'conn_meta) pool -> int
(** [active_connections pool] is the number of connections inside
    [pool]. *)

val pool_stat: ('msg, 'peer_meta,'conn_meta) pool -> P2p_stat.t
(** [pool_stat pool] is a snapshot of current bandwidth usage for the
    entire [pool]. *)

val config : _ pool -> config
(** [config pool] is the [config] argument passed to [pool] at
    creation. *)

val send_swap_request: ('msg, 'peer_meta,'conn_meta) pool -> unit
(** [send_swap_request pool] given two connected peers pi and pj (pi
    <> pj), suggest swap with pi for the peer pj. This behaviour is
    disabled in private mode *)

val score: ('msg, 'peer_meta,'conn_meta) pool -> 'peer_meta -> float
(** [score pool peer_meta] returns the score of a peer in the pool
    whose peer_meta is provided *)

(** {2 Pool events} *)

module Pool_event : sig

  val wait_too_few_connections: ('msg, 'peer_meta,'conn_meta) pool -> unit Lwt.t
  (** [wait_too_few_connections pool] is determined when the number of
      connections drops below the desired level. *)

  val wait_too_many_connections: ('msg, 'peer_meta,'conn_meta) pool -> unit Lwt.t
  (** [wait_too_many_connections pool] is determined when the number of
      connections exceeds the desired level. *)

  val wait_new_peer: ('msg, 'peer_meta,'conn_meta) pool -> unit Lwt.t
  (** [wait_new_peer pool] is determined when a new peer
      (i.e. authentication successful) gets added to the pool. *)

  val wait_new_point: ('msg, 'peer_meta,'conn_meta) pool -> unit Lwt.t
  (** [wait_new_point pool] is determined when a new point gets registered
      to the pool. *)

  val wait_new_connection: ('msg, 'peer_meta,'conn_meta) pool -> unit Lwt.t
  (** [wait_new_connection pool] is determined when a new connection is
      successfully established in the pool. *)

end


(** {1 Connections management} *)

type ('msg, 'peer_meta,'conn_meta) connection
(** Type of a connection to a peer, parametrized by the type of
    messages exchanged as well as meta-information associated to a
    peer and a connection. It mostly wraps [P2p_connection.connection],
    adding meta-information and data-structures describing a more
    fine-grained logical state of the connection. *)

val connect:
  ?timeout:Time.System.Span.t ->
  ('msg, 'peer_meta,'conn_meta) pool -> P2p_point.Id.t ->
  ('msg, 'peer_meta,'conn_meta) connection tzresult Lwt.t
(** [connect ?timeout pool point] tries to add a connection to [point]
    in [pool] in less than [timeout] seconds. *)

val accept:
  ('msg, 'peer_meta,'conn_meta) pool -> P2p_fd.t -> P2p_point.Id.t -> unit
(** [accept pool fd point] instructs [pool] to start the process of
    accepting a connection from [fd]. Used by [P2p_welcome]. *)

val register_new_point:
  ?trusted:bool ->
  ('a, 'b, 'c) pool -> P2p_peer.Table.key -> P2p_point.Id.t -> unit
(** [register_new_point pool source_peer_id point] tries to register [point]
    in pool's internal peer table. *)

val disconnect:
  ?wait:bool -> ('msg, 'peer_meta,'conn_meta) connection -> unit Lwt.t
(** [disconnect conn] cleanly closes [conn] and returns after [conn]'s
    internal worker has returned. *)

module Connection : sig

  val info: ('msg, 'peer_meta,'conn_meta) connection -> 'conn_meta P2p_connection.Info.t
  val local_metadata: ('msg, 'peer_meta,'conn_meta) connection -> 'conn_meta
  val remote_metadata: ('msg, 'peer_meta,'conn_meta) connection -> 'conn_meta

  val stat:  ('msg, 'peer_meta,'conn_meta) connection -> P2p_stat.t
  (** [stat conn] is a snapshot of current bandwidth usage for
      [conn]. *)

  val fold:
    ('msg, 'peer_meta,'conn_meta) pool ->
    init:'a ->
    f:(P2p_peer.Id.t ->  ('msg, 'peer_meta,'conn_meta) connection -> 'a -> 'a) ->
    'a

  val list:
    ('msg, 'peer_meta,'conn_meta) pool ->
    (P2p_peer.Id.t * ('msg, 'peer_meta,'conn_meta) connection) list

  val find_by_point:
    ('msg, 'peer_meta,'conn_meta) pool ->
    P2p_point.Id.t ->
    ('msg, 'peer_meta,'conn_meta) connection option

  val find_by_peer_id:
    ('msg, 'peer_meta,'conn_meta) pool ->
    P2p_peer.Id.t ->
    ('msg, 'peer_meta,'conn_meta) connection option

  val private_node: ('msg, 'peer_meta,'conn_meta) connection -> bool
  (** [private_node conn] returns 'true' if the node assocoatied to this
      connection is in private mode *)

  val trusted_node: ('msg, 'peer_meta,'conn_meta) connection -> bool
  (** [trusted_node conn] returns 'true' if the node assocoatied to this
      connection is trusted *)

end

val on_new_connection:
  ('msg, 'peer_meta,'conn_meta) pool ->
  (P2p_peer.Id.t -> ('msg, 'peer_meta,'conn_meta) connection -> unit) -> unit

(** {1 I/O on connections} *)

val read:  ('msg, 'peer_meta,'conn_meta) connection -> 'msg tzresult Lwt.t
(** [read conn] returns a message popped from [conn]'s app message
    queue, or fails with [Connection_closed]. *)

val is_readable: ('msg, 'peer_meta,'conn_meta) connection -> unit tzresult Lwt.t
(** [is_readable conn] returns when there is at least one message
    ready to be read. *)

val write:
  ('msg, 'peer_meta,'conn_meta) connection -> 'msg -> unit tzresult Lwt.t
(** [write conn msg] is [P2p_connection.write conn' msg] where [conn']
    is the internal [P2p_connection.t] inside [conn]. *)

val write_sync:
  ('msg, 'peer_meta,'conn_meta) connection -> 'msg -> unit tzresult Lwt.t
(** [write_sync conn msg] is [P2p_connection.write_sync conn' msg]
    where [conn'] is the internal [P2p_connection.t] inside [conn]. *)

(**/**)
val raw_write_sync:
  ('msg, 'peer_meta,'conn_meta) connection -> MBytes.t -> unit tzresult Lwt.t
(**/**)

val write_now:  ('msg, 'peer_meta,'conn_meta) connection -> 'msg -> bool tzresult
(** [write_now conn msg] is [P2p_connection.write_now conn' msg] where
    [conn'] is the internal [P2p_connection.t] inside [conn]. *)

(** {2 Broadcast functions} *)

val write_all:  ('msg, 'peer_meta,'conn_meta) pool -> 'msg -> unit
(** [write_all pool msg] is [write_now conn msg] for all member
    connections to [pool] in [Running] state. *)

val broadcast_bootstrap_msg:  ('msg, 'peer_meta,'conn_meta) pool -> unit
(** [broadcast_bootstrap_msg pool] is [P2P_connection.write_now conn Bootstrap]
    for all member connections to [pool] in [Running] state.
    This behavior is deactivated if the node is in private mode  *)

val greylist_addr : ('msg, 'peer_meta,'conn_meta) pool -> P2p_addr.t -> unit
(** [greylist_addr pool addr] adds [addr] to [pool]'s IP greylist. *)

val greylist_peer : ('msg, 'peer_meta,'conn_meta) pool -> P2p_peer.Id.t -> unit
(** [greylist_peer pool peer] adds [peer] to [pool]'s peer greylist
    and [peer]'s address to [pool]'s IP greylist. *)

val gc_greylist: older_than:Time.System.t -> ('msg, 'peer_meta,'conn_meta) pool -> unit
(** [gc_greylist ~older_than pool] *)

val acl_clear : ('msg, 'peer_meta,'conn_meta) pool -> unit
(** [acl_clear pool] clears ACL tables. *)

(** {1 Functions on [Peer_id]} *)

module Peers : sig

  type ('msg, 'peer_meta,'conn_meta) info =
    (('msg, 'peer_meta,'conn_meta) connection, 'peer_meta,'conn_meta) P2p_peer_state.Info.t

  val info:
    ('msg, 'peer_meta,'conn_meta) pool ->
    P2p_peer.Id.t ->
    ('msg, 'peer_meta,'conn_meta) info option

  val get_peer_metadata:
    ('msg, 'peer_meta,'conn_meta) pool -> P2p_peer.Id.t -> 'peer_meta
  val set_peer_metadata:
    ('msg, 'peer_meta,'conn_meta) pool -> P2p_peer.Id.t -> 'peer_meta -> unit
  val get_score: ('msg, 'peer_meta,'conn_meta) pool -> P2p_peer.Id.t -> float

  val get_trusted: ('msg, 'peer_meta,'conn_meta) pool -> P2p_peer.Id.t -> bool
  val set_trusted: ('msg, 'peer_meta,'conn_meta) pool -> P2p_peer.Id.t -> unit
  val unset_trusted: ('msg, 'peer_meta,'conn_meta) pool -> P2p_peer.Id.t -> unit

  val fold_known:
    ('msg, 'peer_meta,'conn_meta) pool ->
    init:'a ->
    f:(P2p_peer.Id.t ->  ('msg, 'peer_meta,'conn_meta) info -> 'a -> 'a) ->
    'a

  val fold_connected:
    ('msg, 'peer_meta,'conn_meta) pool ->
    init:'a ->
    f:(P2p_peer.Id.t ->  ('msg, 'peer_meta,'conn_meta) info -> 'a -> 'a) ->
    'a

  val ban : ('msg, 'peer_meta,'conn_meta) pool -> P2p_peer.Id.t -> unit
  val unban : ('msg, 'peer_meta,'conn_meta) pool -> P2p_peer.Id.t -> unit
  val trust : ('msg, 'peer_meta,'conn_meta) pool -> P2p_peer.Id.t -> unit
  val untrust : ('msg, 'peer_meta,'conn_meta) pool -> P2p_peer.Id.t -> unit
  val banned : ('msg, 'peer_meta,'conn_meta) pool -> P2p_peer.Id.t -> bool

end

(** {1 Functions on [Points]} *)

module Points : sig

  type ('msg, 'peer_meta,'conn_meta) info =
    ('msg, 'peer_meta,'conn_meta) connection P2p_point_state.Info.t

  val info:
    ('msg, 'peer_meta,'conn_meta) pool ->
    P2p_point.Id.t ->
    ('msg, 'peer_meta,'conn_meta) info option

  val get_trusted: ('msg, 'peer_meta,'conn_meta) pool -> P2p_point.Id.t -> bool
  val set_trusted: ('msg, 'peer_meta,'conn_meta) pool -> P2p_point.Id.t -> unit
  val unset_trusted: ('msg, 'peer_meta,'conn_meta) pool -> P2p_point.Id.t -> unit

  val fold_known:
    ('msg, 'peer_meta,'conn_meta) pool ->
    init:'a ->
    f:(P2p_point.Id.t -> ('msg, 'peer_meta,'conn_meta) info  -> 'a -> 'a) ->
    'a

  val fold_connected:
    ('msg, 'peer_meta,'conn_meta) pool ->
    init:'a ->
    f:(P2p_point.Id.t -> ('msg, 'peer_meta,'conn_meta) info  -> 'a -> 'a) ->
    'a

  val ban : ('msg, 'peer_meta,'conn_meta) pool -> P2p_point.Id.t -> unit
  val unban : ('msg, 'peer_meta,'conn_meta) pool -> P2p_point.Id.t -> unit
  val trust : ('msg, 'peer_meta,'conn_meta) pool -> P2p_point.Id.t -> unit
  val untrust : ('msg, 'peer_meta,'conn_meta) pool -> P2p_point.Id.t -> unit
  val banned : ('msg, 'peer_meta,'conn_meta) pool -> P2p_point.Id.t -> bool

end

val watch:
  ('msg, 'peer_meta,'conn_meta) pool ->
  P2p_connection.Pool_event.t Lwt_stream.t * Lwt_watcher.stopper
(** [watch pool] is a [stream, close] a [stream] of events and a
    [close] function for this stream. *)

(**/**)

module Message : sig

  type 'msg t =
    | Bootstrap
    | Advertise of P2p_point.Id.t list
    | Swap_request of P2p_point.Id.t * P2p_peer.Id.t
    | Swap_ack of P2p_point.Id.t * P2p_peer.Id.t
    | Message of 'msg
    | Disconnect

  val encoding: 'msg encoding list -> 'msg t Data_encoding.t

end
