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

(** Tezos Shell Net - Low level API for the Gossip network

    This is the entry point of the peer-to-peer layer.

    It is used by the Shell as the API to communicate with other
    nodes.
*)

type 'peer_meta peer_meta_config = {
  peer_meta_encoding : 'peer_meta Data_encoding.t;
  peer_meta_initial : unit -> 'peer_meta;
  score : 'peer_meta -> float ;
}

type 'conn_meta conn_meta_config = {
  conn_meta_encoding : 'conn_meta Data_encoding.t;
  conn_meta_value : P2p_peer.Id.t -> 'conn_meta ;
  private_node : 'conn_meta -> bool ;
}

type 'msg app_message_encoding = Encoding : {
    tag: int ;
    title: string ;
    encoding: 'a Data_encoding.t ;
    wrap: 'a -> 'msg ;
    unwrap: 'msg -> 'a option ;
    max_length: int option ;
  } -> 'msg app_message_encoding

type 'msg message_config = {
  encoding : 'msg app_message_encoding list ;
  chain_name : Distributed_db_version.name ;
  distributed_db_versions : Distributed_db_version.t list ;
}

(** Network configuration *)
type config = {

  listening_port : P2p_addr.port option;
  (** Tells if incoming connections accepted, precising the TCP port
      on which the peer can be reached (default: [9732])*)

  listening_addr : P2p_addr.t option;
  (** When incoming connections are accepted, precise on which
      IP adddress the node listen (default: [[::]]). *)

  discovery_port : P2p_addr.port option;
  (** Tells if local peer discovery is enabled, precising the TCP port
      on which the peer can be reached (default: [10732]) *)

  discovery_addr : Ipaddr.V4.t option;
  (** When local peer discovery is enabled, precise on which
      IP address messages are broadcasted (default: [255.255.255.255]). *)

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

  identity : P2p_identity.t ;
  (** Cryptographic identity of the peer. *)

  proof_of_work_target : Crypto_box.target ;
  (** Expected level of proof of work of peers' identity. *)

  disable_mempool : bool ;
  (** If [true], all non-empty mempools will be ignored. *)

  trust_discovered_peers : bool ;
  (** If [true], peers discovered on the local network will be trusted. *)

  disable_testchain : bool ;
  (** If [true], testchain related messages will be ignored. *)
}

(** Network capacities *)
type limits = {

  connection_timeout : Time.System.Span.t ;
  (** Maximum time allowed to the establishment of a connection. *)

  authentication_timeout : Time.System.Span.t ;
  (** Delay granted to a peer to perform authentication, in seconds. *)

  greylist_timeout : Time.System.Span.t ;
  (** GC delay for the grelists tables, in seconds. *)

  maintenance_idle_time: Time.System.Span.t ;
  (** How long to wait at most, in seconds, before running a maintenance loop. *)

  min_connections : int ;
  (** Strict minimum number of connections (triggers an urgent maintenance) *)

  expected_connections : int ;
  (** Targeted number of connections to reach when bootstrapping / maintaining *)

  max_connections : int ;
  (** Maximum number of connections (exceeding peers are disconnected) *)

  backlog : int ;
  (** Argument of [Lwt_unix.accept].*)

  max_incoming_connections : int ;
  (** Maximum not-yet-authenticated incoming connections. *)

  max_download_speed : int option ;
  (** Hard-limit in the number of bytes received per second. *)

  max_upload_speed : int option ;
  (** Hard-limit in the number of bytes sent per second. *)

  read_buffer_size : int ;
  (** Size in bytes of the buffer passed to [Lwt_unix.read]. *)

  read_queue_size : int option ;
  write_queue_size : int option ;
  incoming_app_message_queue_size : int option ;
  incoming_message_queue_size : int option ;
  outgoing_message_queue_size : int option ;
  (** Various bounds for internal queues. *)

  known_peer_ids_history_size : int ;
  known_points_history_size : int ;
  (** Size of circular log buffers, in number of events recorded. *)

  max_known_peer_ids : (int * int) option ;
  max_known_points : (int * int) option ;
  (** Optional limitation of internal hashtables (max, target) *)

  swap_linger : Time.System.Span.t ;
  (** Peer swapping does not occur more than once during a timespan of
      [swap_linger] seconds. *)

  binary_chunks_size : int option ;
  (** Size (in bytes) of binary blocks that are sent to other
      peers. Default value is 64 kB. Max value is 64kB. *)

}

(** Type of a P2P layer instance, parametrized by:
    ['msg]: type of messages exchanged between peers
    ['peer_meta]: type of the metadata associated with peers (score, etc.)
    ['conn_meta]: type of the metadata associated with connection (ack_cfg)
*)
type ('msg, 'peer_meta, 'conn_meta) t
type ('msg, 'peer_meta, 'conn_meta) net = ('msg, 'peer_meta, 'conn_meta) t

(** A faked p2p layer, which do not initiate any connection
    nor open any listening socket *)
val faked_network :
  'msg message_config ->
  'peer_meta peer_meta_config ->
  'conn_meta ->
  ('msg, 'peer_meta, 'conn_meta) net

(** Main network initialisation function *)
val create :
  config:config -> limits:limits ->
  'peer_meta peer_meta_config -> 'conn_meta conn_meta_config ->
  'msg message_config ->  ('msg, 'peer_meta, 'conn_meta) net tzresult Lwt.t

val activate : ('msg, 'peer_meta, 'conn_meta) net -> unit

(** Return one's peer_id *)
val peer_id : ('msg, 'peer_meta, 'conn_meta) net -> P2p_peer.Id.t

(** A maintenance operation : try and reach the ideal number of peers *)
val maintain : ('msg, 'peer_meta, 'conn_meta) net -> unit Lwt.t

(** Voluntarily drop some peers and replace them by new buddies *)
val roll : ('msg, 'peer_meta, 'conn_meta) net -> unit Lwt.t

(** Close all connections properly *)
val shutdown : ('msg, 'peer_meta, 'conn_meta) net -> unit Lwt.t

(** A connection to a peer *)
type ('msg, 'peer_meta, 'conn_meta) connection

(** Access the domain of active peers *)
val connections :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection list

(** Return the active peer with identity [peer_id] *)
val find_connection :
  ('msg, 'peer_meta, 'conn_meta) net ->
  P2p_peer.Id.t ->
  ('msg, 'peer_meta, 'conn_meta) connection option

(** Access the info of an active peer, if available *)
val connection_info :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  'conn_meta P2p_connection.Info.t
val connection_local_metadata :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  'conn_meta
val connection_remote_metadata :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  'conn_meta
val connection_stat :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  P2p_stat.t

(** Cleanly closes a connection. *)
val disconnect :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ?wait:bool ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  unit Lwt.t

val global_stat : ('msg, 'peer_meta, 'conn_meta) net -> P2p_stat.t

(** Accessors for meta information about a global identifier *)
val get_peer_metadata :
  ('msg, 'peer_meta, 'conn_meta) net -> P2p_peer.Id.t -> 'peer_meta
val set_peer_metadata :
  ('msg, 'peer_meta, 'conn_meta) net -> P2p_peer.Id.t -> 'peer_meta -> unit

(** Wait for a message from a given connection. *)
val recv :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  'msg tzresult Lwt.t

(** Wait for a message from any active connections. *)
val recv_any :
  ('msg, 'peer_meta, 'conn_meta) net ->
  (('msg, 'peer_meta, 'conn_meta) connection * 'msg) Lwt.t

(** [send net peer msg] is a thread that returns when [msg] has been
    successfully enqueued in the send queue. *)
val send :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  'msg ->
  unit tzresult Lwt.t

(** [try_send net peer msg] is [true] if [msg] has been added to the
    send queue for [peer], [false] otherwise *)
val try_send :
  ('msg, 'peer_meta, 'conn_meta) net ->
  ('msg, 'peer_meta, 'conn_meta) connection ->
  'msg ->
  bool

(** Send a message to all peers *)
val broadcast : ('msg, 'peer_meta, 'conn_meta) net -> 'msg -> unit

val fold_connections :
  ('msg, 'peer_meta, 'conn_meta) net ->
  init:'a ->
  f:(P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection -> 'a -> 'a) ->
  'a

val iter_connections :
  ('msg, 'peer_meta, 'conn_meta) net ->
  (P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection -> unit) -> unit

val on_new_connection :
  ('msg, 'peer_meta, 'conn_meta) net ->
  (P2p_peer.Id.t -> ('msg, 'peer_meta, 'conn_meta) connection -> unit) -> unit

val build_rpc_directory :
  (_, Peer_metadata.t , Connection_metadata.t) t -> unit RPC_directory.t

val greylist_addr : ('msg, 'peer_meta, 'conn_meta) net -> P2p_addr.t -> unit
val greylist_peer : ('msg, 'peer_meta, 'conn_meta) net -> P2p_peer.Id.t -> unit

(**/**)

module Raw : sig
  type 'a t =
    | Bootstrap
    | Advertise of P2p_point.Id.t list
    | Swap_request of P2p_point.Id.t * P2p_peer.Id.t
    | Swap_ack of P2p_point.Id.t * P2p_peer.Id.t
    | Message of 'a
    | Disconnect
  val encoding: 'msg app_message_encoding list -> 'msg t Data_encoding.t
end
