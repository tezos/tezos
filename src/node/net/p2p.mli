(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** A peer connection address *)
type addr = Ipaddr.V6.t

(** A peer connection port *)
type port = int

(** A p2p protocol version *)
module Version = P2p_types.Version

(** A global identifier for a peer, a.k.a. an identity *)
module Gid = P2p_types.Gid

module Identity = P2p_types.Identity

module Point = P2p_types.Point

module Id_point = P2p_types.Id_point

module Connection_info = P2p_types.Connection_info

module Stat = P2p_types.Stat

type 'meta meta_config = {
  encoding : 'meta Data_encoding.t;
  initial : 'meta;
}

type 'msg app_message_encoding = Encoding : {
    tag: int ;
    encoding: 'a Data_encoding.t ;
    wrap: 'a -> 'msg ;
    unwrap: 'msg -> 'a option ;
    max_length: int option ;
  } -> 'msg app_message_encoding

type 'msg message_config = {
  encoding : 'msg app_message_encoding list ;
  versions : Version.t list;
}

(** Network configuration *)
type config = {

  listening_port : port option;
  (** Tells if incoming connections accepted, precising the TCP port
      on which the peer can be reached *)

  listening_addr : addr option;
  (** When incoming connections are accepted, precising on which
      IP adddress the node listen (default: [[::]]). *)

  trusted_points : Point.t list ;
  (** List of hard-coded known peers to bootstrap the network from. *)

  peers_file : string ;
  (** The path to the JSON file where the metadata associated to
      gids are loaded / stored. *)

  closed_network : bool ;
  (** If [true], the only accepted connections are from peers whose
      addresses are in [trusted_peers]. *)

  identity : Identity.t ;
  (** Cryptographic identity of the peer. *)

  proof_of_work_target : Crypto_box.target ;
  (** Expected level of proof of work of peers' identity. *)

}

(** Network capacities *)
type limits = {

  authentification_timeout : float ;
  (** Delay granted to a peer to perform authentication, in seconds. *)

  min_connections : int ;
  (** Strict minimum number of connections (triggers an urgent maintenance) *)

  expected_connections : int ;
  (** Targeted number of connections to reach when bootstraping / maintaining *)

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

}

type ('msg, 'meta) t
type ('msg, 'meta) net = ('msg, 'meta) t

(** A faked p2p layer, which do not initiate any connection
    nor open any listening socket *)
val faked_network : ('msg, 'meta) net

(** Main network initialisation function *)
val create :
  config:config -> limits:limits ->
  'meta meta_config -> 'msg message_config ->  ('msg, 'meta) net Lwt.t

(** Return one's gid *)
val gid : ('msg, 'meta) net -> Gid.t

(** A maintenance operation : try and reach the ideal number of peers *)
val maintain : ('msg, 'meta) net -> unit Lwt.t

(** Voluntarily drop some peers and replace them by new buddies *)
val roll : ('msg, 'meta) net -> unit Lwt.t

(** Close all connections properly *)
val shutdown : ('msg, 'meta) net -> unit Lwt.t

(** A connection to a peer *)
type ('msg, 'meta) connection

(** Access the domain of active peers *)
val connections : ('msg, 'meta) net -> ('msg, 'meta) connection list

(** Return the active peer with identity [gid] *)
val find_connection : ('msg, 'meta) net -> Gid.t -> ('msg, 'meta) connection option

(** Access the info of an active peer, if available *)
val connection_info :
  ('msg, 'meta) net -> ('msg, 'meta) connection -> Connection_info.t
val connection_stat :
  ('msg, 'meta) net -> ('msg, 'meta) connection -> Stat.t
val global_stat : ('msg, 'meta) net -> Stat.t

(** Accessors for meta information about a global identifier *)
val get_metadata : ('msg, 'meta) net -> Gid.t -> 'meta option
val set_metadata : ('msg, 'meta) net -> Gid.t -> 'meta -> unit

(** Wait for a message from a given connection. *)
val recv :
  ('msg, 'meta) net -> ('msg, 'meta) connection -> 'msg tzresult Lwt.t

(** Wait for a message from any active connections. *)
val recv_any :
  ('msg, 'meta) net -> (('msg, 'meta) connection * 'msg) Lwt.t

(** [send net peer msg] is a thread that returns when [msg] has been
    successfully enqueued in the send queue. *)
val send :
  ('msg, 'meta) net -> ('msg, 'meta) connection -> 'msg -> unit Lwt.t

(** [try_send net peer msg] is [true] if [msg] has been added to the
    send queue for [peer], [false] otherwise *)
val try_send :
  ('msg, 'meta) net -> ('msg, 'meta) connection -> 'msg -> bool

(** Send a message to all peers *)
val broadcast : ('msg, 'meta) net -> 'msg -> unit

(**/**)
module Raw : sig
  type 'a t =
    | Bootstrap
    | Advertise of P2p_types.Point.t list
    | Message of 'a
    | Disconnect
  val encoding: 'msg app_message_encoding list -> 'msg t Data_encoding.t
end

