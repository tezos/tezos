(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** A peer connection address *)
type addr = Ipaddr.t

(** A peer connection port *)
type port = int

(** A p2p protocol version *)
type version = {
  name : string ;
  major : int ;
  minor : int ;
}

(** Network configuration *)
type config = {
  (** Tells if incoming connections accepted, precising the TCP port
      on which the peer can be reached *)
  incoming_port : port option ;
  (** Tells if peers should be discovered automatically on the local
      network, precising the UDP port to use *)
  discovery_port : port option ;
  (** List of hard-coded known peers to bootstrap the network from *)
  known_peers : (addr * port) list ;
  (** The path to the JSON file where the peer cache is loaded / stored *)
  peers_file : string ;
  (** If [true], the only accepted connections are from peers whose
      addresses are in [known_peers] *)
  closed_network : bool ;
}

(** Network capacities *)
type limits = {
  (** Maximum length in bytes of network messages' payload *)
  max_packet_size : int ;
  (** Delay after which a non responding peer is considered dead *)
  peer_answer_timeout : float ;
  (** Minimum number of connections to reach when staring / maitening *)
  expected_connections : int ;
  (** Strict minimum number of connections (triggers an urgent maintenance) *)
  min_connections : int ;
  (** Maximum number of connections (exceeding peers are disconnected) *)
  max_connections : int ;
  (** How long peers can be blacklisted for maintenance *)
  blacklist_time : float ;
}

(** A global identifier for a peer, a.k.a. an identity *)
type gid

type 'msg msg_encoding = Encoding : {
    tag: int ;
    encoding: 'a Data_encoding.t ;
    wrap: 'a -> 'msg ;
    unwrap: 'msg -> 'a option ;
    max_length: int option ;
  } -> 'msg msg_encoding

module type NET_PARAMS = sig
  type meta (** Type of metadata associated to an identity *)
  type msg (** Type of message used by higher layers *)

  val msg_encodings : msg msg_encoding list

  val init_meta : meta
  val score_enc : meta Data_encoding.t
  val score: meta -> float

  (** High level protocol(s) talked by the peer. When two peers
      initiate a connection, they exchange their list of supported
      versions. The chosen one, if any, is the maximum common one (in
      lexicographic order) *)
  val supported_versions : version list
end

module Make (P : NET_PARAMS) : sig
  type net

  (** A faked p2p layer, which do not initiate any connection
      nor open any listening socket. *)
  val faked_network : net

  (** Main network initialisation function *)
  val bootstrap : config:config -> limits:limits -> net Lwt.t

  (** A maintenance operation : try and reach the ideal number of peers *)
  val maintain : net -> unit Lwt.t

  (** Voluntarily drop some peers and replace them by new buddies *)
  val roll : net -> unit Lwt.t

  (** Close all connections properly *)
  val shutdown : net -> unit Lwt.t

  (** A connection to a peer *)
  type peer

  (** Access the domain of active peers *)
  val peers : net -> peer list

  (** Return the active peer with identity [gid] *)
  val find_peer : net -> gid -> peer option

  type peer_info = {
    gid : gid;
    addr : addr;
    port : port;
    version : version;
  }

  (** Access the info of an active peer, if available *)
  val peer_info : net -> peer -> peer_info

  (** Accessors for meta information about a peer *)
  val get_meta : net -> gid -> P.meta option
  val set_meta : net -> gid -> P.meta -> unit

  (** Wait for a payload from any peer in the network *)
  val recv : net -> (peer * P.msg) Lwt.t

  (** Send a payload to a peer and wait for it to be in the tube *)
  val send : net -> peer -> P.msg -> unit Lwt.t

  (** Send a payload to a peer without waiting for the result. Return
      [true] if the message can be enqueued in the peer's output queue
      or [false] otherwise. *)
  val try_send : net -> peer -> P.msg -> bool

  (** Send a payload to all peers *)
  val broadcast : net -> P.msg -> unit

  (** Shutdown the connection to all peers at this address and stop the
      communications with this machine for [duration] seconds *)
  val blacklist : net -> gid -> unit

  (** Keep a connection to this pair as often as possible *)
  val whitelist : net -> gid -> unit
end
