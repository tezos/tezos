(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** A P2P network *)
type net

(** A faked p2p layer, which do not initiate any connection
    nor open any listening socket. *)
val faked_network : net

(** A peer connection address *)
type addr = Ipaddr.t

(** A peer connection port *)
type port = int

(** A protocol version tag: (name, major, minor) *)
type version = string * int * int

(** Network configuration *)
type config = {
  (** Tells if incoming connections accepted, precising the TCP port
  on which the peer can be reached *)
  incoming_port : port option ;
  (** Tells if peers should be discovered automatically on the local
  network, precising the UDP port to use *)
  discovery_port : port option ;
  (** High level protocol(s) talked by the peer. When two peers
      initiate a connection, they exchange their list of supported
      versions. The chosen one, if any, is the maximum common one (in
      lexicographic order) *)
  supported_versions : version list ;
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
  (** Maximum length in bytes of network frames *)
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

(** Main network initialisation function *)
val bootstrap : config -> limits -> net Lwt.t

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

(** Access the info of an active peer, if available *)
val peer_info : peer -> net -> addr * port * version

(** Wait for a Netbits.frame from any peer in the network *)
val recv : net -> (peer * Netbits.frame) Lwt.t

(** Send a Netbits.frame to a peer and wait for it to be in the tube *)
val send : peer * Netbits.frame -> net -> unit Lwt.t

(** Send a Netbits.frame to a peer asynchronously *)
val push : peer * Netbits.frame -> net -> unit

(** Send a Netbits.frame to all peers *)
val broadcast : Netbits.frame -> net -> unit

(** Shutdown the connection to all peers at this address and stop the
    communications with this machine for [duration] seconds *)
val blacklist : ?duration:float -> addr -> net -> unit

(** Keep a connection to this pair as often as possible *)
val whitelist : peer -> net -> unit
