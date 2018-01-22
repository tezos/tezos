(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Protocol version *)

module Version : sig
  type t = {
    name : string ;
    major : int ;
    minor : int ;
  }
  (** Type of a protocol version. *)

  val pp : Format.formatter -> t -> unit
  val encoding : t Data_encoding.t
  val common : t list -> t list -> t option
end


(** Peer_id, i.e. persistent peer identifier *)

module Peer_id : Tezos_crypto.S.INTERNAL_HASH
  with type t = Crypto_box.Public_key_hash.t

type addr = Ipaddr.V6.t
type port = int

val addr_encoding : addr Data_encoding.t

(** Point, i.e. socket address *)

module Point : sig

  type t = addr * port
  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
  val pp_opt : Format.formatter -> t option -> unit

  val of_string_exn : string -> t
  val of_string : string -> (t, string) result
  val to_string : t -> string
  val encoding : t Data_encoding.t
  val is_local : t -> bool
  val is_global : t -> bool
  val parse_addr_port : string -> string * string

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
  module Table : Hashtbl.S with type key = t

end

(** Point representing a reachable socket address *)

module Id_point : sig
  type t = addr * port option
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
  val pp_opt : Format.formatter -> t option -> unit
  val to_string : t -> string
  val encoding : t Data_encoding.t
  val is_local : t -> bool
  val is_global : t -> bool
  val of_point : Point.t -> t
  val to_point : t -> Point.t option
  val to_point_exn : t -> Point.t
  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t
  module Table : Hashtbl.S with type key = t
end


(** Identity *)

module Identity : sig
  type t = {
    peer_id : Peer_id.t ;
    public_key : Crypto_box.public_key ;
    secret_key : Crypto_box.secret_key ;
    proof_of_work_stamp : Crypto_box.nonce ;
  }
  (** Type of an identity, comprising a peer_id, a crypto keypair, and a
      proof of work stamp with enough difficulty so that the network
      accept this identity as genuine. *)

  val encoding : t Data_encoding.t

  val generate : Crypto_box.target -> t
  (** [generate target] is a freshly minted identity whose proof of
      work stamp difficulty is at least equal to [target]. *)

  val generate_with_animation :
    Format.formatter -> Crypto_box.target -> t
    (** [generate_with_animation ppf target] is a freshly minted identity
        whose proof of work stamp difficulty is at least equal to [target]. *)

end


(** Bandwidth usage statistics *)

module Stat : sig

  type t = {
    total_sent : int64 ;
    total_recv : int64 ;
    current_inflow : int ;
    current_outflow : int ;
  }

  val empty : t
  val pp : Format.formatter -> t -> unit
  val encoding : t Data_encoding.t
end

(** Information about a connection *)

module Connection_info : sig

  type t = {
    incoming : bool;
    peer_id : Peer_id.t;
    id_point : Id_point.t;
    remote_socket_port : port;
    versions : Version.t list ;
  }

  val pp : Format.formatter -> t -> unit
  val encoding : t Data_encoding.t

end

(** Pool-level events *)

module Connection_pool_log_event : sig

  type t =

    | Too_few_connections
    | Too_many_connections

    | New_point of Point.t
    | New_peer of Peer_id.t

    | Gc_points
    (** Garbage collection of known point table has been triggered. *)

    | Gc_peer_ids
    (** Garbage collection of known peer_ids table has been triggered. *)

    (* Connection-level events *)

    | Incoming_connection of Point.t
    (** We accept(2)-ed an incoming connection *)
    | Outgoing_connection of Point.t
    (** We connect(2)-ed to a remote endpoint *)
    | Authentication_failed of Point.t
    (** Remote point failed authentication *)

    | Accepting_request of Point.t * Id_point.t * Peer_id.t
    (** We accepted a connection after authentifying the remote peer. *)
    | Rejecting_request of Point.t * Id_point.t * Peer_id.t
    (** We rejected a connection after authentifying the remote peer. *)
    | Request_rejected of Point.t * (Id_point.t * Peer_id.t) option
    (** The remote peer rejected our connection. *)

    | Connection_established of Id_point.t * Peer_id.t
    (** We succesfully established a authentified connection. *)

    | Swap_request_received of { source : Peer_id.t }
    (** A swap request has been received. *)
    | Swap_ack_received of { source : Peer_id.t }
    (** A swap ack has been received *)
    | Swap_request_sent of { source : Peer_id.t }
    (** A swap request has been sent *)
    | Swap_ack_sent of { source : Peer_id.t }
    (** A swap ack has been sent *)
    | Swap_request_ignored of { source : Peer_id.t }
    (** A swap request has been ignored *)
    | Swap_success of { source : Peer_id.t }
    (** A swap operation has succeeded *)
    | Swap_failure of { source : Peer_id.t }
    (** A swap operation has failed *)

    | Disconnection of Peer_id.t
    (** We decided to close the connection. *)
    | External_disconnection of Peer_id.t
    (** The connection was closed for external reason. *)

  val encoding : t Data_encoding.t

end

module Point_state : sig

  type t =
    | Requested
    | Accepted of Peer_id.t
    | Running of Peer_id.t
    | Disconnected

  val pp_digram : Format.formatter -> t -> unit
  val encoding : t Data_encoding.t

end

module Point_info : sig

  type t = {
    trusted : bool ;
    greylisted_until : Time.t ;
    state : Point_state.t ;
    last_failed_connection : Time.t option ;
    last_rejected_connection : (Peer_id.t * Time.t) option ;
    last_established_connection : (Peer_id.t * Time.t) option ;
    last_disconnection : (Peer_id.t * Time.t) option ;
    last_seen : (Peer_id.t * Time.t) option ;
    last_miss : Time.t option ;
  }

  val encoding : t Data_encoding.t

end

module Peer_state : sig

  type t =
    | Accepted
    | Running
    | Disconnected

  val pp_digram : Format.formatter -> t -> unit
  val encoding : t Data_encoding.t

end

module Peer_info : sig

  type t = {
    score : float ;
    trusted : bool ;
    state : Peer_state.t ;
    id_point : Id_point.t option ;
    stat : Stat.t ;
    last_failed_connection : (Id_point.t * Time.t) option ;
    last_rejected_connection : (Id_point.t * Time.t) option ;
    last_established_connection : (Id_point.t * Time.t) option ;
    last_disconnection : (Id_point.t * Time.t) option ;
    last_seen : (Id_point.t * Time.t) option ;
    last_miss : (Id_point.t * Time.t) option ;
  }
  val encoding : t Data_encoding.t

end

