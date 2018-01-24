(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type peer_id = Crypto_box.Public_key_hash.t
(* = P2p_peer.Id.t, but we should break cycles *)

module Id : sig

  type t = P2p_addr.t * P2p_addr.port
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

end

module Map : Map.S with type key = Id.t
module Set : Set.S with type elt = Id.t
module Table : Hashtbl.S with type key = Id.t

module State : sig

  type t =
    | Requested
    | Accepted of peer_id
    | Running of peer_id
    | Disconnected

  val pp_digram : Format.formatter -> t -> unit
  val encoding : t Data_encoding.t

  val of_peer_id : t -> peer_id option
  val of_peerid_state : t -> peer_id option -> t

end

module Info : sig

  type t = {
    trusted : bool ;
    greylisted_until : Time.t ;
    state : State.t ;
    last_failed_connection : Time.t option ;
    last_rejected_connection : (peer_id * Time.t) option ;
    last_established_connection : (peer_id * Time.t) option ;
    last_disconnection : (peer_id * Time.t) option ;
    last_seen : (peer_id * Time.t) option ;
    last_miss : Time.t option ;
  }

  val encoding: t Data_encoding.t

end

module Pool_info : sig

  type 'conn t
  type 'conn point_info = 'conn t
  (** Type of info associated to a point. *)

  val compare : 'conn point_info -> 'conn point_info -> int

  type greylisting_config = {
    factor: float ;
    initial_delay: int ;
    disconnection_delay: int ;
  }

  val create :
    ?trusted:bool ->
    ?greylisting_config:greylisting_config ->
    P2p_addr.t -> P2p_addr.port -> 'conn point_info
  (** [create ~trusted addr port] is a freshly minted point_info. If
      [trusted] is true, this point is considered trusted and will
      be treated as such. *)

  val trusted : 'conn point_info -> bool
  (** [trusted pi] is [true] iff [pi] has is trusted,
      i.e. "whitelisted". *)

  val set_trusted : 'conn point_info -> unit
  val unset_trusted : 'conn point_info -> unit

  val last_failed_connection :
    'conn point_info -> Time.t option
  val last_rejected_connection :
    'conn point_info -> (peer_id * Time.t) option
  val last_established_connection :
    'conn point_info -> (peer_id * Time.t) option
  val last_disconnection :
    'conn point_info -> (peer_id * Time.t) option

  val last_seen :
    'conn point_info -> (peer_id * Time.t) option
  (** [last_seen pi] is the most recent of:

      * last established connection
      * last rejected connection
      * last disconnection
  *)

  val last_miss :
    'conn point_info -> Time.t option
  (** [last_miss pi] is the most recent of:

      * last failed connection
      * last rejected connection
      * last disconnection
  *)

  val greylisted :
    ?now:Time.t -> 'conn point_info -> bool

  val greylisted_until : 'conn point_info -> Time.t

  val point : 'conn point_info -> Id.t

  val log_incoming_rejection :
    ?timestamp:Time.t -> 'conn point_info -> peer_id -> unit

end

module Pool_state : sig

  type 'conn t =
    | Requested of { cancel: Lwt_canceler.t }
    (** We initiated a connection. *)
    | Accepted of { current_peer_id: peer_id ;
                    cancel: Lwt_canceler.t }
    (** We accepted a incoming connection. *)
    | Running of { data: 'conn ;
                   current_peer_id: peer_id }
    (** Successfully authentificated connection, normal business. *)
    | Disconnected
    (** No connection established currently. *)
  type 'conn state = 'conn t

  val pp : Format.formatter -> 'conn t -> unit

  val get : 'conn Pool_info.t -> 'conn state

  val is_disconnected : 'conn Pool_info.t -> bool

  val set_requested :
    ?timestamp:Time.t ->
    'conn Pool_info.t -> Lwt_canceler.t -> unit

  val set_accepted :
    ?timestamp:Time.t ->
    'conn Pool_info.t -> peer_id -> Lwt_canceler.t -> unit

  val set_running :
    ?timestamp:Time.t -> 'conn Pool_info.t -> peer_id -> 'conn -> unit

  val set_disconnected :
    ?timestamp:Time.t -> ?requested:bool -> 'conn Pool_info.t -> unit

end

module Pool_event : sig

  type kind =
    | Outgoing_request
    (** We initiated a connection. *)
    | Accepting_request of peer_id
    (** We accepted a connection after authentifying the remote peer. *)
    | Rejecting_request of peer_id
    (** We rejected a connection after authentifying the remote peer. *)
    | Request_rejected of peer_id option
    (** The remote peer rejected our connection. *)
    | Connection_established of peer_id
    (** We succesfully established a authentified connection. *)
    | Disconnection of peer_id
    (** We decided to close the connection. *)
    | External_disconnection of peer_id
    (** The connection was closed for external reason. *)

  type t = {
    kind : kind ;
    timestamp : Time.t ;
  }

  val encoding : t Data_encoding.t

  val fold :
    'conn Pool_info.t -> init:'a -> f:('a -> t -> 'a) -> 'a

  val watch :
    'conn Pool_info.t -> t Lwt_stream.t * Lwt_watcher.stopper

end


