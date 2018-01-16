(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open P2p_types

module Point_info : sig

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
    addr -> port -> 'conn point_info
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
    'conn point_info -> (Peer_id.t * Time.t) option
  val last_established_connection :
    'conn point_info -> (Peer_id.t * Time.t) option
  val last_disconnection :
    'conn point_info -> (Peer_id.t * Time.t) option

  val last_seen :
    'conn point_info -> (Peer_id.t * Time.t) option
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

  val point : 'conn point_info -> Point.t

  module State : sig

    type 'conn t =
      | Requested of { cancel: Lwt_canceler.t }
      (** We initiated a connection. *)
      | Accepted of { current_peer_id: Peer_id.t ;
                      cancel: Lwt_canceler.t }
      (** We accepted a incoming connection. *)
      | Running of { data: 'conn ;
                     current_peer_id: Peer_id.t }
      (** Successfully authentificated connection, normal business. *)
      | Disconnected
      (** No connection established currently. *)
    type 'conn state = 'conn t

    val pp : Format.formatter -> 'conn t -> unit

    val get : 'conn point_info -> 'conn state

    val is_disconnected : 'conn point_info -> bool

    val set_requested :
      ?timestamp:Time.t ->
      'conn point_info -> Lwt_canceler.t -> unit

    val set_accepted :
      ?timestamp:Time.t ->
      'conn point_info -> Peer_id.t -> Lwt_canceler.t -> unit

    val set_running :
      ?timestamp:Time.t -> 'conn point_info -> Peer_id.t -> 'conn -> unit

    val set_disconnected :
      ?timestamp:Time.t -> ?requested:bool -> 'conn point_info -> unit

  end

  module Event : sig

    type kind =
      | Outgoing_request
      (** We initiated a connection. *)
      | Accepting_request of Peer_id.t
      (** We accepted a connection after authentifying the remote peer. *)
      | Rejecting_request of Peer_id.t
      (** We rejected a connection after authentifying the remote peer. *)
      | Request_rejected of Peer_id.t option
      (** The remote peer rejected our connection. *)
      | Connection_established of Peer_id.t
      (** We succesfully established a authentified connection. *)
      | Disconnection of Peer_id.t
      (** We decided to close the connection. *)
      | External_disconnection of Peer_id.t
      (** The connection was closed for external reason. *)

    type t = {
      kind : kind ;
      timestamp : Time.t ;
    }

    val encoding : t Data_encoding.t

  end

  val fold_events :
    'conn point_info -> init:'a -> f:('a -> Event.t -> 'a) -> 'a

  val watch :
    'conn point_info -> Event.t Lwt_stream.t * Lwt_watcher.stopper

  val log_incoming_rejection :
    ?timestamp:Time.t -> 'conn point_info -> Peer_id.t -> unit

end


(** Peer_id info: current and historical information about a peer_id *)

module Peer_info : sig

  type ('conn, 'meta) t
  type ('conn, 'meta) peer_info = ('conn, 'meta) t

  val compare : ('conn, 'meta) t -> ('conn, 'meta) t -> int

  val create :
    ?created:Time.t ->
    ?trusted:bool ->
    metadata:'meta ->
    Peer_id.t -> ('conn, 'meta) peer_info
  (** [create ~trusted ~meta peer_id] is a freshly minted peer_id info for
      [peer_id]. *)

  val peer_id : ('conn, 'meta) peer_info -> Peer_id.t

  val created : ('conn, 'meta) peer_info -> Time.t
  val metadata : ('conn, 'meta) peer_info -> 'meta
  val set_metadata : ('conn, 'meta) peer_info -> 'meta -> unit

  val trusted : ('conn, 'meta) peer_info -> bool
  val set_trusted : ('conn, 'meta) peer_info -> unit
  val unset_trusted : ('conn, 'meta) peer_info -> unit

  val last_failed_connection :
    ('conn, 'meta) peer_info -> (Id_point.t * Time.t) option
  val last_rejected_connection :
    ('conn, 'meta) peer_info -> (Id_point.t * Time.t) option
  val last_established_connection :
    ('conn, 'meta) peer_info -> (Id_point.t * Time.t) option
  val last_disconnection :
    ('conn, 'meta) peer_info -> (Id_point.t * Time.t) option

  val last_seen :
    ('conn, 'meta) peer_info -> (Id_point.t * Time.t) option
  (** [last_seen gi] is the most recent of:

      * last established connection
      * last rejected connection
      * last disconnection
  *)

  val last_miss :
    ('conn, 'meta) peer_info -> (Id_point.t * Time.t) option
  (** [last_miss gi] is the most recent of:

      * last failed connection
      * last rejected connection
      * last disconnection
  *)

  module State : sig

    type 'conn t =
      | Accepted of { current_point: Id_point.t ;
                      cancel: Lwt_canceler.t }
      (** We accepted a incoming connection, we greeted back and
          we are waiting for an acknowledgement. *)
      | Running of { data: 'conn ;
                     current_point: Id_point.t }
      (** Successfully authentificated connection, normal business. *)
      | Disconnected
      (** No connection established currently. *)
    type 'conn state = 'conn t

    val pp : Format.formatter -> 'conn t -> unit

    val get : ('conn, 'meta) peer_info -> 'conn state

    val is_disconnected : ('conn, 'meta) peer_info -> bool

    val set_accepted :
      ?timestamp:Time.t ->
      ('conn, 'meta) peer_info -> Id_point.t -> Lwt_canceler.t -> unit

    val set_running :
      ?timestamp:Time.t ->
      ('conn, 'meta) peer_info -> Id_point.t -> 'conn -> unit

    val set_disconnected :
      ?timestamp:Time.t ->
      ?requested:bool ->
      ('conn, 'meta) peer_info -> unit

  end

  module Event : sig

    type kind =
      | Accepting_request
      (** We accepted a connection after authentifying the remote peer. *)
      | Rejecting_request
      (** We rejected a connection after authentifying the remote peer. *)
      | Request_rejected
      (** The remote peer rejected our connection. *)
      | Connection_established
      (** We succesfully established a authentified connection. *)
      | Disconnection
      (** We decided to close the connection. *)
      | External_disconnection
      (** The connection was closed for external reason. *)

    type t = {
      kind : kind ;
      timestamp : Time.t ;
      point : Id_point.t ;
    }

    val encoding : t Data_encoding.t
  end

  val fold_events :
    ('conn, 'meta) peer_info -> init:'a -> f:('a -> Event.t -> 'a) -> 'a

  val watch :
    ('conn, 'meta) peer_info -> Event.t Lwt_stream.t * Lwt_watcher.stopper

  val log_incoming_rejection :
    ?timestamp:Time.t ->
    ('conn, 'meta) peer_info -> Id_point.t -> unit

  module File : sig
    val load :
      string -> 'meta Data_encoding.t ->
      ('conn, 'meta) peer_info list tzresult Lwt.t
    val save :
      string -> 'meta Data_encoding.t ->
      ('conn, 'meta) peer_info list -> unit tzresult Lwt.t
  end

end
