(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
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
    'conn point_info -> (Gid.t * Time.t) option
  val last_established_connection :
    'conn point_info -> (Gid.t * Time.t) option
  val last_disconnection :
    'conn point_info -> (Gid.t * Time.t) option

  val last_seen :
    'conn point_info -> (Gid.t * Time.t) option
  (** [last_seen pi] is the most recent of:

      * last established connection
      * last rejected connection
      * last disconnection
  *)

  val last_miss :
    'conn point_info -> Time.t option

  val greylisted :
    ?now:Time.t -> 'conn point_info -> bool

  val point : 'conn point_info -> Point.t

  module State : sig

    type 'conn t =
      | Requested of { cancel: Canceler.t }
        (** We initiated a connection. *)
      | Accepted of { current_gid: Gid.t ;
                      cancel: Canceler.t }
        (** We accepted a incoming connection. *)
      | Running of { data: 'conn ;
                     current_gid: Gid.t }
        (** Successfully authentificated connection, normal business. *)
      | Disconnected
        (** No connection established currently. *)
    type 'conn state = 'conn t

    val pp : Format.formatter -> 'conn t -> unit

    val get : 'conn point_info -> 'conn state

    val is_disconnected : 'conn point_info -> bool

    val set_requested :
      ?timestamp:Time.t ->
      'conn point_info -> Canceler.t -> unit

    val set_accepted :
      ?timestamp:Time.t ->
      'conn point_info -> Gid.t -> Canceler.t -> unit

    val set_running :
      ?timestamp:Time.t -> 'conn point_info -> Gid.t -> 'conn -> unit

    val set_disconnected :
      ?timestamp:Time.t -> ?requested:bool -> 'conn point_info -> unit

  end

  module Event : sig

    type kind =
      | Outgoing_request
        (** We initiated a connection. *)
      | Accepting_request of Gid.t
        (** We accepted a connection after authentifying the remote peer. *)
      | Rejecting_request of Gid.t
        (** We rejected a connection after authentifying the remote peer. *)
      | Request_rejected of Gid.t option
        (** The remote peer rejected our connection. *)
      | Connection_established of Gid.t
        (** We succesfully established a authentified connection. *)
      | Disconnection of Gid.t
        (** We decided to close the connection. *)
      | External_disconnection of Gid.t
        (** The connection was closed for external reason. *)

    type t = {
      kind : kind ;
      timestamp : Time.t ;
    }

  end

  val fold_events :
    'conn point_info -> init:'a -> f:('a -> Event.t -> 'a) -> 'a

  val log_incoming_rejection :
    ?timestamp:Time.t -> 'conn point_info -> Gid.t -> unit

end


(** Gid info: current and historical information about a gid *)

module Gid_info : sig

  type ('conn, 'meta) t
  type ('conn, 'meta) gid_info = ('conn, 'meta) t

  val compare : ('conn, 'meta) t -> ('conn, 'meta) t -> int

  val create :
    ?trusted:bool ->
    metadata:'meta ->
    Gid.t -> ('conn, 'meta) gid_info
  (** [create ~trusted ~meta gid] is a freshly minted gid info for
      [gid]. *)

  val gid : ('conn, 'meta) gid_info -> Gid.t

  val metadata : ('conn, 'meta) gid_info -> 'meta
  val set_metadata : ('conn, 'meta) gid_info -> 'meta -> unit

  val trusted : ('conn, 'meta) gid_info -> bool
  val set_trusted : ('conn, 'meta) gid_info -> unit
  val unset_trusted : ('conn, 'meta) gid_info -> unit

  val last_failed_connection :
    ('conn, 'meta) gid_info -> (Id_point.t * Time.t) option
  val last_rejected_connection :
    ('conn, 'meta) gid_info -> (Id_point.t * Time.t) option
  val last_established_connection :
    ('conn, 'meta) gid_info -> (Id_point.t * Time.t) option
  val last_disconnection :
    ('conn, 'meta) gid_info -> (Id_point.t * Time.t) option

  val last_seen :
    ('conn, 'meta) gid_info -> (Id_point.t * Time.t) option
  (** [last_seen gi] is the most recent of:

      * last established connection
      * last rejected connection
      * last disconnection
  *)

  val last_miss :
    ('conn, 'meta) gid_info -> (Id_point.t * Time.t) option
  (** [last_miss gi] is the most recent of:

      * last failed connection
      * last rejected connection
      * last disconnection
  *)

  module State : sig

    type 'conn t =
      | Accepted of { current_point: Id_point.t ;
                      cancel: Canceler.t }
        (** We accepted a incoming connection, we greeted back and
            we are waiting for an acknowledgement. *)
      | Running of { data: 'conn ;
                     current_point: Id_point.t }
        (** Successfully authentificated connection, normal business. *)
      | Disconnected
        (** No connection established currently. *)
    type 'conn state = 'conn t

    val pp : Format.formatter -> 'conn t -> unit

    val get : ('conn, 'meta) gid_info -> 'conn state

    val is_disconnected : ('conn, 'meta) gid_info -> bool

    val set_accepted :
      ?timestamp:Time.t ->
      ('conn, 'meta) gid_info -> Id_point.t -> Canceler.t -> unit

    val set_running :
      ?timestamp:Time.t ->
      ('conn, 'meta) gid_info -> Id_point.t -> 'conn -> unit

    val set_disconnected :
      ?timestamp:Time.t ->
      ?requested:bool ->
      ('conn, 'meta) gid_info -> unit

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

  end

  val fold_events :
    ('conn, 'meta) gid_info -> init:'a -> f:('a -> Event.t -> 'a) -> 'a

  val log_incoming_rejection :
    ?timestamp:Time.t ->
    ('conn, 'meta) gid_info -> Id_point.t -> unit

  module File : sig
    val load :
      string -> 'meta Data_encoding.t ->
      ('conn, 'meta) gid_info list tzresult Lwt.t
    val save :
      string -> 'meta Data_encoding.t ->
      ('conn, 'meta) gid_info list -> unit tzresult Lwt.t
  end

end
