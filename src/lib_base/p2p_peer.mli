(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

module Id = Tezos_crypto.Crypto_box.Public_key_hash

module Map = Id.Map
module Set = Id.Set
module Table = Id.Table

module State : sig

  type t =
    | Accepted
    | Running
    | Disconnected

  val pp_digram : Format.formatter -> t -> unit
  val encoding : t Data_encoding.t

end

module Info : sig

  type t = {
    score : float ;
    trusted : bool ;
    state : State.t ;
    id_point : P2p_connection.Id.t option ;
    stat : P2p_stat.t ;
    last_failed_connection : (P2p_connection.Id.t * Time.t) option ;
    last_rejected_connection : (P2p_connection.Id.t * Time.t) option ;
    last_established_connection : (P2p_connection.Id.t * Time.t) option ;
    last_disconnection : (P2p_connection.Id.t * Time.t) option ;
    last_seen : (P2p_connection.Id.t * Time.t) option ;
    last_miss : (P2p_connection.Id.t * Time.t) option ;
  }

  val encoding : t Data_encoding.t

end

(** P2p_peer.Id info: current and historical information about a peer_id *)

module Pool_info : sig

  type ('conn, 'meta) t
  type ('conn, 'meta) peer_info = ('conn, 'meta) t

  val compare : ('conn, 'meta) t -> ('conn, 'meta) t -> int

  val create :
    ?created:Time.t ->
    ?trusted:bool ->
    metadata:'meta ->
    Id.t -> ('conn, 'meta) peer_info
  (** [create ~trusted ~meta peer_id] is a freshly minted peer_id info for
      [peer_id]. *)

  val peer_id : ('conn, 'meta) peer_info -> Id.t

  val created : ('conn, 'meta) peer_info -> Time.t
  val metadata : ('conn, 'meta) peer_info -> 'meta
  val set_metadata : ('conn, 'meta) peer_info -> 'meta -> unit

  val trusted : ('conn, 'meta) peer_info -> bool
  val set_trusted : ('conn, 'meta) peer_info -> unit
  val unset_trusted : ('conn, 'meta) peer_info -> unit

  val last_failed_connection :
    ('conn, 'meta) peer_info -> (P2p_connection.Id.t * Time.t) option
  val last_rejected_connection :
    ('conn, 'meta) peer_info -> (P2p_connection.Id.t * Time.t) option
  val last_established_connection :
    ('conn, 'meta) peer_info -> (P2p_connection.Id.t * Time.t) option
  val last_disconnection :
    ('conn, 'meta) peer_info -> (P2p_connection.Id.t * Time.t) option

  val last_seen :
    ('conn, 'meta) peer_info -> (P2p_connection.Id.t * Time.t) option
  (** [last_seen gi] is the most recent of:

      * last established connection
      * last rejected connection
      * last disconnection
  *)

  val last_miss :
    ('conn, 'meta) peer_info -> (P2p_connection.Id.t * Time.t) option
  (** [last_miss gi] is the most recent of:

      * last failed connection
      * last rejected connection
      * last disconnection
  *)

  val log_incoming_rejection :
    ?timestamp:Time.t ->
    ('conn, 'meta) peer_info -> P2p_connection.Id.t -> unit

  module File : sig
    val load :
      string -> 'meta Data_encoding.t ->
      ('conn, 'meta) peer_info list tzresult Lwt.t
    val save :
      string -> 'meta Data_encoding.t ->
      ('conn, 'meta) peer_info list -> unit tzresult Lwt.t
  end

end

module Pool_state : sig

  type 'conn t =
    | Accepted of { current_point: P2p_connection.Id.t ;
                    cancel: Lwt_canceler.t }
    (** We accepted a incoming connection, we greeted back and
        we are waiting for an acknowledgement. *)
    | Running of { data: 'conn ;
                   current_point: P2p_connection.Id.t }
    (** Successfully authentificated connection, normal business. *)
    | Disconnected
    (** No connection established currently. *)
  type 'conn state = 'conn t

  val pp : Format.formatter -> 'conn t -> unit

  val get : ('conn, 'meta) Pool_info.t -> 'conn state

  val is_disconnected : ('conn, 'meta) Pool_info.t -> bool

  val set_accepted :
    ?timestamp:Time.t ->
    ('conn, 'meta) Pool_info.t -> P2p_connection.Id.t -> Lwt_canceler.t -> unit

  val set_running :
    ?timestamp:Time.t ->
    ('conn, 'meta) Pool_info.t -> P2p_connection.Id.t -> 'conn -> unit

  val set_disconnected :
    ?timestamp:Time.t ->
    ?requested:bool ->
    ('conn, 'meta) Pool_info.t -> unit

end

module Pool_event : sig

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
    point : P2p_connection.Id.t ;
  }

  val encoding : t Data_encoding.t

  val fold :
    ('conn, 'meta) Pool_info.t -> init:'a -> f:('a -> t -> 'a) -> 'a

  val watch :
    ('conn, 'meta) Pool_info.t -> t Lwt_stream.t * Lwt_watcher.stopper

end
