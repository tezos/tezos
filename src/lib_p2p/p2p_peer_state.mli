(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open P2p_peer

type ('conn, 'conn_meta) t =
  | Accepted of { current_point: P2p_connection.Id.t ;
                  cancel: Lwt_canceler.t }
  (** We accepted a incoming connection, we greeted back and
      we are waiting for an acknowledgement. *)
  | Running of { data: 'conn ;
                 conn_metadata: 'conn_meta ;
                 current_point: P2p_connection.Id.t }
  (** Successfully authentificated connection, normal business. *)
  | Disconnected
  (** No connection established currently. *)
type ('conn, 'conn_meta) state = ('conn, 'conn_meta) t

val pp : Format.formatter -> ('conn, 'conn_meta) t -> unit

module Info : sig

  type ('conn, 'peer_meta, 'conn_meta) t
  type ('conn, 'peer_meta, 'conn_meta) peer_info = ('conn, 'peer_meta, 'conn_meta) t

  val compare : ('conn, 'peer_meta, 'conn_meta) t -> ('conn, 'peer_meta, 'conn_meta) t -> int

  val create :
    ?created:Time.t ->
    ?trusted:bool ->
    peer_metadata:'peer_meta ->
    Id.t -> ('conn, 'peer_meta, 'conn_meta) peer_info
  (** [create ~trusted ~meta peer_id] is a freshly minted peer_id info for
      [peer_id]. *)

  val peer_id : ('conn, 'peer_meta, 'conn_meta) peer_info -> Id.t

  val created : ('conn, 'peer_meta, 'conn_meta) peer_info -> Time.t
  val peer_metadata : ('conn, 'peer_meta, 'conn_meta) peer_info -> 'peer_meta
  val set_peer_metadata : ('conn, 'peer_meta, 'conn_meta) peer_info -> 'peer_meta -> unit

  val trusted : ('conn, 'peer_meta, 'conn_meta) peer_info -> bool
  val set_trusted : ('conn, 'peer_meta, 'conn_meta) peer_info -> unit
  val unset_trusted : ('conn, 'peer_meta, 'conn_meta) peer_info -> unit

  val last_failed_connection :
    ('conn, 'peer_meta, 'conn_meta) peer_info -> (P2p_connection.Id.t * Time.t) option
  val last_rejected_connection :
    ('conn, 'peer_meta, 'conn_meta) peer_info -> (P2p_connection.Id.t * Time.t) option
  val last_established_connection :
    ('conn, 'peer_meta, 'conn_meta) peer_info -> (P2p_connection.Id.t * Time.t) option
  val last_disconnection :
    ('conn, 'peer_meta, 'conn_meta) peer_info -> (P2p_connection.Id.t * Time.t) option

  val last_seen :
    ('conn, 'peer_meta, 'conn_meta) peer_info -> (P2p_connection.Id.t * Time.t) option
  (** [last_seen gi] is the most recent of:

      * last established connection
      * last rejected connection
      * last disconnection
  *)

  val last_miss :
    ('conn, 'peer_meta, 'conn_meta) peer_info -> (P2p_connection.Id.t * Time.t) option
  (** [last_miss gi] is the most recent of:

      * last failed connection
      * last rejected connection
      * last disconnection
  *)

  val log_incoming_rejection :
    ?timestamp:Time.t ->
    ('conn, 'peer_meta, 'conn_meta) peer_info -> P2p_connection.Id.t -> unit

  module File : sig
    val load :
      string -> 'peer_meta Data_encoding.t ->
      ('conn, 'peer_meta, 'conn_meta) peer_info list tzresult Lwt.t
    val save :
      string -> 'peer_meta Data_encoding.t ->
      ('conn, 'peer_meta, 'conn_meta) peer_info list -> unit tzresult Lwt.t
  end

  val fold :
    ('conn, 'peer_meta, 'conn_meta) t -> init:'a -> f:('a -> Pool_event.t -> 'a) -> 'a

  val watch :
    ('conn, 'peer_meta, 'conn_meta) t -> Pool_event.t Lwt_stream.t * Lwt_watcher.stopper

end

val get : ('conn, 'peer_meta, 'conn_meta) Info.t -> ('conn, 'conn_meta) state

val is_disconnected : ('conn, 'peer_meta, 'conn_meta) Info.t -> bool

val set_accepted :
  ?timestamp:Time.t ->
  ('conn, 'peer_meta, 'conn_meta) Info.t -> P2p_connection.Id.t -> Lwt_canceler.t -> unit

val set_running :
  ?timestamp:Time.t ->
  ('conn, 'peer_meta, 'conn_meta) Info.t -> P2p_connection.Id.t -> 'conn -> 'conn_meta -> unit

val set_disconnected :
  ?timestamp:Time.t ->
  ?requested:bool ->
  ('conn, 'peer_meta, 'conn_meta) Info.t -> unit
