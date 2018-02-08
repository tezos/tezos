(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open P2p_peer

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

module Info : sig

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

  val fold :
    ('conn, 'meta) t -> init:'a -> f:('a -> Pool_event.t -> 'a) -> 'a

  val watch :
    ('conn, 'meta) t -> Pool_event.t Lwt_stream.t * Lwt_watcher.stopper

end


val get : ('conn, 'meta) Info.t -> 'conn state

val is_disconnected : ('conn, 'meta) Info.t -> bool

val set_accepted :
  ?timestamp:Time.t ->
  ('conn, 'meta) Info.t -> P2p_connection.Id.t -> Lwt_canceler.t -> unit

val set_running :
  ?timestamp:Time.t ->
  ('conn, 'meta) Info.t -> P2p_connection.Id.t -> 'conn -> unit

val set_disconnected :
  ?timestamp:Time.t ->
  ?requested:bool ->
  ('conn, 'meta) Info.t -> unit
