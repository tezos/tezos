(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Typed and encrypted connections to peers.

    This modules adds message encoding and encryption to
    [P2p_io_scheduler]'s generic throttled connections.

    Each connection have an associated internal read (resp. write)
    queue containing messages (of type ['msg]), whose size can be
    limited by providing corresponding arguments to [accept].
*)

(** {1 Types} *)

type 'conn_meta authenticated_fd
(** Type of a connection that successfully passed the authentication
    phase, but has not been accepted yet. Parametrized by the type
    of expected parameter in the `ack` message. *)

type 'msg t
(** Type of an accepted connection, parametrized by the type of
    messages exchanged between peers. *)

val equal: 'mst t -> 'msg t -> bool

val pp: Format.formatter -> 'msg t -> unit
val info: 'msg t -> P2p_connection.Info.t

(** {1 Low-level functions (do not use directly)} *)

val authenticate:
  proof_of_work_target:Crypto_box.target ->
  incoming:bool ->
  P2p_io_scheduler.connection -> P2p_point.Id.t ->
  ?listening_port: int ->
  P2p_identity.t -> P2p_version.t list -> 'conn_meta Data_encoding.t ->
  (P2p_connection.Info.t * 'conn_meta authenticated_fd) tzresult Lwt.t
(** (Low-level) (Cancelable) Authentication function of a remote
    peer. Used in [P2p_connection_pool], to promote a
    [P2P_io_scheduler.connection] into an [authenticated_fd] (auth
    correct, acceptation undecided). *)

val kick: 'conn_meta authenticated_fd -> unit Lwt.t
(** (Low-level) (Cancelable) [kick afd] notifies the remote peer that
    we refuse this connection and then closes [afd]. Used in
    [P2p_connection_pool] to reject an [aunthenticated_fd] which we do
    not want to connect to for some reason. *)

val accept:
  ?incoming_message_queue_size:int ->
  ?outgoing_message_queue_size:int ->
  ?binary_chunks_size: int ->
  'conn_meta authenticated_fd -> 'conn_meta ->
  'msg Data_encoding.t -> ('msg t * 'conn_meta) tzresult Lwt.t
(** (Low-level) (Cancelable) Accepts a remote peer given an
    authenticated_fd. Used in [P2p_connection_pool], to promote an
    [authenticated_fd] to the status of an active peer. *)

val check_binary_chunks_size:  int -> unit tzresult Lwt.t
(** Precheck for the [?binary_chunks_size] parameter of [accept]. *)

(** {1 IO functions on connections} *)

(** {2 Output functions} *)

val write: 'msg t -> 'msg -> unit tzresult Lwt.t
(** [write conn msg] returns when [msg] has successfully been added to
    [conn]'s internal write queue or fails with a corresponding
    error. *)

val write_now: 'msg t -> 'msg -> bool tzresult
(** [write_now conn msg] is [Ok true] if [msg] has been added to
    [conn]'s internal write queue, [Ok false] if [msg] has been
    dropped, or fails with a correponding error otherwise. *)

val write_sync: 'msg t -> 'msg -> unit tzresult Lwt.t
(** [write_sync conn msg] returns when [msg] has been successfully
    sent to the remote end of [conn], or fails accordingly. *)

(** {2 Input functions} *)

val is_readable: 'msg t -> bool
(** [is_readable conn] is [true] iff [conn] internal read queue is not
    empty. *)

val wait_readable: 'msg t -> unit tzresult Lwt.t
(** (Cancelable) [wait_readable conn] returns when [conn]'s internal
    read queue becomes readable (i.e. not empty). *)

val read: 'msg t -> (int * 'msg) tzresult Lwt.t
(** [read conn msg] returns when [msg] has successfully been popped
    from [conn]'s internal read queue or fails with a corresponding
    error. *)

val read_now: 'msg t -> (int * 'msg) tzresult option
(** [read_now conn msg] is [Some msg] if [conn]'s internal read queue
    is not empty, [None] if it is empty, or fails with a correponding
    error otherwise. *)

val stat: 'msg t -> P2p_stat.t
(** [stat conn] is a snapshot of current bandwidth usage for
    [conn]. *)

val close: ?wait:bool -> 'msg t -> unit Lwt.t

(**/**)

(** for testing only *)
val raw_write_sync: 'msg t -> MBytes.t -> unit tzresult Lwt.t
