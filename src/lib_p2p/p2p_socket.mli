(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Typed and encrypted connections to peers.

    This modules adds message encoding and encryption to
    [P2p_io_scheduler]'s generic throttled connections.

    Each connection have an associated internal read (resp. write)
    queue containing messages (of type ['msg]), whose size can be
    limited by providing corresponding arguments to [accept].
*)

(** {1 Types} *)

type 'meta metadata_config = {
  conn_meta_encoding : 'meta Data_encoding.t ;
  conn_meta_value : P2p_peer.Id.t -> 'meta ;
  private_node : 'meta -> bool ;
}
(** Type for the parameter negotiation mechanism. *)

type 'meta authenticated_connection
(** Type of a connection that successfully passed the authentication
    phase, but has not been accepted yet. Parametrized by the type
    of expected parameter in the `ack` message. *)

type ('msg, 'meta) t
(** Type of an accepted connection, parametrized by the type of
    messages exchanged between peers. *)

val equal: ('mst, 'meta) t -> ('msg, 'meta) t -> bool

val pp: Format.formatter -> ('msg, 'meta) t -> unit
val info: ('msg, 'meta) t -> 'meta P2p_connection.Info.t
val local_metadata: ('msg, 'meta) t -> 'meta
val remote_metadata: ('msg, 'meta) t -> 'meta
val private_node: ('msg, 'meta) t -> bool

(** {1 Low-level functions (do not use directly)} *)

val authenticate:
  canceler:Lwt_canceler.t ->
  proof_of_work_target:Crypto_box.target ->
  incoming:bool ->
  P2p_io_scheduler.connection -> P2p_point.Id.t ->
  ?listening_port: int ->
  P2p_identity.t -> Network_version.t ->
  'meta metadata_config ->
  ('meta P2p_connection.Info.t * 'meta authenticated_connection) tzresult Lwt.t
(** (Low-level) (Cancelable) Authentication function of a remote
    peer. Used in [P2p_connection_pool], to promote a
    [P2P_io_scheduler.connection] into an [authenticated_connection] (auth
    correct, acceptation undecided). *)

val kick: 'meta authenticated_connection -> unit Lwt.t
(** (Low-level) (Cancelable) [kick afd] notifies the remote peer that
    we refuse this connection and then closes [afd]. Used in
    [P2p_connection_pool] to reject an [authenticated_connection] which we do
    not want to connect to for some reason. *)

val accept:
  ?incoming_message_queue_size:int ->
  ?outgoing_message_queue_size:int ->
  ?binary_chunks_size: int ->
  canceler:Lwt_canceler.t ->
  'meta authenticated_connection ->
  'msg Data_encoding.t -> ('msg, 'meta) t tzresult Lwt.t
(** (Low-level) (Cancelable) Accepts a remote peer given an
    authenticated_connection. Used in [P2p_connection_pool], to promote an
    [authenticated_connection] to the status of an active peer. *)

val check_binary_chunks_size: int -> unit tzresult Lwt.t
(** Precheck for the [?binary_chunks_size] parameter of [accept]. *)

(** {1 IO functions on connections} *)

(** {2 Output functions} *)

val write: ('msg, 'meta) t -> 'msg -> unit tzresult Lwt.t
(** [write conn msg] returns when [msg] has successfully been added to
    [conn]'s internal write queue or fails with a corresponding
    error. *)

val write_now: ('msg, 'meta) t -> 'msg -> bool tzresult
(** [write_now conn msg] is [Ok true] if [msg] has been added to
    [conn]'s internal write queue, [Ok false] if [msg] has been
    dropped, or fails with a correponding error otherwise. *)

val write_sync: ('msg, 'meta) t -> 'msg -> unit tzresult Lwt.t
(** [write_sync conn msg] returns when [msg] has been successfully
    sent to the remote end of [conn], or fails accordingly. *)

(** {2 Input functions} *)

val is_readable: ('msg, 'meta) t -> bool
(** [is_readable conn] is [true] iff [conn] internal read queue is not
    empty. *)

val wait_readable: ('msg, 'meta) t -> unit tzresult Lwt.t
(** (Cancelable) [wait_readable conn] returns when [conn]'s internal
    read queue becomes readable (i.e. not empty). *)

val read: ('msg, 'meta) t -> (int * 'msg) tzresult Lwt.t
(** [read conn msg] returns when [msg] has successfully been popped
    from [conn]'s internal read queue or fails with a corresponding
    error. *)

val read_now: ('msg, 'meta) t -> (int * 'msg) tzresult option
(** [read_now conn msg] is [Some msg] if [conn]'s internal read queue
    is not empty, [None] if it is empty, or fails with a correponding
    error otherwise. *)

val stat: ('msg, 'meta) t -> P2p_stat.t
(** [stat conn] is a snapshot of current bandwidth usage for
    [conn]. *)

val close: ?wait:bool -> ('msg, 'meta) t -> unit Lwt.t

(**/**)

(** for testing only *)
val raw_write_sync: ('msg, 'meta) t -> MBytes.t -> unit tzresult Lwt.t
