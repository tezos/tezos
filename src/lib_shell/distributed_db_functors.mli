(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Tezos Shell - High-level API for the Gossip network and local
                  storage (helpers). *)

(** {1 Indexes} *)

(** Generic interface for a "distributed" index.

    By "distributed", it means that this interface abstract the p2p
    gossip layer and it is able to fetch missing data from known
    peers in a "synchronous" interface.

*)
module type DISTRIBUTED_DB = sig

  type t

  (** The index key *)
  type key

  (** The indexed data *)
  type value

  (** An extra parameter for the network lookup, usually
      used for prevalidating data. *)
  type param

  (** Is the value known locally? *)
  val known: t -> key -> bool Lwt.t

  type error += Missing_data of key
  type error += Canceled of key
  type error += Timeout of key

  (** Return the value if it is known locally, otherwise fail with
      the error [Missing_data]. *)
  val read: t -> key -> value tzresult Lwt.t

  (** Return the value if it is known locally, otherwise fail with
      the value [None]. *)
  val read_opt: t -> key -> value option Lwt.t

  (** Same as `fetch` but the call is non-blocking: the data will be
      stored in the local index when received. *)
  val prefetch:
    t ->
    ?peer:P2p_peer.Id.t ->
    ?timeout:Time.System.Span.t ->
    key -> param -> unit

  (** Return the value if it is known locally, or block until the data
      is received from the network. By default, the data will be
      requested to all the active peers in the network; if the [peer]
      argument is provided, the data will only be requested to the
      provided peer. By default, the resulting promise will block
      forever if the data is never received. If [timeout] is provided
      the promise will be resolved with the error [Timeout] after the
      provided amount of seconds.

      A internal scheduler is able to re-send the request with an
      exponential back-off until the data is received. If the function
      is called multiple time with the same key but with disctinct
      peers, the internal scheduler randomly chooses the requested
      peer (at each retry). *)
  val fetch:
    t ->
    ?peer:P2p_peer.Id.t ->
    ?timeout:Time.System.Span.t ->
    key -> param -> value tzresult Lwt.t

  (** Remove the data from the local index or cancel all pending
      request. Any pending [fetch] promises are resolved with the
      error [Canceled]. *)
  val clear_or_cancel: t -> key -> unit

  (** [resolve_pending t pids k v] resolves pending request (if any) in the
      local index for key k with [Found v]. It notifies the scheduler using
      'notify_cancelation' for this key and wakes up the the waiter on this
      request. *)
  val resolve_pending: t -> key -> value -> unit

  val inject: t -> key -> value -> bool Lwt.t

  (** Monitor all the fetched data. A given data will appear only
      once. *)
  val watch: t -> (key * value) Lwt_stream.t * Lwt_watcher.stopper

  val pending: t -> key -> bool

end

module type DISK_TABLE = sig
  type store
  type key
  type value
  val known: store -> key -> bool Lwt.t
  val read: store -> key -> value tzresult Lwt.t
  val read_opt: store -> key -> value option Lwt.t
end

module type MEMORY_TABLE = sig
  (* A subtype of Hashtbl.S *)
  type 'a t
  type key
  val create: int -> 'a t
  val find: 'a t -> key -> 'a
  val find_opt: 'a t -> key -> 'a option
  val add: 'a t -> key -> 'a -> unit
  val replace: 'a t -> key -> 'a -> unit
  val remove: 'a t -> key -> unit
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module type SCHEDULER_EVENTS = sig
  type t
  type key
  val request: t -> P2p_peer.Id.t option -> key -> unit
  val notify: t -> P2p_peer.Id.t -> key -> unit
  val notify_cancelation: t -> key -> unit
  val notify_unrequested: t -> P2p_peer.Id.t -> key -> unit
  val notify_duplicate: t -> P2p_peer.Id.t -> key -> unit
  val notify_invalid: t -> P2p_peer.Id.t -> key -> unit
end

module type PRECHECK = sig
  type key
  type param
  type notified_value
  type value
  val precheck: key -> param -> notified_value -> value option
end

module Make_table
    (Hash : sig
       type t
       val name : string
       val encoding : t Data_encoding.t
       val pp : Format.formatter -> t -> unit
     end)
    (Disk_table : DISK_TABLE with type key := Hash.t)
    (Memory_table : MEMORY_TABLE with type key := Hash.t)
    (Scheduler : SCHEDULER_EVENTS with type key := Hash.t)
    (Precheck : PRECHECK with type key := Hash.t
                          and type value := Disk_table.value) : sig

  include DISTRIBUTED_DB with type key = Hash.t
                          and type value = Disk_table.value
                          and type param = Precheck.param
  val create:
    ?global_input:(key * value) Lwt_watcher.input ->
    Scheduler.t -> Disk_table.store -> t
  val notify: t -> P2p_peer.Id.t -> key -> Precheck.notified_value -> unit Lwt.t

end

module type REQUEST = sig
  type key
  type param
  val initial_delay : Time.System.Span.t
  val active : param -> P2p_peer.Set.t
  val send : param -> P2p_peer.Id.t -> key list -> unit
end

module Make_request_scheduler
    (Hash : sig
       type t
       val name : string
       val encoding : t Data_encoding.t
       val pp : Format.formatter -> t -> unit

       module Logging : sig
         val tag : t Tag.def
       end
     end)
    (Table : MEMORY_TABLE with type key := Hash.t)
    (Request : REQUEST with type key := Hash.t) : sig

  type t
  val create: Request.param -> t
  val shutdown: t -> unit Lwt.t
  include SCHEDULER_EVENTS with type t := t and type key := Hash.t

end
