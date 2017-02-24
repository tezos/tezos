(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module type DISTRIBUTED_DB = sig
  type t
  type key
  type value
  val known: t -> key -> bool Lwt.t
  val read: t -> key -> value option Lwt.t
  val read_exn: t -> key -> value Lwt.t
  val prefetch: t -> ?peer:P2p.Peer_id.t -> key -> unit
  val fetch: t -> ?peer:P2p.Peer_id.t -> key -> value Lwt.t
  val commit: t -> key -> unit Lwt.t
  (* val commit_invalid: t -> key -> unit Lwt.t *) (* TODO *)
  val inject: t -> key -> value -> bool Lwt.t
  val watch: t -> (key * value) Lwt_stream.t * Watcher.stopper
end

module type SCHEDULER_EVENTS = sig
  type t
  type key
  val request: t -> P2p.Peer_id.t option -> key -> unit
  val notify: t -> P2p.Peer_id.t -> key -> unit
  val notify_unrequested: t -> P2p.Peer_id.t -> key -> unit
  val notify_duplicate: t -> P2p.Peer_id.t -> key -> unit
end

module Make_table
    (Hash : HASH)
    (Disk_table : State.DATA_STORE with type key := Hash.t)
    (Memory_table : Hashtbl.S with type key := Hash.t)
    (Scheduler : SCHEDULER_EVENTS with type key := Hash.t) : sig

  include DISTRIBUTED_DB with type key = Hash.t
                          and type value = Disk_table.value
  val create:
    ?global_input:(key * value) Watcher.input ->
    Scheduler.t -> Disk_table.store -> t
  val notify: t -> P2p.Peer_id.t -> key -> value -> unit Lwt.t

end

module type REQUEST = sig
  type key
  type param
  val active : param -> P2p.Peer_id.Set.t
  val send : param -> P2p.Peer_id.t -> key list -> unit
end

module Make_request_scheduler
    (Hash : HASH)
    (Table : Hashtbl.S with type key := Hash.t)
    (Request : REQUEST with type key := Hash.t) : sig

  type t
  val create: Request.param -> t
  val shutdown: t -> unit Lwt.t
  include SCHEDULER_EVENTS with type t := t and type key := Hash.t

end
