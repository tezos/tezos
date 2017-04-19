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
  type param

  val known: t -> key -> bool Lwt.t

  type error += Missing_data of key
  val read: t -> key -> value tzresult Lwt.t
  val read_opt: t -> key -> value option Lwt.t
  val read_exn: t -> key -> value Lwt.t

  val prefetch: t -> ?peer:P2p.Peer_id.t -> key -> param -> unit
  val fetch: t -> ?peer:P2p.Peer_id.t -> key -> param -> value Lwt.t

  val remove: t -> key -> unit Lwt.t
  val inject: t -> key -> value -> bool Lwt.t
  val watch: t -> (key * value) Lwt_stream.t * Watcher.stopper

end

module type DISK_TABLE = sig
  type store
  type key
  type value
  val known: store -> key -> bool Lwt.t
  val read: store -> key -> value tzresult Lwt.t
  val read_opt: store -> key -> value option Lwt.t
  val read_exn: store -> key -> value Lwt.t
end

module type MEMORY_TABLE = sig
  (* A subtype of Hashtbl.S *)
  type 'a t
  type key
  val create: int -> 'a t
  val find: 'a t -> key -> 'a
  val add: 'a t -> key -> 'a -> unit
  val replace: 'a t -> key -> 'a -> unit
  val remove: 'a t -> key -> unit
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module type SCHEDULER_EVENTS = sig
  type t
  type key
  val request: t -> P2p.Peer_id.t option -> key -> unit
  val notify: t -> P2p.Peer_id.t -> key -> unit
  val notify_unrequested: t -> P2p.Peer_id.t -> key -> unit
  val notify_duplicate: t -> P2p.Peer_id.t -> key -> unit
  val notify_invalid: t -> P2p.Peer_id.t -> key -> unit
end

module type PRECHECK = sig
  type key
  type param
  type notified_value
  type value
  val precheck: key -> param -> notified_value -> value option
end

module Make_table
    (Hash : sig type t end)
    (Disk_table : DISK_TABLE with type key := Hash.t)
    (Memory_table : MEMORY_TABLE with type key := Hash.t)
    (Scheduler : SCHEDULER_EVENTS with type key := Hash.t)
    (Precheck : PRECHECK with type key := Hash.t
                          and type value := Disk_table.value) : sig

  include DISTRIBUTED_DB with type key = Hash.t
                          and type value = Disk_table.value
                          and type param = Precheck.param
  val create:
    ?global_input:(key * value) Watcher.input ->
    Scheduler.t -> Disk_table.store -> t
  val notify: t -> P2p.Peer_id.t -> key -> Precheck.notified_value -> unit Lwt.t

end

module type REQUEST = sig
  type key
  type param
  val active : param -> P2p.Peer_id.Set.t
  val send : param -> P2p.Peer_id.t -> key list -> unit
end

module Make_request_scheduler
    (Hash : sig type t end)
    (Table : MEMORY_TABLE with type key := Hash.t)
    (Request : REQUEST with type key := Hash.t) : sig

  type t
  val create: Request.param -> t
  val shutdown: t -> unit Lwt.t
  include SCHEDULER_EVENTS with type t := t and type key := Hash.t

end
