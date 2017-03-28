(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module type PARAMETRIZED_RO_DISTRIBUTED_DB = sig

  type t
  type key
  type value
  type param

  val known: t -> key -> bool Lwt.t
  val read: t -> key -> value option Lwt.t
  val read_exn: t -> key -> value Lwt.t

  val prefetch: t -> ?peer:P2p.Peer_id.t -> key -> param -> unit
  val fetch: t -> ?peer:P2p.Peer_id.t -> key -> param -> value Lwt.t

end

module type PARAMETRIZED_DISTRIBUTED_DB = sig

  include PARAMETRIZED_RO_DISTRIBUTED_DB

  val commit: t -> key -> unit Lwt.t
  (* val commit_invalid: t -> key -> unit Lwt.t *) (* TODO *)
  val inject: t -> key -> value -> bool Lwt.t
  val watch: t -> (key * value) Lwt_stream.t * Watcher.stopper

end

module type DISTRIBUTED_DB = sig

  include PARAMETRIZED_DISTRIBUTED_DB with type param := unit

  val prefetch: t -> ?peer:P2p.Peer_id.t -> key -> unit
  val fetch: t -> ?peer:P2p.Peer_id.t -> key -> value Lwt.t

end

module type DISK_TABLE = sig
  type store
  type key
  type value
  val known: store -> key -> bool Lwt.t
  val read: store -> key -> value tzresult Lwt.t
  val read_opt: store -> key -> value option Lwt.t
  val read_exn: store -> key -> value Lwt.t
  val store: store -> key -> value -> bool Lwt.t
  val remove: store -> key -> bool Lwt.t
end

module type MEMORY_TABLE = sig
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
  type value
  val precheck: key -> param -> value -> bool
end

module Make_table
    (Hash : sig type t end)
    (Disk_table : DISK_TABLE with type key := Hash.t)
    (Memory_table : MEMORY_TABLE with type key := Hash.t)
    (Scheduler : SCHEDULER_EVENTS with type key := Hash.t)
    (Precheck : PRECHECK with type key := Hash.t
                          and type value := Disk_table.value) : sig

  include PARAMETRIZED_DISTRIBUTED_DB with type key = Hash.t
                                       and type value = Disk_table.value
                                       and type param = Precheck.param
  val create:
    ?global_input:(key * value) Watcher.input ->
    Scheduler.t -> Disk_table.store -> t
  val notify: t -> P2p.Peer_id.t -> key -> value -> unit Lwt.t

end = struct

  type key = Hash.t
  type value = Disk_table.value
  type param = Precheck.param

  type t = {
    scheduler: Scheduler.t ;
    disk: Disk_table.store ;
    memory: status Memory_table.t ;
    global_input: (key * value) Watcher.input option ;
    input: (key * value) Watcher.input ;
  }

  and status =
    | Pending of value Lwt.u * param
    | Found of value

  let known s k =
    match Memory_table.find s.memory k with
    | exception Not_found -> Disk_table.known s.disk k
    | Pending _ -> Lwt.return_false
    | Found _ -> Lwt.return_true

  let read s k =
    match Memory_table.find s.memory k with
    | exception Not_found -> Disk_table.read_opt s.disk k
    | Found v -> Lwt.return (Some v)
    | Pending _ -> Lwt.return_none

  let read_exn s k =
    match Memory_table.find s.memory k with
    | exception Not_found -> Disk_table.read_exn s.disk k
    | Found v -> Lwt.return v
    | Pending _ -> Lwt.fail Not_found

  let fetch s ?peer k param =
    match Memory_table.find s.memory k with
    | exception Not_found -> begin
        Disk_table.read_opt s.disk k >>= function
        | None ->
          let waiter, wakener = Lwt.wait () in
          Memory_table.add s.memory k (Pending (wakener, param)) ;
          Scheduler.request s.scheduler peer k ;
          waiter
        | Some v -> Lwt.return v
      end
    | Pending (w, _) -> Lwt.waiter_of_wakener w
    | Found v -> Lwt.return v

  let prefetch s ?peer k param = Lwt.ignore_result (fetch s ?peer k param)

  let notify s p k v =
    match Memory_table.find s.memory k with
    | exception Not_found -> begin
        Disk_table.known s.disk k >>= function
        | true ->
            Scheduler.notify_duplicate s.scheduler p k ;
            Lwt.return_unit
        | false ->
            Scheduler.notify_unrequested s.scheduler p k ;
            Lwt.return_unit
      end
    | Pending (w, param) ->
        if not (Precheck.precheck k param v) then begin
          Scheduler.notify_invalid s.scheduler p k ;
          Lwt.return_unit
        end else begin
          Scheduler.notify s.scheduler p k ;
          Memory_table.replace s.memory k (Found v) ;
          Lwt.wakeup w v ;
          iter_option s.global_input
            ~f:(fun input -> Watcher.notify input (k, v)) ;
          Watcher.notify s.input (k, v) ;
          Lwt.return_unit
        end
    | Found _ ->
        Scheduler.notify_duplicate s.scheduler p k ;
        Lwt.return_unit

  let inject s k v =
    match Memory_table.find s.memory k with
    | exception Not_found -> begin
        Disk_table.known s.disk k >>= function
        | true ->
            Lwt.return_false
        | false ->
            Memory_table.add s.memory k (Found v) ;
            Lwt.return_true
      end
    | Pending _
    | Found _ ->
        Lwt.return_false

  let commit s k =
    match Memory_table.find s.memory k with
    | exception Not_found -> Lwt.return_unit
    | Pending _ -> assert false
    | Found v ->
        Disk_table.store s.disk k v >>= fun _ ->
        Memory_table.remove s.memory k ;
        Lwt.return_unit

  let watch s = Watcher.create_stream s.input

  let create ?global_input scheduler disk =
    let memory = Memory_table.create 17 in
    let input = Watcher.create_input () in
    { scheduler ; disk ; memory ; input ; global_input }

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

end = struct

  type key = Hash.t
  type param = Request.param

  type t = {
    push_to_worker: event -> unit ;
    cancel_worker: unit -> unit Lwt.t ;
    worker: unit Lwt.t ;
  }

  and event =
    | Request of P2p.Peer_id.t option * key
    | Notify of P2p.Peer_id.t * key
    | Notify_invalid of P2p.Peer_id.t * key
    | Notify_duplicate of P2p.Peer_id.t * key
    | Notify_unrequested of P2p.Peer_id.t * key

  let request t p k =
    t.push_to_worker (Request (p, k))
  let notify t p k =
    t.push_to_worker (Notify (p, k))
  let notify_invalid t p k =
    t.push_to_worker (Notify_invalid (p, k))
  let notify_duplicate t p k =
    t.push_to_worker (Notify_duplicate (p, k))
  let notify_unrequested t p k =
    t.push_to_worker (Notify_unrequested (p, k))

  type worker_state = {
    param: Request.param ;
    pending: status Table.t ;
    cancelation: unit -> unit Lwt.t ;
    wait_events: unit -> event list Lwt.t ;
  }

  and status = {
    peers: P2p.Peer_id.Set.t ;
    next_request: float ;
    delay: float ;
  }

  let compute_timeout state =
    let next =
      Table.fold
        (fun _ { next_request } acc -> min next_request acc)
        state.pending infinity in
    let now = Unix.gettimeofday () in
    let delay = next -. now in
    if delay <= 0. then Lwt.return_unit else Lwt_unix.sleep delay

  let process_event state = function
    | Request (peer, key) -> begin
        try
          let data = Table.find state.pending key in
          let peers =
            match peer with
            | None -> data.peers
            | Some peer -> P2p.Peer_id.Set.add peer data.peers in
          Table.replace state.pending key { data with peers } ;
          Lwt.return_unit
        with Not_found ->
          let peers =
            match peer with
            | None -> P2p.Peer_id.Set.empty
            | Some peer -> P2p.Peer_id.Set.singleton peer in
          Table.add state.pending key {
            peers ;
            next_request = Unix.gettimeofday () ;
            delay = 1.0 ;
          } ;
          Lwt.return_unit
      end
    | Notify (_gid, key) ->
        Table.remove state.pending key ;
        Lwt.return_unit
    | Notify_invalid _
    | Notify_unrequested _
    | Notify_duplicate _ ->
        (* TODO *)
        Lwt.return_unit

  let worker_loop state =
    let process = process_event state in
    let rec loop () =
      let shutdown = state.cancelation () >|= fun () -> `Shutdown
      and timeout = compute_timeout state >|= fun () -> `Timeout
      and events = state.wait_events () >|= fun events -> `Events events in
      Lwt.pick [ timeout ; events ; shutdown ] >>= function
      | `Shutdown -> Lwt.return_unit
      | `Events events ->
          Lwt_list.iter_s process events >>= fun () ->
          loop ()
      | `Timeout ->
          let now = Unix.gettimeofday () in
          let active_peers = Request.active state.param in
          let requests =
            Table.fold
              (fun key { peers ; next_request ; delay } acc ->
                 if next_request > now +. 0.2 then
                   acc
                 else
                   let still_peers = P2p.Peer_id.Set.inter peers active_peers in
                   if P2p.Peer_id.Set.is_empty still_peers &&
                      not (P2p.Peer_id.Set.is_empty peers) then
                     ( Table.remove state.pending key ; acc )
                   else
                     let requested_peers =
                       if P2p.Peer_id.Set.is_empty peers
                       then active_peers
                       else peers in
                     let next = { peers = still_peers ;
                                  next_request = now +. delay ;
                                  delay = delay *. 1.2 } in
                     Table.replace state.pending key next ;
                     P2p.Peer_id.Set.fold
                       (fun gid acc ->
                          let requests =
                            try key :: P2p_types.Peer_id.Map.find gid acc
                            with Not_found -> [key] in
                          P2p_types.Peer_id.Map.add gid requests acc)
                       requested_peers
                       acc)
              state.pending P2p_types.Peer_id.Map.empty in
          P2p_types.Peer_id.Map.iter (Request.send state.param) requests ;
          loop ()
    in
    loop

  let create param =
    let cancelation, cancel_worker, _ = Lwt_utils.canceler () in
    let push_to_worker, wait_events = Lwt_utils.queue () in
    let pending = Table.create 17 in
    let worker_state =
      { cancelation ; wait_events ; pending ; param } in
    let worker =
      Lwt_utils.worker "db_request_scheduler"
        ~run:(worker_loop worker_state)
        ~cancel:cancel_worker in
    { cancel_worker ; push_to_worker ; worker }

  let shutdown s =
    s.cancel_worker () >>= fun () ->
    s.worker

end
