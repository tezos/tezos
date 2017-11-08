(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Canceler = Lwt_utils.Canceler

module type DISTRIBUTED_DB = sig

  type t
  type key
  type value
  type param

  val known: t -> key -> bool Lwt.t

  type error += Missing_data of key
  type error += Canceled of key
  type error += Timeout of key

  val read: t -> key -> value tzresult Lwt.t
  val read_opt: t -> key -> value option Lwt.t
  val read_exn: t -> key -> value Lwt.t

  val prefetch: t -> ?peer:P2p.Peer_id.t -> key -> param -> unit
  val fetch:
    t ->
    ?peer:P2p.Peer_id.t ->
    ?timeout:float ->
    key -> param -> value tzresult Lwt.t

  val clear_or_cancel: t -> key -> unit
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
  val notify_cancelation: t -> key -> unit
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
    ?global_input:(key * value) Watcher.input ->
    Scheduler.t -> Disk_table.store -> t
  val notify: t -> P2p.Peer_id.t -> key -> Precheck.notified_value -> unit Lwt.t

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
    | Pending of { wakener : value tzresult Lwt.u ;
                   mutable waiters : int ;
                   param : param }
    | Found of value

  let known s k =
    match Memory_table.find s.memory k with
    | exception Not_found -> Disk_table.known s.disk k
    | Pending _ -> Lwt.return_false
    | Found _ -> Lwt.return_true

  let read_opt s k =
    match Memory_table.find s.memory k with
    | exception Not_found -> Disk_table.read_opt s.disk k
    | Found v -> Lwt.return (Some v)
    | Pending _ -> Lwt.return_none

  let read_exn s k =
    match Memory_table.find s.memory k with
    | exception Not_found -> Disk_table.read_exn s.disk k
    | Found v -> Lwt.return v
    | Pending _ -> Lwt.fail Not_found

  type error += Missing_data of key
  type error += Canceled of key
  type error += Timeout of key

  let () =
    Error_monad.register_error_kind `Permanent
      ~id: ("distributed_db." ^ Hash.name ^ ".missing")
      ~title: ("Missing " ^ Hash.name)
      ~description: ("Some " ^ Hash.name ^ " is missing from the distributed db")
      ~pp: (fun ppf key ->
          Format.fprintf ppf "Missing %s %a" Hash.name Hash.pp key)
      (Data_encoding.obj1 (Data_encoding.req "key" Hash.encoding))
      (function Missing_data key -> Some key | _ -> None)
      (fun key -> Missing_data key)

  let read s k =
    match Memory_table.find s.memory k with
    | exception Not_found ->
        trace (Missing_data k) @@
        Disk_table.read s.disk k
    | Found v -> return v
    | Pending _ -> fail (Missing_data k)

  let wrap s k ?timeout t =
    let t = Lwt.protected t in
    Lwt.on_cancel t begin fun () ->
      match Memory_table.find s.memory k with
      | exception Not_found -> ()
      | Found _ -> ()
      | Pending data ->
          data.waiters <- data.waiters - 1 ;
          if data.waiters = 0 then begin
            Memory_table.remove s.memory k ;
            Scheduler.notify_cancelation s.scheduler k ;
          end
    end ;
    match timeout with
    | None -> t
    | Some delay ->
        let timeout =
          Lwt_unix.sleep delay >>= fun () -> fail (Timeout k) in
        Lwt.pick [ t ; timeout ]

  let fetch s ?peer ?timeout k param =
    match Memory_table.find s.memory k with
    | exception Not_found -> begin
        Disk_table.read_opt s.disk k >>= function
        | Some v -> return v
        | None ->
            match Memory_table.find s.memory k with
            | exception Not_found -> begin
                let waiter, wakener = Lwt.wait () in
                Memory_table.add s.memory k
                  (Pending { wakener ; waiters =  1 ; param }) ;
                Scheduler.request s.scheduler peer k ;
                wrap s k ?timeout waiter
              end
            | Pending data ->
                Scheduler.request s.scheduler peer k ;
                data.waiters <- data.waiters + 1 ;
                wrap s k ?timeout (Lwt.waiter_of_wakener data.wakener)
            | Found v -> return v
      end
    | Pending data ->
        Scheduler.request s.scheduler peer k ;
        data.waiters <- data.waiters + 1 ;
        wrap s k ?timeout (Lwt.waiter_of_wakener data.wakener)
    | Found v -> return v

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
    | Pending { wakener = w ; param } -> begin
        match Precheck.precheck k param v with
        | None ->
            Scheduler.notify_invalid s.scheduler p k ;
            Lwt.return_unit
        | Some v ->
            Scheduler.notify s.scheduler p k ;
            Memory_table.replace s.memory k (Found v) ;
            Lwt.wakeup_later w (Ok v) ;
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

  let clear_or_cancel s k =
    match Memory_table.find s.memory k with
    | exception Not_found -> ()
    | Pending { wakener = w ; _ } ->
        Scheduler.notify_cancelation s.scheduler k ;
        Memory_table.remove s.memory k ;
        Lwt.wakeup_later w (Error [Canceled k])
    | Found _ -> Memory_table.remove s.memory k

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
    (Hash : sig
       type t
       val name : string
       val encoding : t Data_encoding.t
       val pp : Format.formatter -> t -> unit
     end)
    (Table : MEMORY_TABLE with type key := Hash.t)
    (Request : REQUEST with type key := Hash.t) : sig

  type t
  val create: Request.param -> t
  val shutdown: t -> unit Lwt.t
  include SCHEDULER_EVENTS with type t := t and type key := Hash.t

end = struct

  include Logging.Make(struct let name = "node.distributed_db.scheduler." ^ Hash.name end)

  type key = Hash.t
  type param = Request.param

  type t = {
    param: Request.param ;
    pending: status Table.t ;

    queue: event Lwt_pipe.t ;
    mutable events: event list Lwt.t ;

    canceler: Canceler.t ;
    mutable worker: unit Lwt.t ;
  }

  and status = {
    peers: P2p.Peer_id.Set.t ;
    next_request: float ;
    delay: float ;
  }

  and event =
    | Request of P2p.Peer_id.t option * key
    | Notify of P2p.Peer_id.t * key
    | Notify_cancelation of key
    | Notify_invalid of P2p.Peer_id.t * key
    | Notify_duplicate of P2p.Peer_id.t * key
    | Notify_unrequested of P2p.Peer_id.t * key

  let request t p k =
    assert (Lwt_pipe.push_now t.queue (Request (p, k)))
  let notify t p k =
    debug "push received %a from %a"
      Hash.pp k P2p.Peer_id.pp_short p ;
    assert (Lwt_pipe.push_now t.queue (Notify (p, k)))
  let notify_cancelation t k =
    debug "push cancelation %a"
      Hash.pp k ;
    assert (Lwt_pipe.push_now t.queue (Notify_cancelation k))
  let notify_invalid t p k =
    debug "push received invalid %a from %a"
      Hash.pp k P2p.Peer_id.pp_short p ;
    assert (Lwt_pipe.push_now t.queue (Notify_invalid (p, k)))
  let notify_duplicate t p k =
    debug "push received duplicate %a from %a"
      Hash.pp k P2p.Peer_id.pp_short p ;
    assert (Lwt_pipe.push_now t.queue (Notify_duplicate (p, k)))
  let notify_unrequested t p k =
    debug "push received unrequested %a from %a"
      Hash.pp k P2p.Peer_id.pp_short p ;
    assert (Lwt_pipe.push_now t.queue (Notify_unrequested (p, k)))

  let compute_timeout state =
    let next =
      Table.fold
        (fun _ { next_request } acc -> min next_request acc)
        state.pending infinity in
    let now = Unix.gettimeofday () in
    let delay = next -. now in
    if delay <= 0. then Lwt.return_unit else begin
      (* lwt_debug "waiting at least %.2fs" delay >>= fun () -> *)
      Lwt_unix.sleep delay
    end

  let may_pp_peer ppf = function
    | None -> ()
    | Some peer -> P2p.Peer_id.pp_short ppf peer

  (* TODO should depend on the ressource kind... *)
  let initial_delay = 0.1

  let process_event state now = function
    | Request (peer, key) -> begin
        lwt_debug "registering request %a from %a"
          Hash.pp key may_pp_peer peer >>= fun () ->
        try
          let data = Table.find state.pending key in
          let peers =
            match peer with
            | None -> data.peers
            | Some peer -> P2p.Peer_id.Set.add peer data.peers in
          Table.replace state.pending key {
            delay = initial_delay ;
            next_request = min data.next_request (now +. initial_delay) ;
            peers ;
          } ;
          lwt_debug "registering request %a from %a -> replaced"
            Hash.pp key may_pp_peer peer >>= fun () ->
          Lwt.return_unit
        with Not_found ->
          let peers =
            match peer with
            | None -> P2p.Peer_id.Set.empty
            | Some peer -> P2p.Peer_id.Set.singleton peer in
          Table.add state.pending key {
            peers ;
            next_request = now ;
            delay = initial_delay ;
          } ;
          lwt_debug "registering request %a from %a -> added"
            Hash.pp key may_pp_peer peer >>= fun () ->
          Lwt.return_unit
      end
    | Notify (peer, key) ->
        Table.remove state.pending key ;
        lwt_debug "received %a from %a"
          Hash.pp key P2p.Peer_id.pp_short peer >>= fun () ->
        Lwt.return_unit
    | Notify_cancelation key ->
        Table.remove state.pending key ;
        lwt_debug "canceled %a"
          Hash.pp key >>= fun () ->
        Lwt.return_unit
    | Notify_invalid (peer, key) ->
        lwt_debug "received invalid %a from %a"
          Hash.pp key P2p.Peer_id.pp_short peer >>= fun () ->
        (* TODO *)
        Lwt.return_unit
    | Notify_unrequested (peer, key) ->
        lwt_debug "received unrequested %a from %a"
          Hash.pp key P2p.Peer_id.pp_short peer >>= fun () ->
        (* TODO *)
        Lwt.return_unit
    | Notify_duplicate (peer, key) ->
        lwt_debug "received duplicate %a from %a"
          Hash.pp key P2p.Peer_id.pp_short peer >>= fun () ->
        (* TODO *)
        Lwt.return_unit

  let rec worker_loop state =
    let shutdown = Canceler.cancelation state.canceler
    and timeout = compute_timeout state in
    Lwt.choose
      [ (state.events >|= fun _ -> ()) ; timeout ; shutdown ] >>= fun () ->
    if Lwt.state shutdown <> Lwt.Sleep then
      lwt_debug "terminating" >>= fun () ->
      Lwt.return_unit
    else if Lwt.state state.events <> Lwt.Sleep then
      let now = Unix.gettimeofday () in
      state.events >>= fun events ->
      state.events <- Lwt_pipe.pop_all state.queue ;
      Lwt_list.iter_s (process_event state now) events >>= fun () ->
      worker_loop state
    else
      lwt_debug "timeout" >>= fun () ->
      let now = Unix.gettimeofday () in
      let active_peers = Request.active state.param in
      let requests =
        Table.fold
          (fun key { peers ; next_request ; delay } acc ->
             if next_request > now +. 0.2 then
               acc
             else
               let remaining_peers =
                 P2p.Peer_id.Set.inter peers active_peers in
               if P2p.Peer_id.Set.is_empty remaining_peers &&
                  not (P2p.Peer_id.Set.is_empty peers) then
                 ( Table.remove state.pending key ; acc )
               else
                 let requested_peer =
                   P2p.Peer_id.random_set_elt
                     (if P2p.Peer_id.Set.is_empty remaining_peers
                      then active_peers
                      else remaining_peers) in
                 let next = { peers = remaining_peers ;
                              next_request = now +. delay ;
                              delay = delay *. 1.2 } in
                 Table.replace state.pending key next ;
                 let requests =
                   try key :: P2p_types.Peer_id.Map.find requested_peer acc
                   with Not_found -> [key] in
                 P2p_types.Peer_id.Map.add requested_peer requests acc)
          state.pending P2p_types.Peer_id.Map.empty in
      P2p_types.Peer_id.Map.iter (Request.send state.param) requests ;
      P2p_types.Peer_id.Map.fold begin fun peer request acc ->
        acc >>= fun () ->
        Lwt_list.iter_s (fun key ->
            lwt_debug "requested %a from %a"
              Hash.pp key P2p.Peer_id.pp_short peer)
          request
      end requests Lwt.return_unit >>= fun () ->
      worker_loop state

  let create param =
    let state = {
      param ;
      queue = Lwt_pipe.create () ;
      pending = Table.create 17 ;
      events = Lwt.return [] ;
      canceler = Canceler.create () ;
      worker = Lwt.return_unit ;
    } in
    state.worker <-
      Lwt_utils.worker "db_request_scheduler"
        ~run:(fun () -> worker_loop state)
        ~cancel:(fun () -> Canceler.cancel state.canceler) ;
    state

  let shutdown s =
    Canceler.cancel s.canceler >>= fun () ->
    s.worker

end
