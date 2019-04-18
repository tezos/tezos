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

  val prefetch:
    t ->
    ?peer:P2p_peer.Id.t ->
    ?timeout:Time.System.Span.t ->
    key -> param -> unit

  val fetch:
    t ->
    ?peer:P2p_peer.Id.t ->
    ?timeout:Time.System.Span.t ->
    key -> param -> value tzresult Lwt.t

  val clear_or_cancel: t -> key -> unit
  val resolve_pending: t -> key -> value -> unit
  val inject: t -> key -> value -> bool Lwt.t
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

end = struct

  type key = Hash.t
  type value = Disk_table.value
  type param = Precheck.param

  type t = {
    scheduler: Scheduler.t ;
    disk: Disk_table.store ;
    memory: status Memory_table.t ;
    global_input: (key * value) Lwt_watcher.input option ;
    input: (key * value) Lwt_watcher.input ;
  }

  and status =
    | Pending of { waiter : value tzresult Lwt.t ;
                   wakener : value tzresult Lwt.u ;
                   mutable waiters : int ;
                   param : param }
    | Found of value

  let known s k =
    match Memory_table.find_opt s.memory k with
    | None -> Disk_table.known s.disk k
    | Some (Pending _) -> Lwt.return_false
    | Some (Found _) -> Lwt.return_true

  let read_opt s k =
    match Memory_table.find_opt s.memory k with
    | None -> Disk_table.read_opt s.disk k
    | Some (Found v) -> Lwt.return_some v
    | Some (Pending _) -> Lwt.return_none

  type error += Missing_data of key
  type error += Canceled of key
  type error += Timeout of key

  let () =
    (* Missing data key *)
    register_error_kind
      `Permanent
      ~id: ("distributed_db." ^ Hash.name ^ ".missing")
      ~title: ("Missing " ^ Hash.name)
      ~description: ("Some " ^ Hash.name ^ " is missing from the distributed db")
      ~pp: (fun ppf key ->
          Format.fprintf ppf "Missing %s %a" Hash.name Hash.pp key)
      (Data_encoding.obj1 (Data_encoding.req "key" Hash.encoding))
      (function Missing_data key -> Some key | _ -> None)
      (fun key -> Missing_data key) ;
    (* Canceled key *)
    register_error_kind
      `Permanent
      ~title: ("Canceled fetch of a " ^ Hash.name)
      ~description: ("The fetch of a " ^ Hash.name ^ " has been canceled")
      ~id: ("distributed_db." ^ Hash.name ^ ".fetch_canceled")
      ~pp: (fun ppf key ->
          Format.fprintf ppf "Fetch of %s %a canceled" Hash.name Hash.pp key)
      Data_encoding.(obj1 (req "key" Hash.encoding))
      (function (Canceled key) -> Some key | _ -> None)
      (fun key -> Canceled key) ;
    (* Timeout key *)
    register_error_kind
      `Permanent
      ~title: ("Timed out fetch of a " ^ Hash.name)
      ~description: ("The fetch of a " ^ Hash.name ^ " has timed out")
      ~id: ("distributed_db." ^ Hash.name ^ ".fetch_timeout")
      ~pp: (fun ppf key ->
          Format.fprintf ppf "Fetch of %s %a timed out" Hash.name Hash.pp key)
      Data_encoding.(obj1 (req "key" Hash.encoding))
      (function (Timeout key) -> Some key | _ -> None)
      (fun key -> Timeout key)

  let read s k =
    match Memory_table.find_opt s.memory k with
    | None ->
        trace (Missing_data k) @@
        Disk_table.read s.disk k
    | Some (Found v) -> return v
    | Some (Pending _) -> fail (Missing_data k)

  let wrap s k ?timeout t =
    let t = Lwt.protected t in
    Lwt.on_cancel t begin fun () ->
      match Memory_table.find_opt s.memory k with
      | None -> ()
      | Some (Found _) -> ()
      | Some (Pending data) ->
          data.waiters <- data.waiters - 1 ;
          if data.waiters = 0 then begin
            Memory_table.remove s.memory k ;
            Scheduler.notify_cancelation s.scheduler k ;
          end
    end ;
    match timeout with
    | None -> t
    | Some delay ->
        let timeout = Systime_os.sleep delay >>= fun () -> fail (Timeout k) in
        Lwt.pick [ t ; timeout ]

  let fetch s ?peer ?timeout k param =
    match Memory_table.find_opt s.memory k with
    | None -> begin
        Disk_table.read_opt s.disk k >>= function
        | Some v -> return v
        | None ->
            match Memory_table.find_opt s.memory k with
            | None -> begin
                let waiter, wakener = Lwt.wait () in
                Memory_table.add s.memory k
                  (Pending { waiter ; wakener ; waiters =  1 ; param }) ;
                Scheduler.request s.scheduler peer k ;
                wrap s k ?timeout waiter
              end
            | Some (Pending data) ->
                Scheduler.request s.scheduler peer k ;
                data.waiters <- data.waiters + 1 ;
                wrap s k ?timeout data.waiter
            | Some (Found v) -> return v
      end
    | Some (Pending data) ->
        Scheduler.request s.scheduler peer k ;
        data.waiters <- data.waiters + 1 ;
        wrap s k ?timeout data.waiter
    | Some (Found v) -> return v

  let prefetch s ?peer ?timeout k param =
    try ignore (fetch s ?peer ?timeout k param) with _ -> ()

  let notify s p k v =
    match Memory_table.find_opt s.memory k with
    | None -> begin
        Disk_table.known s.disk k >>= function
        | true ->
            Scheduler.notify_duplicate s.scheduler p k ;
            Lwt.return_unit
        | false ->
            Scheduler.notify_unrequested s.scheduler p k ;
            Lwt.return_unit
      end
    | Some (Pending { wakener = w ; param ; _ }) -> begin
        match Precheck.precheck k param v with
        | None ->
            Scheduler.notify_invalid s.scheduler p k ;
            Lwt.return_unit
        | Some v ->
            Scheduler.notify s.scheduler p k ;
            Memory_table.replace s.memory k (Found v) ;
            Lwt.wakeup_later w (Ok v) ;
            Option.iter s.global_input
              ~f:(fun input -> Lwt_watcher.notify input (k, v)) ;
            Lwt_watcher.notify s.input (k, v) ;
            Lwt.return_unit
      end
    | Some (Found _) ->
        Scheduler.notify_duplicate s.scheduler p k ;
        Lwt.return_unit

  let inject s k v =
    match Memory_table.find_opt s.memory k with
    | None -> begin
        Disk_table.known s.disk k >>= function
        | true ->
            Lwt.return_false
        | false ->
            Memory_table.add s.memory k (Found v) ;
            Lwt.return_true
      end
    | Some (Pending _)
    | Some (Found _) ->
        Lwt.return_false

  let resolve_pending s k v =
    match Memory_table.find_opt s.memory k with
    | Some (Pending { wakener ; _ }) ->
        Scheduler.notify_cancelation s.scheduler k ;
        Memory_table.replace s.memory k (Found v) ;
        Lwt.wakeup_later wakener (Ok v) ;
        Option.iter s.global_input
          ~f:(fun input -> Lwt_watcher.notify input (k, v)) ;
        Lwt_watcher.notify s.input (k, v) ;
    | _ -> ()

  let clear_or_cancel s k =
    match Memory_table.find_opt s.memory k with
    | None -> ()
    | Some (Pending { wakener = w ; _ }) ->
        Scheduler.notify_cancelation s.scheduler k ;
        Memory_table.remove s.memory k ;
        Lwt.wakeup_later w (Error [Canceled k])
    | Some (Found _) -> Memory_table.remove s.memory k

  let watch s = Lwt_watcher.create_stream s.input

  let create ?global_input scheduler disk =
    let memory = Memory_table.create 17 in
    let input = Lwt_watcher.create_input () in
    { scheduler ; disk ; memory ; input ; global_input }

  let pending s k =
    match Memory_table.find_opt s.memory k with
    | None -> false
    | Some (Found _) -> false
    | Some (Pending _) -> true

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

end = struct

  include Internal_event.Legacy_logging.Make_semantic
      (struct let name = "node.distributed_db.scheduler." ^ Hash.name end)

  type key = Hash.t

  type t = {
    param: Request.param ;
    pending: status Table.t ;

    queue: event Lwt_pipe.t ;
    mutable events: event list Lwt.t ;

    canceler: Lwt_canceler.t ;
    mutable worker: unit Lwt.t ;
  }

  and status = {
    peers: P2p_peer.Set.t ;
    next_request: Time.System.t ;
    delay: Time.System.Span.t ;
  }

  and event =
    | Request of P2p_peer.Id.t option * key
    | Notify of P2p_peer.Id.t * key
    | Notify_cancelation of key
    | Notify_invalid of P2p_peer.Id.t * key
    | Notify_duplicate of P2p_peer.Id.t * key
    | Notify_unrequested of P2p_peer.Id.t * key

  let request t p k =
    assert (Lwt_pipe.push_now t.queue (Request (p, k)))
  let notify t p k =
    debug Tag.DSL.(fun f ->
        f "push received %a from %a"
        -% t event "push_received"
        -% a Hash.Logging.tag k
        -% a P2p_peer.Id.Logging.tag p);
    assert (Lwt_pipe.push_now t.queue (Notify (p, k)))
  let notify_cancelation t k =
    debug Tag.DSL.(fun f ->
        f "push cancelation %a"
        -% t event "push_cancelation"
        -% a Hash.Logging.tag k);
    assert (Lwt_pipe.push_now t.queue (Notify_cancelation k))
  let notify_invalid t p k =
    debug Tag.DSL.(fun f ->
        f "push received invalid %a from %a"
        -% t event "push_received_invalid"
        -% a Hash.Logging.tag k
        -% a P2p_peer.Id.Logging.tag p);
    assert (Lwt_pipe.push_now t.queue (Notify_invalid (p, k)))
  let notify_duplicate t p k =
    debug Tag.DSL.(fun f ->
        f "push received duplicate %a from %a"
        -% t event "push_received_duplicate"
        -% a Hash.Logging.tag k
        -% a P2p_peer.Id.Logging.tag p);
    assert (Lwt_pipe.push_now t.queue (Notify_duplicate (p, k)))
  let notify_unrequested t p k =
    debug Tag.DSL.(fun f ->
        f "push received unrequested %a from %a"
        -% t event "push_received_unrequested"
        -% a Hash.Logging.tag k
        -% a P2p_peer.Id.Logging.tag p);
    assert (Lwt_pipe.push_now t.queue (Notify_unrequested (p, k)))

  let compute_timeout state =
    let next =
      Table.fold
        (fun _ { next_request ; _ } acc ->
           match acc with
           | None -> Some next_request
           | Some x -> Some (Time.System.min x next_request))
        state.pending None in
    match next with
    | None -> fst @@ Lwt.task ()
    | Some next ->
        let now = Systime_os.now () in
        let delay = Ptime.diff next now in
        if Ptime.Span.compare delay Ptime.Span.zero <= 0 then
          Lwt.return_unit
        else
          Systime_os.sleep delay


  let process_event state now = function
    | Request (peer, key) -> begin
        lwt_debug Tag.DSL.(fun f ->
            f "registering request %a from %a"
            -% t event "registering_request"
            -% a Hash.Logging.tag key
            -% a P2p_peer.Id.Logging.tag_opt peer) >>= fun () ->
        try
          let data = Table.find state.pending key in
          let peers =
            match peer with
            | None -> data.peers
            | Some peer -> P2p_peer.Set.add peer data.peers in
          let next_request =
            Option.unopt
              ~default:Ptime.max
              (Ptime.add_span now Request.initial_delay) in
          Table.replace state.pending key {
            delay = Request.initial_delay ;
            next_request ;
            peers ;
          } ;
          lwt_debug Tag.DSL.(fun f ->
              f "registering request %a from %a -> replaced"
              -% t event "registering_request_replaced"
              -% a Hash.Logging.tag key
              -% a P2p_peer.Id.Logging.tag_opt peer) >>= fun () ->
          Lwt.return_unit
        with Not_found ->
          let peers =
            match peer with
            | None -> P2p_peer.Set.empty
            | Some peer -> P2p_peer.Set.singleton peer in
          Table.add state.pending key {
            peers ;
            next_request = now ;
            delay = Request.initial_delay ;
          } ;
          lwt_debug Tag.DSL.(fun f ->
              f "registering request %a from %a -> added"
              -% t event "registering_request_added"
              -% a Hash.Logging.tag key
              -% a P2p_peer.Id.Logging.tag_opt peer) >>= fun () ->
          Lwt.return_unit
      end
    | Notify (peer, key) ->
        Table.remove state.pending key ;
        lwt_debug Tag.DSL.(fun f ->
            f "received %a from %a"
            -% t event "received"
            -% a Hash.Logging.tag key
            -% a P2p_peer.Id.Logging.tag peer) >>= fun () ->
        Lwt.return_unit
    | Notify_cancelation key ->
        Table.remove state.pending key ;
        lwt_debug Tag.DSL.(fun f ->
            f "canceled %a"
            -% t event "canceled"
            -% a Hash.Logging.tag key) >>= fun () ->
        Lwt.return_unit
    | Notify_invalid (peer, key) ->
        lwt_debug Tag.DSL.(fun f ->
            f "received invalid %a from %a"
            -% t event "received_invalid"
            -% a Hash.Logging.tag key
            -% a P2p_peer.Id.Logging.tag peer) >>= fun () ->
        (* TODO *)
        Lwt.return_unit
    | Notify_unrequested (peer, key) ->
        lwt_debug Tag.DSL.(fun f ->
            f "received unrequested %a from %a"
            -% t event "received_unrequested"
            -% a Hash.Logging.tag key
            -% a P2p_peer.Id.Logging.tag peer) >>= fun () ->
        (* TODO *)
        Lwt.return_unit
    | Notify_duplicate (peer, key) ->
        lwt_debug Tag.DSL.(fun f ->
            f "received duplicate %a from %a"
            -% t event "received_duplicate"
            -% a Hash.Logging.tag key
            -% a P2p_peer.Id.Logging.tag peer) >>= fun () ->
        (* TODO *)
        Lwt.return_unit

  let worker_loop state =
    let shutdown = Lwt_canceler.cancelation state.canceler in
    let rec loop state =
      let timeout = compute_timeout state in
      Lwt.choose
        [ (state.events >|= fun _ -> ()) ; timeout ; shutdown ] >>= fun () ->
      if Lwt.state shutdown <> Lwt.Sleep then
        lwt_debug Tag.DSL.(fun f ->
            f "terminating" -% t event "terminating") >>= fun () ->
        Lwt.return_unit
      else if Lwt.state state.events <> Lwt.Sleep then
        let now = Systime_os.now () in
        state.events >>= fun events ->
        state.events <- Lwt_pipe.pop_all state.queue ;
        Lwt_list.iter_s (process_event state now) events >>= fun () ->
        loop state
      else
        lwt_debug Tag.DSL.(fun f ->
            f "timeout" -% t event "timeout") >>= fun () ->
        let now = Systime_os.now () in
        let active_peers = Request.active state.param in
        let requests =
          Table.fold
            (fun key { peers ; next_request ; delay } acc ->
               let later =
                 Option.unopt
                   ~default:Ptime.max
                   (Ptime.add_span now (Time.System.Span.of_seconds_exn 0.2)) in
               if Ptime.is_later next_request ~than:later then
                 acc
               else
                 let remaining_peers =
                   P2p_peer.Set.inter peers active_peers in
                 if P2p_peer.Set.is_empty remaining_peers &&
                    not (P2p_peer.Set.is_empty peers) then
                   ( Table.remove state.pending key ; acc )
                 else
                   let requested_peer =
                     P2p_peer.Id.Set.random_elt
                       (if P2p_peer.Set.is_empty remaining_peers
                        then active_peers
                        else remaining_peers) in
                   let next_request =
                     Option.unopt
                       ~default:Ptime.max
                       (Ptime.add_span now delay) in
                   let next = { peers = remaining_peers ;
                                next_request ;
                                delay = Time.System.Span.multiply_exn 1.5 delay } in
                   Table.replace state.pending key next ;
                   let requests =
                     try key :: P2p_peer.Map.find requested_peer acc
                     with Not_found -> [key] in
                   P2p_peer.Map.add requested_peer requests acc)
            state.pending P2p_peer.Map.empty in
        P2p_peer.Map.iter (Request.send state.param) requests ;
        P2p_peer.Map.fold begin fun peer request acc ->
          acc >>= fun () ->
          Lwt_list.iter_s (fun key ->
              lwt_debug Tag.DSL.(fun f ->
                  f "requested %a from %a"
                  -% t event "requested"
                  -% a Hash.Logging.tag key
                  -% a P2p_peer.Id.Logging.tag peer))
            request
        end requests Lwt.return_unit >>= fun () ->
        loop state
    in
    loop state

  let create param =
    let state = {
      param ;
      queue = Lwt_pipe.create () ;
      pending = Table.create 17 ;
      events = Lwt.return_nil ;
      canceler = Lwt_canceler.create () ;
      worker = Lwt.return_unit ;
    } in
    state.worker <-
      Lwt_utils.worker "db_request_scheduler"
        ~on_event:Internal_event.Lwt_worker_event.on_event
        ~run:(fun () -> worker_loop state)
        ~cancel:(fun () -> Lwt_canceler.cancel state.canceler) ;
    state

  let shutdown s =
    Lwt_canceler.cancel s.canceler >>= fun () ->
    s.worker

end
