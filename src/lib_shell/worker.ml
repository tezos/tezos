(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module type NAME = sig
  val base : string list
  type t
  val encoding : t Data_encoding.t
  val pp : Format.formatter -> t -> unit
end

module type EVENT = sig
  type t

  val level : t -> Logging.level
  val encoding : t Data_encoding.t
  val pp : Format.formatter -> t -> unit
end

module type REQUEST = sig
  type 'a t
  type view

  val view : 'a t -> view
  val encoding : view Data_encoding.t
  val pp : Format.formatter -> view -> unit
end

module type TYPES = sig
  type state
  type parameters
  type view

  val view : state -> parameters -> view
  val encoding : view Data_encoding.t
  val pp : Format.formatter -> view -> unit
end

module Make
    (Name : NAME)
    (Event : EVENT)
    (Request : REQUEST)
    (Types : TYPES) = struct

  let base_name = String.concat "." Name.base

  module Logger = Logging.Make(struct let name = base_name end)

  type message = Message: 'a Request.t * 'a tzresult Lwt.u option -> message

  type 'a queue and bounded and infinite
  type dropbox

  type _ buffer_kind =
    | Queue : infinite queue buffer_kind
    | Bounded : { size : int } -> bounded queue buffer_kind
    | Dropbox :
        { merge : (dropbox t ->
                   any_request ->
                   any_request option ->
                   any_request option) }
      -> dropbox buffer_kind
  and any_request = Any_request : _ Request.t -> any_request

  and _ buffer =
    | Queue_buffer : (Time.t * message) Lwt_pipe.t -> infinite queue buffer
    | Bounded_buffer : (Time.t * message) Lwt_pipe.t -> bounded queue buffer
    | Dropbox_buffer : (Time.t * message) Lwt_dropbox.t -> dropbox buffer

  and 'kind t = {
    limits : Worker_types.limits ;
    timeout : float option ;
    parameters : Types.parameters ;
    mutable (* only for init *) worker : unit Lwt.t ;
    mutable (* only for init *) state : Types.state option ;
    buffer : 'kind buffer ;
    event_log : (Logging.level * Event.t Ring.t) list ;
    canceler : Lwt_canceler.t ;
    name : Name.t ;
    id : int ;
    mutable status : Worker_types.worker_status ;
    mutable current_request : (Time.t * Time.t * Request.view) option ;
    table : 'kind table ;
  }
  and 'kind table = {
    buffer_kind : 'kind buffer_kind ;
    mutable last_id : int ;
    instances : (Name.t, 'kind t) Hashtbl.t ;
    zombies : (int, 'kind t) Hashtbl.t
  }

  type error += Closed of Name.t

  let () =
    register_error_kind `Permanent
      ~id:("worker." ^ base_name ^ ".closed")
      ~title:("Worker " ^ base_name ^ " closed")
      ~description:
        ("An operation on a " ^ base_name ^
         " worker could not complete \
          before it was shut down.")
      ~pp: (fun ppf name ->
          Format.fprintf ppf
            "Worker %s[%a] has been shut down."
            base_name Name.pp name)
      Data_encoding.(obj1 (req "worker_id" Name.encoding))
      (function Closed net_id -> Some net_id | _ -> None)
      (fun net_id -> Closed net_id)

  let queue_item ?u r =
    Time.now (),
    Message (r, u)

  let drop_request (w : dropbox t) request =
    let Dropbox { merge } = w.table.buffer_kind in
    let Dropbox_buffer message_box = w.buffer in
    try
      match
        match Lwt_dropbox.peek message_box with
        | None ->
            merge w (Any_request request) None
        | Some (_, Message (old, _)) ->
            Lwt.ignore_result (Lwt_dropbox.take message_box) ;
            merge w (Any_request request) (Some (Any_request old))
      with
      | None -> ()
      | Some (Any_request neu) ->
          Lwt_dropbox.put message_box (Time.now (), Message (neu, None))
    with Lwt_dropbox.Closed -> ()

  let push_request (type a) (w : a queue t) request =
    match w.buffer with
    | Queue_buffer message_queue ->
        Lwt_pipe.push message_queue (queue_item request)
    | Bounded_buffer message_queue ->
        Lwt_pipe.push message_queue (queue_item request)

  let push_request_now (w : infinite queue t) request =
    let Queue_buffer message_queue = w.buffer in
    Lwt_pipe.push_now_exn message_queue (queue_item request)

  let try_push_request_now (w : bounded queue t) request =
    let Bounded_buffer message_queue = w.buffer in
    Lwt_pipe.push_now message_queue (queue_item request)

  let push_request_and_wait (type a) (w : a queue t) request =
    let message_queue = match w.buffer with
      | Queue_buffer message_queue -> message_queue
      | Bounded_buffer message_queue -> message_queue in
    let t, u = Lwt.wait () in
    Lwt.catch
      (fun () ->
         Lwt_pipe.push message_queue (queue_item ~u request) >>= fun () ->
         t)
      (function
        | Lwt_pipe.Closed -> fail (Closed w.name)
        | exn -> fail (Exn exn))

  let close (type a) (w : a t) =
    let wakeup = function
      | _, Message (_, Some u) ->
          Lwt.wakeup_later u (Error [ Closed w.name ])
      | _ -> () in
    let close_queue message_queue =
      let messages = Lwt_pipe.pop_all_now message_queue in
      List.iter wakeup messages ;
      Lwt_pipe.close message_queue in
    match w.buffer with
    | Queue_buffer message_queue -> close_queue message_queue
    | Bounded_buffer message_queue -> close_queue message_queue
    | Dropbox_buffer message_box ->
        Option.iter ~f:wakeup (Lwt_dropbox.peek message_box) ;
        Lwt_dropbox.close message_box

  let pop (type a) (w : a t) =
    let pop_queue message_queue =
      match w.timeout with
      | None ->
          Lwt_pipe.pop message_queue >>= fun m ->
          return (Some m)
      | Some timeout ->
          Lwt_pipe.pop_with_timeout timeout message_queue >>= fun m ->
          return m in
    match w.buffer with
    | Queue_buffer message_queue -> pop_queue message_queue
    | Bounded_buffer message_queue -> pop_queue message_queue
    | Dropbox_buffer message_box ->
        match w.timeout with
        | None ->
            Lwt_dropbox.take message_box >>= fun m ->
            return (Some m)
        | Some timeout ->
            Lwt_dropbox.take_with_timeout timeout message_box >>= fun m ->
            return m

  let trigger_shutdown w =
    Lwt.ignore_result (Lwt_canceler.cancel w.canceler)

  let protect { canceler } ?on_error f =
    Lwt_utils.protect ?on_error ~canceler f

  let canceler { canceler } = canceler

  let log_event w evt =
    let level = Event.level evt in
    let log =
      match level with
      | Debug -> Logger.lwt_debug
      | Info -> Logger.lwt_log_info
      | Notice -> Logger.lwt_log_notice
      | Warning -> Logger.lwt_warn
      | Error -> Logger.lwt_log_error
      | Fatal -> Logger.lwt_fatal_error in
    log "[%a] %a" Name.pp w.name Event.pp evt >>= fun () ->
    begin if level >= w.limits.backlog_level then
        Ring.add (List.assoc level w.event_log) evt
    end ;
    Lwt.return_unit

  let record_event w evt =
    Lwt.ignore_result (log_event w evt)

  module type HANDLERS = sig
    type self
    val on_launch :
      self -> Name.t -> Types.parameters -> Types.state Lwt.t
    val on_request :
      self -> 'a Request.t -> 'a tzresult Lwt.t
    val on_no_request :
      self -> unit tzresult Lwt.t
    val on_close :
      self -> unit Lwt.t
    val on_error :
      self -> Request.view -> Worker_types.request_status -> error list -> unit tzresult Lwt.t
    val on_completion :
      self -> 'a Request.t -> 'a -> Worker_types.request_status -> unit Lwt.t
  end

  let create_table buffer_kind =
    { buffer_kind ;
      last_id = 0 ;
      instances = Hashtbl.create 10 ;
      zombies = Hashtbl.create 10 }

  let worker_loop (type kind) handlers (w : kind t) =
    let (module Handlers : HANDLERS with type self = kind t) = handlers in
    let do_close errs =
      let t0 = match w.status with
        | Running t0 -> t0
        | _ -> assert false in
      w.status <- Closing (t0, Time.now ()) ;
      Handlers.on_close w >>= fun () ->
      close w ;
      Lwt_canceler.cancel w.canceler >>= fun () ->
      w.status <- Closed (t0, Time.now (), errs) ;
      w.state <- None ;
      Hashtbl.remove w.table.instances w.name ;
      Hashtbl.add w.table.zombies w.id w ;
      Lwt.ignore_result
        (Lwt_unix.sleep w.limits.zombie_memory >>= fun () ->
         List.iter (fun (_, ring) -> Ring.clear ring) w.event_log ;
         Lwt_unix.sleep (w.limits.zombie_lifetime -. w.limits.zombie_memory) >>= fun () ->
         Hashtbl.remove w.table.zombies w.id ;
         Lwt.return ()) ;
      Lwt.return_unit in
    let rec loop () =
      begin
        Lwt_utils.protect ~canceler:w.canceler begin fun () ->
          pop w
        end >>=? function
        | None -> Handlers.on_no_request w
        | Some (pushed, Message (request, u)) ->
            let current_request = Request.view request in
            let treated = Time.now () in
            w.current_request <- Some (pushed, treated, current_request) ;
            Logger.debug "[%a] request: @[%a@]"
              Name.pp w.name
              Request.pp current_request ;
            match u with
            | None ->
                Handlers.on_request w request >>=? fun res ->
                let completed = Time.now () in
                w.current_request <- None ;
                Handlers.on_completion w
                  request res Worker_types.{ pushed ; treated ; completed } >>= fun () ->
                return ()
            | Some u ->
                Handlers.on_request w request >>= fun res ->
                Lwt.wakeup_later u res ;
                Lwt.return res >>=? fun res ->
                let completed = Time.now () in
                w.current_request <- None ;
                Handlers.on_completion w
                  request res Worker_types.{ pushed ; treated ; completed } >>= fun () ->
                return ()
      end >>= function
      | Ok () ->
          loop ()
      | Error [Lwt_utils.Canceled | Exn Lwt_pipe.Closed | Exn Lwt_dropbox.Closed ] ->
          Logger.lwt_log_info
            "[%a] worker terminated"
            Name.pp w.name >>= fun () ->
          do_close None
      | Error errs ->
          begin match w.current_request with
            | Some (pushed, treated, request) ->
                let completed = Time.now () in
                w.current_request <- None ;
                Handlers.on_error w
                  request Worker_types.{ pushed ; treated ; completed } errs
            | None -> assert false
          end >>= function
          | Ok () ->
              loop ()
          | Error errs ->
              Logger.lwt_log_error
                "@[[%a] worker crashed:@ %a@]"
                Name.pp w.name
                pp_print_error errs >>= fun () ->
              do_close (Some errs) in
    loop ()

  let launch
    : type kind.
      kind table -> ?timeout:float ->
      Worker_types.limits -> Name.t -> Types.parameters ->
      (module HANDLERS with type self = kind t) ->
      kind t Lwt.t
    = fun table ?timeout limits name parameters (module Handlers) ->
      if Hashtbl.mem table.instances name then
        invalid_arg
          (Format.asprintf
             "Lwt_worker.launch: \
              duplicate worker %s[%a]" base_name Name.pp name) ;
      Logger.lwt_log_info
        "[%a] worker started"
        Name.pp name >>= fun () ->
      let canceler = Lwt_canceler.create () in
      let buffer : kind buffer =
        match table.buffer_kind with
        | Queue ->
            Queue_buffer (Lwt_pipe.create ())
        | Bounded { size } ->
            Bounded_buffer (Lwt_pipe.create ~size:(size, (fun _ -> 1)) ())
        | Dropbox _ ->
            Dropbox_buffer (Lwt_dropbox.create ()) in
      let event_log =
        let levels =
          [ Logging.Debug ; Info ; Notice ; Warning ; Error ; Fatal ] in
        List.map (fun l -> l, Ring.create limits.backlog_size) levels in
      let w = { limits ; parameters ; name ; canceler ;
                table ; buffer ;
                state = None ; id = (table.last_id <- table.last_id + 1; table.last_id) ;
                worker = Lwt.return_unit ;
                event_log ; timeout ;
                current_request = None ;
                status = Launching (Time.now ())} in
      Hashtbl.add table.instances name w ;
      Handlers.on_launch w name parameters >>= fun state ->
      w.status <- Running (Time.now ()) ;
      w.state <- Some state ;
      w.worker <-
        Lwt_utils.worker
          (Format.asprintf "%s[%a]"
             base_name
             Name.pp w.name)
          ~run:(fun () -> worker_loop (module Handlers) w)
          ~cancel:(fun () -> Lwt_canceler.cancel w.canceler) ;
      Lwt.return w

  let shutdown w =
    Logger.lwt_debug "triggering shutdown" >>= fun () ->
    Lwt_canceler.cancel w.canceler >>= fun () ->
    w.worker

  let state w =
    match w.state with
    | None ->
        invalid_arg
          "Lwt_worker.state: \
           state called before worker was initialized"
    | Some state -> state

  let last_events w =
    List.map
      (fun (level, ring) -> (level, Ring.elements ring))
      w.event_log

  let pending_requests (type a) (w : a queue t) =
    let message_queue = match w.buffer with
      | Queue_buffer message_queue -> message_queue
      | Bounded_buffer message_queue -> message_queue in
    List.map
      (function (t, Message (req, _)) -> t, Request.view req)
      (Lwt_pipe.peek_all message_queue)

  let status { status } = status

  let current_request { current_request } = current_request

  let view w =
    Types.view (state w) w.parameters

  let list { instances } =
    Hashtbl.fold
      (fun n w acc -> (n, w) :: acc)
      instances []

end
