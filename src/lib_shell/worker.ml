(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module type NAME = sig
  val base : string list
  type t
  val encoding : t Data_encoding.t
  val pp : Format.formatter -> t -> unit
end

module type EVENT = sig
  type t

  val level : t -> Internal_event.level
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

(** An error returned when trying to communicate with a worker that
    has been closed.*)
type worker_name = {base: string; name:string}
type Error_monad.error += Closed of worker_name

let () =
  register_error_kind `Permanent
    ~id:("worker.closed")
    ~title:("Worker closed")
    ~description:
      ("An operation on a worker could not complete \
        before it was shut down.")
    ~pp: (fun ppf w ->
        Format.fprintf ppf
          "Worker %s[%s] has been shut down."
          w.base w.name)
    Data_encoding.(
      conv
        (fun { base ; name } -> (base,name))
        (fun (name,base) -> { base ; name })
        (obj1
           (req "worker" (tup2 string string))
        )
    )
    (function Closed w -> Some w | _ -> None)
    (fun w -> Closed w)

module type T = sig

  module Name: NAME
  module Event: EVENT
  module Request: REQUEST
  module Types: TYPES

  (** A handle to a specific worker, parameterized by the type of
      internal message buffer. *)
  type 'kind t

  (** A handle to a table of workers. *)
  type 'kind table

  (** Internal buffer kinds used as parameters to {!t}. *)
  type 'a queue and bounded and infinite
  type dropbox

  (** Supported kinds of internal buffers. *)
  type _ buffer_kind =
    | Queue : infinite queue buffer_kind
    | Bounded : { size : int } -> bounded queue buffer_kind
    | Dropbox :
        { merge : (dropbox t ->
                   any_request ->
                   any_request option ->
                   any_request option) }  -> dropbox buffer_kind
  and any_request = Any_request : _ Request.t -> any_request

  (** Create a table of workers. *)
  val create_table : 'kind buffer_kind -> 'kind table

  (** The callback handlers specific to each worker instance. *)
  module type HANDLERS = sig

    (** Placeholder replaced with {!t} with the right parameters
        provided by the type of buffer chosen at {!launch}.*)
    type self

    (** Builds the initial internal state of a worker at launch.
        It is possible to initialize the message queue.
        Of course calling {!state} will fail at that point. *)
    val on_launch :
      self -> Name.t -> Types.parameters -> Types.state tzresult Lwt.t

    (** The main request processor, i.e. the body of the event loop. *)
    val on_request :
      self -> 'a Request.t -> 'a tzresult Lwt.t

    (** Called when no request has been made before the timeout, if
        the parameter has been passed to {!launch}. *)
    val on_no_request :
      self -> unit tzresult Lwt.t

    (** A function called when terminating a worker. *)
    val on_close :
      self -> unit Lwt.t

    (** A function called at the end of the worker loop in case of an
        abnormal error. This function can handle the error by
        returning [Ok ()], or leave the default unexpected error
        behaviour by returning its parameter. A possibility is to
        handle the error for ad-hoc logging, and still use
        {!trigger_shutdown} to kill the worker. *)
    val on_error :
      self ->
      Request.view ->
      Worker_types.request_status ->
      error list ->
      unit tzresult Lwt.t

    (** A function called at the end of the worker loop in case of a
        successful treatment of the current request. *)
    val on_completion :
      self ->
      'a Request.t -> 'a ->
      Worker_types.request_status ->
      unit Lwt.t

  end

  (** Creates a new worker instance.
      Parameter [queue_size] not passed means unlimited queue. *)
  val launch :
    'kind table -> ?timeout:Time.System.Span.t ->
    Worker_types.limits -> Name.t -> Types.parameters ->
    (module HANDLERS with type self = 'kind t) ->
    'kind t tzresult Lwt.t

  (** Triggers a worker termination and waits for its completion.
      Cannot be called from within the handlers.  *)
  val shutdown :
    _ t -> unit Lwt.t

  module type BOX = sig
    type t
    val put_request : t -> 'a Request.t -> unit
    val put_request_and_wait : t -> 'a Request.t -> 'a tzresult Lwt.t
  end
  module type QUEUE = sig
    type 'a t
    val push_request_and_wait : 'q t -> 'a Request.t -> 'a tzresult Lwt.t
    val push_request : 'q t -> 'a Request.t -> unit Lwt.t
    val pending_requests : 'a t -> (Time.System.t * Request.view) list
    val pending_requests_length : 'a t -> int
  end
  module type BOUNDED_QUEUE = sig
    type t
    val try_push_request_now : t -> 'a Request.t -> bool
  end

  module Dropbox : sig
    include BOX with type t := dropbox t
  end
  module Queue : sig
    include QUEUE with type 'a t := 'a queue t
    include BOUNDED_QUEUE with type t := bounded queue t

    (** Adds a message to the queue immediately. *)
    val push_request_now :
      infinite queue t -> 'a Request.t -> unit
  end


  (** Detects cancelation from within the request handler to stop
      asynchronous operations. *)
  val protect :
    _ t ->
    ?on_error: (error list -> 'b tzresult Lwt.t) ->
    (unit -> 'b tzresult Lwt.t) ->
    'b tzresult Lwt.t

  (** Exports the canceler to allow cancelation of other tasks when this
      worker is shutdowned or when it dies. *)
  val canceler : _ t -> Lwt_canceler.t

  (** Triggers a worker termination. *)
  val trigger_shutdown : _ t -> unit

  (** Recod an event in the backlog. *)
  val record_event : _ t -> Event.t -> unit

  (** Record an event and make sure it is logged. *)
  val log_event : _ t -> Event.t -> unit Lwt.t

  (** Access the internal state, once initialized. *)
  val state : _ t -> Types.state

  (** Access the event backlog. *)
  val last_events : _ t -> (Internal_event.level * Event.t list) list

  (** Introspect the message queue, gives the times requests were pushed. *)
  val pending_requests : _ queue t -> (Time.System.t * Request.view) list

  (** Get the running status of a worker. *)
  val status : _ t -> Worker_types.worker_status

  (** Get the request being treated by a worker.
      Gives the time the request was pushed, and the time its
      treatment started. *)
  val current_request : _ t -> (Time.System.t * Time.System.t * Request.view) option

  val information : _ t -> Worker_types.worker_information

  (** Introspect the state of a worker. *)
  val view : _ t -> Types.view

  (** Lists the running workers in this group. *)
  val list : 'a table -> (Name.t * 'a t) list

  (** [find_opt table n] is [Some worker] if the [worker] is in the [table] and
      has name [n]. *)
  val find_opt : 'a table -> Name.t -> 'a t option
end

module Make
    (Name : NAME)
    (Event : EVENT)
    (Request : REQUEST)
    (Types : TYPES) = struct

  module Name = Name
  module Event = Event
  module Request = Request
  module Types = Types

  let base_name = String.concat "." Name.base

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
    | Queue_buffer : (Time.System.t * message) Lwt_pipe.t -> infinite queue buffer
    | Bounded_buffer : (Time.System.t * message) Lwt_pipe.t -> bounded queue buffer
    | Dropbox_buffer : (Time.System.t * message) Lwt_dropbox.t -> dropbox buffer

  and 'kind t = {
    limits : Worker_types.limits ;
    timeout : Time.System.Span.t option ;
    parameters : Types.parameters ;
    mutable (* only for init *) worker : unit Lwt.t ;
    mutable (* only for init *) state : Types.state option ;
    buffer : 'kind buffer ;
    event_log : (Internal_event.level * Event.t Ring.t) list ;
    logger : (module Internal_event.Legacy_logging.LOG) ;
    canceler : Lwt_canceler.t ;
    name : Name.t ;
    id : int ;
    mutable status : Worker_types.worker_status ;
    mutable current_request : (Time.System.t * Time.System.t * Request.view) option ;
    table : 'kind table ;
  }
  and 'kind table = {
    buffer_kind : 'kind buffer_kind ;
    mutable last_id : int ;
    instances : (Name.t, 'kind t) Hashtbl.t ;
  }

  let queue_item ?u r =
    Systime_os.now (),
    Message (r, u)

  let drop_request w merge message_box request =
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
          Lwt_dropbox.put message_box (Systime_os.now (), Message (neu, None))
    with Lwt_dropbox.Closed -> ()

  let push_request_and_wait w message_queue request =
    let t, u = Lwt.wait () in
    Lwt.catch
      (fun () ->
         Lwt_pipe.push message_queue (queue_item ~u request) >>= fun () ->
         t)
      (function
        | Lwt_pipe.Closed ->
            let name = Format.asprintf "%a" Name.pp w.name in
            fail (Closed {base=base_name; name})
        | exn -> fail (Exn exn))

  let drop_request_and_wait w message_box request =
    let t, u = Lwt.wait () in
    Lwt.catch
      (fun () ->
         Lwt_dropbox.put message_box (queue_item ~u request);
         t)
      (function
        | Lwt_pipe.Closed ->
            let name = Format.asprintf "%a" Name.pp w.name in
            fail (Closed {base=base_name; name})
        | exn -> fail (Exn exn))

  module type BOX = sig
    type t
    val put_request : t -> 'a Request.t -> unit
    val put_request_and_wait : t -> 'a Request.t -> 'a tzresult Lwt.t
  end
  module type QUEUE = sig
    type 'a t
    val push_request_and_wait : 'q t -> 'a Request.t -> 'a tzresult Lwt.t
    val push_request : 'q t -> 'a Request.t -> unit Lwt.t
    val pending_requests : 'a t -> (Time.System.t * Request.view) list
    val pending_requests_length : 'a t -> int
  end
  module type BOUNDED_QUEUE = sig
    type t
    val try_push_request_now : t -> 'a Request.t -> bool
  end
  module Dropbox = struct

    let put_request (w : dropbox t) request =
      let Dropbox { merge } = w.table.buffer_kind in
      let Dropbox_buffer message_box = w.buffer in
      drop_request w merge message_box request

    let put_request_and_wait (w : dropbox t) request =
      let Dropbox_buffer message_box = w.buffer in
      drop_request_and_wait w message_box request

  end

  module Queue = struct

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
      push_request_and_wait w message_queue request

    let pending_requests (type a) (w : a queue t) =
      let message_queue = match w.buffer with
        | Queue_buffer message_queue -> message_queue
        | Bounded_buffer message_queue -> message_queue in
      List.map
        (function (t, Message (req, _)) -> t, Request.view req)
        (Lwt_pipe.peek_all message_queue)

    let pending_requests_length (type a) (w : a queue t) =
      let pipe_length (type a) (q : a buffer ) = match q with
        | Queue_buffer queue -> Lwt_pipe.length queue
        | Bounded_buffer queue -> Lwt_pipe.length queue
        | Dropbox_buffer _ -> 1
      in pipe_length w.buffer

  end

  let close (type a) (w : a t) =
    let wakeup = function
      | _, Message (_, Some u) ->
          let name = Format.asprintf "%a" Name.pp w.name in
          Lwt.wakeup_later u (Error [ Closed {base=base_name; name} ])
      | _, Message (_, None) -> () in
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
          return_some m
      | Some timeout ->
          Lwt_pipe.pop_with_timeout
            (Systime_os.sleep timeout) message_queue >>= fun m ->
          return m in
    match w.buffer with
    | Queue_buffer message_queue -> pop_queue message_queue
    | Bounded_buffer message_queue -> pop_queue message_queue
    | Dropbox_buffer message_box ->
        match w.timeout with
        | None ->
            Lwt_dropbox.take message_box >>= fun m ->
            return_some m
        | Some timeout ->
            Lwt_dropbox.take_with_timeout
              (Systime_os.sleep timeout) message_box >>= fun m ->
            return m

  let trigger_shutdown w =
    Lwt.ignore_result (Lwt_canceler.cancel w.canceler)

  let canceler { canceler ; _ } = canceler

  let log_event w evt =
    let (module Logger) = w.logger in
    let level = Event.level evt in
    let log =
      match level with
      | Debug -> Logger.lwt_debug
      | Info -> Logger.lwt_log_info
      | Notice -> Logger.lwt_log_notice
      | Warning -> Logger.lwt_warn
      | Error -> Logger.lwt_log_error
      | Fatal -> Logger.lwt_fatal_error in
    log "@[<v 0>%a@]" Event.pp evt >>= fun () ->
    if level >= w.limits.backlog_level then
      Ring.add (List.assoc level w.event_log) evt ;
    Lwt.return_unit

  let record_event w evt =
    Lwt.ignore_result (log_event w evt)

  module type HANDLERS = sig
    type self
    val on_launch :
      self -> Name.t -> Types.parameters -> Types.state tzresult Lwt.t
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
      instances = Hashtbl.create 10 ; }

  let worker_loop (type kind) handlers (w : kind t) =
    let (module Handlers : HANDLERS with type self = kind t) = handlers in
    let (module Logger) = w.logger in
    let do_close errs =
      let t0 = match w.status with
        | Running t0 -> t0
        | Launching _ | Closing _ | Closed _ -> assert false in
      w.status <- Closing (t0, Systime_os.now ()) ;
      close w ;
      Lwt_canceler.cancel w.canceler >>= fun () ->
      w.status <- Closed (t0, Systime_os.now (), errs) ;
      Hashtbl.remove w.table.instances w.name ;
      Handlers.on_close w >>= fun () ->
      w.state <- None ;
      Lwt.ignore_result
        ( List.iter (fun (_, ring) -> Ring.clear ring) w.event_log ;
          Lwt.return_unit) ;
      Lwt.return_unit in
    let rec loop () =
      begin
        protect ~canceler:w.canceler begin fun () ->
          pop w
        end >>=? function
        | None -> Handlers.on_no_request w
        | Some (pushed, Message (request, u)) ->
            let current_request = Request.view request in
            let treated = Systime_os.now () in
            w.current_request <- Some (pushed, treated, current_request) ;
            Logger.debug "@[<v 2>Request:@,%a@]"
              Request.pp current_request ;
            match u with
            | None ->
                Handlers.on_request w request >>=? fun res ->
                let completed = Systime_os.now () in
                w.current_request <- None ;
                Handlers.on_completion w
                  request res Worker_types.{ pushed ; treated ; completed } >>= fun () ->
                return_unit
            | Some u ->
                Handlers.on_request w request >>= fun res ->
                Lwt.wakeup_later u res ;
                Lwt.return res >>=? fun res ->
                let completed = Systime_os.now () in
                w.current_request <- None ;
                Handlers.on_completion w
                  request res Worker_types.{ pushed ; treated ; completed } >>= fun () ->
                return_unit
      end >>= function
      | Ok () ->
          loop ()
      | Error [Canceled | Exn Lwt.Canceled | Exn Lwt_pipe.Closed | Exn Lwt_dropbox.Closed ] ->
          Logger.lwt_log_notice
            "@[Worker terminated [%a] @]"
            Name.pp w.name  >>= fun () ->
          do_close None
      | Error errs ->
          begin match w.current_request with
            | Some (pushed, treated, request) ->
                let completed = Systime_os.now () in
                w.current_request <- None ;
                Handlers.on_error w
                  request Worker_types.{ pushed ; treated ; completed } errs
            | None -> assert false
          end >>= function
          | Ok () ->
              loop ()
          | Error ([Timeout] as errs) ->
              Logger.lwt_log_notice
                "@[Worker terminated with timeout [%a] @]"
                Name.pp w.name  >>= fun () ->
              do_close (Some errs)
          | Error errs ->
              Logger.lwt_log_error
                "@[<v 0>Worker crashed [%a]:@,%a@]"
                Name.pp w.name
                (Format.pp_print_list Error_monad.pp) errs >>= fun () ->
              do_close (Some errs) in
    loop ()

  let launch
    : type kind.
      kind table -> ?timeout:Time.System.Span.t ->
      Worker_types.limits -> Name.t -> Types.parameters ->
      (module HANDLERS with type self = kind t) ->
      kind t tzresult Lwt.t
    = fun table ?timeout limits name parameters (module Handlers) ->
      let name_s =
        Format.asprintf "%a" Name.pp name in
      let full_name =
        if name_s = "" then base_name else Format.asprintf "%s_%s" base_name name_s in
      let id =
        table.last_id <- table.last_id + 1 ;
        table.last_id in
      let id_name =
        if name_s = "" then base_name else Format.asprintf "%s_%d" base_name id in
      if Hashtbl.mem table.instances name then
        invalid_arg (Format.asprintf "Worker.launch: duplicate worker %s" full_name) ;
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
          Internal_event.[
            Debug ; Info ; Notice ; Warning ; Error ; Fatal
          ] in
        List.map (fun l -> l, Ring.create limits.backlog_size) levels in
      let module Logger =
        Internal_event.Legacy_logging.Make(struct
          let name = id_name
        end) in
      let w = { limits ; parameters ; name ; canceler ;
                table ; buffer ; logger = (module Logger) ;
                state = None ; id ;
                worker = Lwt.return_unit ;
                event_log ; timeout ;
                current_request = None ;
                status = Launching (Systime_os.now ())} in
      Hashtbl.add table.instances name w ;
      begin
        if id_name = base_name then
          Logger.lwt_log_notice "Worker started"
        else
          Logger.lwt_log_notice "Worker started for %s" name_s
      end >>= fun () ->
      Handlers.on_launch w name parameters >>=? fun state ->
      w.status <- Running (Systime_os.now ()) ;
      w.state <- Some state ;
      w.worker <-
        Lwt_utils.worker
          full_name
          ~on_event:Internal_event.Lwt_worker_event.on_event
          ~run:(fun () -> worker_loop (module Handlers) w)
          ~cancel:(fun () -> Lwt_canceler.cancel w.canceler) ;
      return w

  let shutdown w =
    let (module Logger) = w.logger in
    Logger.lwt_debug "Triggering shutdown" >>= fun () ->
    Lwt_canceler.cancel w.canceler >>= fun () ->
    w.worker

  let state w =
    match w.state, w.status with
    | None, Launching _  ->
        invalid_arg
          (Format.asprintf
             "Worker.state (%s[%a]): \
              state called before worker was initialized"
             base_name Name.pp w.name)
    | None, (Closing _ | Closed _)  ->
        invalid_arg
          (Format.asprintf
             "Worker.state (%s[%a]): \
              state called after worker was terminated"
             base_name Name.pp w.name)
    | None, _  -> assert false
    | Some state, _ -> state

  let pending_requests q =
    Queue.pending_requests q

  let last_events w =
    List.map
      (fun (level, ring) -> (level, Ring.elements ring))
      w.event_log

  let status { status ; _ } = status

  let current_request { current_request ; _ } = current_request

  let information (type a) (w : a t) =
    { Worker_types.instances_number = Hashtbl.length w.table.instances ;
      wstatus = w.status ;
      queue_length = match w.buffer with
        | Queue_buffer pipe ->  Lwt_pipe.length pipe
        | Bounded_buffer pipe ->  Lwt_pipe.length pipe
        | Dropbox_buffer _ -> 1
    }

  let view w =
    Types.view (state w) w.parameters

  let list { instances ; _ } =
    Hashtbl.fold
      (fun n w acc -> (n, w) :: acc)
      instances []

  let find_opt { instances ; _ } =
    Hashtbl.find_opt instances

  let protect { canceler ; _ } ?on_error f =
    protect ?on_error ~canceler f

end
