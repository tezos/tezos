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

(* TODO decide whether we need to preallocate buffers or not. *)

include Internal_event.Legacy_logging.Make (struct
    let name = "p2p.io-scheduler"
  end)

let alpha = 0.2

module type IO = sig
  val name: string
  type in_param
  val pop: in_param -> MBytes.t tzresult Lwt.t
  type out_param
  val push: out_param -> MBytes.t -> unit tzresult Lwt.t
  val close: out_param -> error list -> unit Lwt.t
end

module Scheduler(IO : IO) = struct

  [@@@ocaml.warning "-30"]

  type t = {
    canceler: Lwt_canceler.t ;
    mutable worker: unit Lwt.t ;
    counter: Moving_average.t ;
    max_speed: int option ;
    mutable quota: int ;
    quota_updated: unit Lwt_condition.t ;
    readys: unit Lwt_condition.t ;
    readys_high: (connection * MBytes.t tzresult) Queue.t ;
    readys_low: (connection * MBytes.t tzresult) Queue.t ;
  }

  and connection = {
    id: int ;
    mutable closed: bool ;
    canceler: Lwt_canceler.t ;
    in_param: IO.in_param ;
    out_param: IO.out_param ;
    mutable current_pop: MBytes.t tzresult Lwt.t ;
    mutable current_push: unit tzresult Lwt.t ;
    counter: Moving_average.t ;
    mutable quota: int ;
    mutable last_quota: int ;
  }

  let cancel (conn : connection) err =
    Lwt_utils.unless conn.closed begin fun () ->
      lwt_debug "Connection closed (%d, %s) " conn.id IO.name >>= fun () ->
      conn.closed <- true ;
      Lwt.catch
        (fun () -> IO.close conn.out_param err)
        (fun _ -> Lwt.return_unit) >>= fun () ->
      Lwt_canceler.cancel conn.canceler
    end

  let waiter st conn =
    assert (Lwt.state conn.current_pop <> Sleep) ;
    conn.current_pop <- IO.pop conn.in_param ;
    Lwt.async begin fun () ->
      conn.current_pop >>= fun res ->
      conn.current_push >>= fun _ ->
      let was_empty =
        Queue.is_empty st.readys_high && Queue.is_empty st.readys_low in
      if conn.quota > 0 then
        Queue.push (conn, res) st.readys_high
      else
        Queue.push (conn, res) st.readys_low ;
      if was_empty then Lwt_condition.broadcast st.readys () ;
      Lwt.return_unit
    end

  let wait_data st =
    let is_empty =
      Queue.is_empty st.readys_high && Queue.is_empty st.readys_low in
    if is_empty then Lwt_condition.wait st.readys else Lwt.return_unit

  let check_quota st =
    if st.max_speed <> None && st.quota < 0 then begin
      lwt_debug "scheduler.wait_quota(%s)" IO.name >>= fun () ->
      Lwt_condition.wait st.quota_updated
    end else
      Lwt_unix.yield ()

  let rec worker_loop st =
    check_quota st >>= fun () ->
    lwt_debug "scheduler.wait(%s)" IO.name >>= fun () ->
    Lwt.pick [
      Lwt_canceler.cancelation st.canceler ;
      wait_data st
    ] >>= fun () ->
    if Lwt_canceler.canceled st.canceler then
      Lwt.return_unit
    else
      let prio, (conn, msg) =
        if not (Queue.is_empty st.readys_high) then
          true, (Queue.pop st.readys_high)
        else
          false, (Queue.pop st.readys_low)
      in
      match msg with
      | Error [ Canceled ] ->
          worker_loop st
      | Error ([P2p_errors.Connection_closed |
                Exn ( Lwt_pipe.Closed |
                      Unix.Unix_error ((EBADF | ETIMEDOUT), _, _) )]
               as err) ->
          lwt_debug "Connection closed (pop: %d, %s)"
            conn.id IO.name >>= fun () ->
          cancel conn err >>= fun () ->
          worker_loop st
      | Error err ->
          lwt_log_error
            "@[Unexpected error in connection (pop: %d, %s):@ %a@]"
            conn.id IO.name pp_print_error err >>= fun () ->
          cancel conn err >>= fun () ->
          worker_loop st
      | Ok msg ->
          conn.current_push <- begin
            IO.push conn.out_param msg >>= function
            | Ok ()
            | Error [ Canceled ] ->
                return_unit
            | Error ([P2p_errors.Connection_closed |
                      Exn (Unix.Unix_error (EBADF, _, _) |
                           Lwt_pipe.Closed)] as err) ->
                lwt_debug "Connection closed (push: %d, %s)"
                  conn.id IO.name >>= fun () ->
                cancel conn err >>= fun () ->
                return_unit
            | Error err ->
                lwt_log_error
                  "@[Unexpected error in connection (push: %d, %s):@ %a@]"
                  conn.id IO.name pp_print_error err >>= fun () ->
                cancel conn err >>= fun () ->
                Lwt.return_error err
          end ;
          let len = MBytes.length msg in
          lwt_debug "Handle: %d (%d, %s)" len conn.id IO.name >>= fun () ->
          Moving_average.add st.counter len ;
          st.quota <- st.quota - len ;
          Moving_average.add conn.counter len ;
          if prio then conn.quota <- conn.quota - len ;
          waiter st conn ;
          worker_loop st

  let create max_speed =
    let st = {
      canceler = Lwt_canceler.create () ;
      worker = Lwt.return_unit ;
      counter = Moving_average.create ~init:0 ~alpha ;
      max_speed ; quota = Option.unopt ~default:0 max_speed ;
      quota_updated = Lwt_condition.create () ;
      readys = Lwt_condition.create () ;
      readys_high = Queue.create () ;
      readys_low = Queue.create () ;
    } in
    st.worker <-
      Lwt_utils.worker IO.name
        ~on_event:Internal_event.Lwt_worker_event.on_event
        ~run:(fun () -> worker_loop st)
        ~cancel:(fun () -> Lwt_canceler.cancel st.canceler) ;
    st

  let create_connection st in_param out_param canceler id =
    debug "scheduler(%s).create_connection (%d)" IO.name id ;
    let conn =
      { id ; closed = false ;
        canceler ;
        in_param ; out_param ;
        current_pop = Lwt.fail Not_found (* dummy *) ;
        current_push = return_unit ;
        counter = Moving_average.create ~init:0 ~alpha ;
        quota = 0 ; last_quota = 0 ;
      } in
    waiter st conn ;
    conn

  let update_quota st =
    debug "scheduler(%s).update_quota" IO.name ;
    Option.iter st.max_speed ~f:begin fun quota ->
      st.quota <- (min st.quota 0) + quota ;
      Lwt_condition.broadcast st.quota_updated ()
    end ;
    if not (Queue.is_empty st.readys_low) then begin
      let tmp = Queue.create () in
      Queue.iter
        (fun ((conn : connection), _ as msg) ->
           if conn.quota > 0 then
             Queue.push msg st.readys_high
           else
             Queue.push msg tmp)
        st.readys_low ;
      Queue.clear st.readys_low ;
      Queue.transfer tmp st.readys_low ;
    end

  let shutdown st =
    lwt_debug "--> scheduler(%s).shutdown" IO.name >>= fun () ->
    Lwt_canceler.cancel st.canceler >>= fun () ->
    st.worker >>= fun () ->
    lwt_debug "<-- scheduler(%s).shutdown" IO.name >>= fun () ->
    Lwt.return_unit


end

module ReadScheduler = Scheduler(struct
    let name = "io_scheduler(read)"
    type in_param = P2p_fd.t * int
    let pop (fd, maxlen) =
      Lwt.catch
        (fun () ->
           let buf = MBytes.create maxlen in
           P2p_fd.read fd buf 0 maxlen >>= fun len ->
           if len = 0 then
             fail P2p_errors.Connection_closed
           else
             return (MBytes.sub buf 0 len) )
        (function
          | Unix.Unix_error(Unix.ECONNRESET, _, _) ->
              fail P2p_errors.Connection_closed
          | exn ->
              Lwt.return (error_exn exn))
    type out_param = MBytes.t tzresult Lwt_pipe.t
    let push p msg =
      Lwt.catch
        (fun () -> Lwt_pipe.push p (Ok msg) >>= return)
        (fun exn -> fail (Exn exn))
    let close p err =
      Lwt.catch
        (fun () -> Lwt_pipe.push p (Error err))
        (fun _ -> Lwt.return_unit)
  end)

module WriteScheduler = Scheduler(struct
    let name = "io_scheduler(write)"
    type in_param = MBytes.t Lwt_pipe.t
    let pop p =
      Lwt.catch
        (fun () -> Lwt_pipe.pop p >>= return)
        (fun _ -> fail (Exn Lwt_pipe.Closed))
    type out_param = P2p_fd.t
    let push fd buf =
      Lwt.catch
        (fun () ->
           P2p_fd.write fd buf >>= return)
        (function
          | Unix.Unix_error(Unix.ECONNRESET, _, _)
          | Unix.Unix_error(Unix.EPIPE, _, _)
          | Lwt.Canceled
          | End_of_file ->
              fail P2p_errors.Connection_closed
          | exn ->
              Lwt.return (error_exn exn))
    let close _p _err = Lwt.return_unit
  end)

type connection = {
  sched: t ;
  conn: P2p_fd.t;
  canceler: Lwt_canceler.t ;
  read_conn: ReadScheduler.connection ;
  read_queue: MBytes.t tzresult Lwt_pipe.t ;
  write_conn: WriteScheduler.connection ;
  write_queue: MBytes.t Lwt_pipe.t ;
  mutable partial_read: MBytes.t option ;
}

and t = {
  mutable closed: bool ;
  connected: connection P2p_fd.Table.t ;
  read_scheduler: ReadScheduler.t ;
  write_scheduler: WriteScheduler.t ;
  max_upload_speed: int option ; (* bytes per second. *)
  max_download_speed: int option ;
  read_buffer_size: int ;
  read_queue_size: int option ;
  write_queue_size: int option ;
}

let reset_quota st =
  debug "--> reset quota" ;
  let { Moving_average.average = current_inflow ; _ } =
    Moving_average.stat st.read_scheduler.counter
  and { Moving_average.average = current_outflow ; _ } =
    Moving_average.stat st.write_scheduler.counter in
  let nb_conn = P2p_fd.Table.length st.connected in
  if nb_conn > 0 then begin
    let fair_read_quota = current_inflow / nb_conn
    and fair_write_quota = current_outflow / nb_conn in
    P2p_fd.Table.iter
      (fun _id conn ->
         conn.read_conn.last_quota <- fair_read_quota ;
         conn.read_conn.quota <-
           (min conn.read_conn.quota 0) + fair_read_quota ;
         conn.write_conn.last_quota <- fair_write_quota ;
         conn.write_conn.quota <-
           (min conn.write_conn.quota 0) + fair_write_quota ; )
      st.connected
  end ;
  ReadScheduler.update_quota st.read_scheduler ;
  WriteScheduler.update_quota st.write_scheduler

let create
    ?max_upload_speed ?max_download_speed
    ?read_queue_size ?write_queue_size
    ~read_buffer_size
    () =
  log_info "--> create" ;
  let st = {
    closed = false ;
    connected = P2p_fd.Table.create 53 ;
    read_scheduler = ReadScheduler.create max_download_speed ;
    write_scheduler = WriteScheduler.create max_upload_speed ;
    max_upload_speed ;
    max_download_speed ;
    read_buffer_size ;
    read_queue_size ;
    write_queue_size ;
  } in
  Moving_average.on_update (fun () -> reset_quota st) ;
  st

exception Closed

let read_size = function
  | Ok buf -> (Sys.word_size / 8) * 8 + MBytes.length buf + Lwt_pipe.push_overhead
  | Error _ -> 0 (* we push Error only when we close the socket,
                    we don't fear memory leaks in that case... *)

let write_size mbytes =
  (Sys.word_size / 8) * 6 + MBytes.length mbytes + Lwt_pipe.push_overhead

let register st conn =
  if st.closed then begin
    Lwt.async (fun () -> P2p_fd.close conn) ;
    raise Closed
  end else begin
    let id = P2p_fd.id conn in
    let canceler = Lwt_canceler.create () in
    let read_size =
      Option.map st.read_queue_size ~f:(fun v -> v, read_size) in
    let write_size =
      Option.map st.write_queue_size ~f:(fun v -> v, write_size) in
    let read_queue = Lwt_pipe.create ?size:read_size () in
    let write_queue = Lwt_pipe.create ?size:write_size () in
    let read_conn =
      ReadScheduler.create_connection
        st.read_scheduler (conn, st.read_buffer_size) read_queue canceler id
    and write_conn =
      WriteScheduler.create_connection
        st.write_scheduler write_queue conn canceler id in
    Lwt_canceler.on_cancel canceler begin fun () ->
      P2p_fd.Table.remove st.connected conn ;
      Moving_average.destroy read_conn.counter ;
      Moving_average.destroy write_conn.counter ;
      Lwt_pipe.close write_queue ;
      Lwt_pipe.close read_queue ;
      P2p_fd.close conn
    end ;
    let conn = {
      sched = st ; conn ; canceler ;
      read_queue ; read_conn ;
      write_queue ; write_conn ;
      partial_read = None ;
    } in
    P2p_fd.Table.add st.connected conn.conn conn ;
    log_info "--> register (%d)" id ;
    conn
  end

let write ?canceler { write_queue ; _ } msg =
  trace P2p_errors.Connection_closed @@
  protect ?canceler begin fun () ->
    Lwt_pipe.push write_queue msg >>= return
  end
let write_now { write_queue ; _ } msg = Lwt_pipe.push_now write_queue msg

let read_from conn ?pos ?len buf msg =
  let maxlen = MBytes.length buf in
  let pos = Option.unopt ~default:0 pos in
  assert (0 <= pos && pos < maxlen) ;
  let len = Option.unopt ~default:(maxlen - pos) len in
  assert (len <= maxlen - pos) ;
  match msg with
  | Ok msg ->
      let msg_len = MBytes.length msg in
      let read_len = min len msg_len in
      MBytes.blit msg 0 buf pos read_len ;
      if read_len < msg_len then
        conn.partial_read <-
          Some (MBytes.sub msg read_len (msg_len - read_len)) ;
      Ok read_len
  | Error _ ->
      Error [P2p_errors.Connection_closed]

let read_now conn ?pos ?len buf =
  match conn.partial_read with
  | Some msg ->
      conn.partial_read <- None ;
      Some (read_from conn ?pos ?len buf (Ok msg))
  | None ->
      try
        Option.map
          ~f:(read_from conn ?pos ?len buf)
          (Lwt_pipe.pop_now conn.read_queue)
      with Lwt_pipe.Closed -> Some (Error [P2p_errors.Connection_closed])

let read ?canceler conn ?pos ?len buf =
  match conn.partial_read with
  | Some msg ->
      conn.partial_read <- None ;
      Lwt.return (read_from conn ?pos ?len buf (Ok msg))
  | None ->
      Lwt.catch
        (fun () ->
           protect ?canceler begin fun () ->
             Lwt_pipe.pop conn.read_queue
           end >|= fun msg ->
           read_from conn ?pos ?len buf msg)
        (fun _ -> fail P2p_errors.Connection_closed)

let read_full ?canceler conn ?pos ?len buf =
  let maxlen = MBytes.length buf in
  let pos = Option.unopt ~default:0 pos in
  let len = Option.unopt ~default:(maxlen - pos) len in
  assert (0 <= pos && pos < maxlen) ;
  assert (len <= maxlen - pos) ;
  let rec loop pos len =
    if len = 0 then
      return_unit
    else
      read ?canceler conn ~pos ~len buf >>=? fun read_len ->
      loop (pos + read_len) (len - read_len) in
  loop pos len

let convert ~ws ~rs =
  { P2p_stat.total_sent = ws.Moving_average.total ;
    total_recv = rs.Moving_average.total ;
    current_outflow = ws.average ;
    current_inflow = rs.average ;
  }

let global_stat { read_scheduler ; write_scheduler ; _ } =
  let rs = Moving_average.stat read_scheduler.counter
  and ws = Moving_average.stat write_scheduler.counter in
  convert ~rs ~ws

let stat { read_conn ; write_conn ; _ } =
  let rs = Moving_average.stat read_conn.counter
  and ws = Moving_average.stat write_conn.counter in
  convert ~rs ~ws

let close ?timeout conn =
  let id = P2p_fd.id conn.conn in
  lwt_log_info "--> close (%d)" id >>= fun () ->
  P2p_fd.Table.remove conn.sched.connected conn.conn ;
  Lwt_pipe.close conn.write_queue ;
  begin
    match timeout with
    | None ->
        return (Lwt_canceler.cancelation conn.canceler)
    | Some timeout ->
        with_timeout
          ~canceler:conn.canceler
          (Lwt_unix.sleep timeout)
          (fun canceler -> return (Lwt_canceler.cancelation canceler))
  end >>=? fun _ ->
  conn.write_conn.current_push >>= fun res ->
  lwt_log_info "<-- close (%d)" id >>= fun () ->
  Lwt.return res

let iter_connection { connected ; _ } f =
  P2p_fd.Table.iter (fun _ conn -> f conn) connected

let shutdown ?timeout st =
  lwt_log_info "--> shutdown" >>= fun () ->
  st.closed <- true ;
  ReadScheduler.shutdown st.read_scheduler >>= fun () ->
  P2p_fd.Table.fold
    (fun _peer_id conn acc -> close ?timeout conn >>= fun _ -> acc)
    st.connected
    Lwt.return_unit >>= fun () ->
  WriteScheduler.shutdown st.write_scheduler >>= fun () ->
  lwt_log_info "<-- shutdown" >>= fun () ->
  Lwt.return_unit

let id conn = P2p_fd.id conn.conn
