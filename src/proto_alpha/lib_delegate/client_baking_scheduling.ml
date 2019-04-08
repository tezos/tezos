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

include Internal_event.Legacy_logging.Make_semantic (struct
    let name =  Proto_alpha.Name.name ^ ".client.scheduling"
  end)

open Logging

let sleep_until time =
  (* Sleeping is a system op, baking is a protocol op, this is where we convert *)
  let time = Time.System.of_protocol_exn time in
  let delay = Ptime.diff time (Tezos_stdlib_unix.Systime_os.now ()) in
  if Ptime.Span.compare delay Ptime.Span.zero < 0 then
    None
  else
    Some (Lwt_unix.sleep (Ptime.Span.to_float_s delay))

let rec wait_for_first_event ~name stream =
  Lwt_stream.get stream >>= function
  | None | Some (Error _) ->
      lwt_log_info Tag.DSL.(fun f ->
          f "Can't fetch the current event. Waiting for new event."
          -% t event "cannot_fetch_event"
          -% t worker_tag name) >>= fun () ->
      (* NOTE: this is not a tight loop because of Lwt_stream.get *)
      wait_for_first_event ~name stream
  | Some (Ok bi) ->
      Lwt.return bi

let log_errors_and_continue ~name p =
  p >>= function
  | Ok () -> Lwt.return_unit
  | Error errs -> lwt_log_error Tag.DSL.(fun f ->
      f "Error while baking:@\n%a"
      -% t event "daemon_error"
      -% t worker_tag name
      -% a errs_tag errs)

let main
    ~(name: string)
    ~(cctxt: #Proto_alpha.full)
    ~(stream: 'event tzresult Lwt_stream.t)
    ~(state_maker: ('event ->
                    'state tzresult Lwt.t))
    ~(pre_loop: (#Proto_alpha.full ->
                 'state ->
                 'event ->
                 unit tzresult Lwt.t))
    ~(compute_timeout: ('state -> 'timesup Lwt.t))
    ~(timeout_k: (#Proto_alpha.full ->
                  'state ->
                  'timesup ->
                  unit tzresult Lwt.t))
    ~(event_k: (#Proto_alpha.full ->
                'state ->
                'event ->
                unit tzresult Lwt.t))
  =

  lwt_log_info Tag.DSL.(fun f ->
      f "Setting up before the %s can start."
      -% t event "daemon_setup"
      -% s worker_tag name) >>= fun () ->

  wait_for_first_event ~name stream >>= fun first_event ->

  (* statefulness *)
  let last_get_event = ref None in
  let get_event () =
    match !last_get_event with
    | None ->
        let t = Lwt_stream.get stream in
        last_get_event := Some t ;
        t
    | Some t -> t in
  state_maker first_event >>=? fun state ->

  log_errors_and_continue ~name @@ pre_loop cctxt state first_event >>= fun () ->

  (* main loop *)
  let rec worker_loop () =
    begin
      (* event construction *)
      let timeout = compute_timeout state in
      Lwt.choose [ (Lwt_exit.termination_thread >|= fun _ -> `Termination) ;
                   (timeout >|= fun timesup -> `Timeout timesup) ;
                   (get_event () >|= fun e -> `Event e) ;
                 ] >>= function
        (* event matching *)
      | `Termination ->
          return_unit
      | `Event (None | Some (Error _)) ->
          (* exit when the node is unavailable *)
          last_get_event := None ;
          lwt_log_error Tag.DSL.(fun f ->
              f "Connection to node lost, %s exiting."
              -% t event "daemon_connection_lost"
              -% s worker_tag name) >>= fun () ->
          return_unit
      | `Event (Some (Ok event)) -> begin
          (* new event: cancel everything and execute callback *)
          last_get_event := None ;
          (* TODO: pretty-print events (requires passing a pp as argument) *)
          log_errors_and_continue ~name @@ event_k cctxt state event >>= fun () ->
          worker_loop ()
        end
      | `Timeout timesup ->
          (* main event: it's time *)
          lwt_debug Tag.DSL.(fun f ->
              f "Waking up for %s."
              -% t event "daemon_wakeup"
              -% s worker_tag name) >>= fun () ->
          (* core functionality *)
          log_errors_and_continue ~name @@ timeout_k cctxt state timesup >>= fun () ->
          worker_loop ()
    end in

  (* ignition *)
  lwt_log_info Tag.DSL.(fun f ->
      f "Starting %s daemon"
      -% t event "daemon_start"
      -% s worker_tag name) >>= fun () ->
  worker_loop ()
