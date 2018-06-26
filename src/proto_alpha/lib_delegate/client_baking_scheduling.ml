(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Logging.Make(struct let name = "client.scheduling" end)

let sleep_until time =
  let delay = Time.diff time (Time.now ()) in
  if delay < 0L then
    None
  else
    Some (Lwt_unix.sleep (Int64.to_float delay))

let rec wait_for_first_event stream =
  Lwt_stream.get stream >>= function
  | None | Some (Error _) ->
      lwt_log_info "Can't fetch the current event. Waiting for new event." >>= fun () ->
      (* NOTE: this is not a tight loop because of Lwt_stream.get *)
      wait_for_first_event stream
  | Some (Ok bi) ->
      Lwt.return bi

let log_errors_and_continue p =
  p >>= function
  | Ok () -> Lwt.return_unit
  | Error errs -> lwt_log_error "Error while baking:@\n%a" pp_print_error errs

let main
    ~(name: string)
    ~(cctxt: #Proto_alpha.full)
    ~(stream: 'event tzresult Lwt_stream.t)
    ~(state_maker: (Block_hash.t ->
                    'event ->
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

  lwt_log_info "Setting up before the %s can start." name >>= fun () ->

  wait_for_first_event stream >>= fun first_event ->
  Shell_services.Blocks.hash cctxt ~block:`Genesis () >>=? fun genesis_hash ->

  (* statefulness *)
  let last_get_event = ref None in
  let get_event () =
    match !last_get_event with
    | None ->
        let t = Lwt_stream.get stream in
        last_get_event := Some t ;
        t
    | Some t -> t in
  state_maker genesis_hash first_event >>=? fun state ->

  log_errors_and_continue @@ pre_loop cctxt state first_event >>= fun () ->

  (* main loop *)
  let rec worker_loop () =
    begin
      (* event construction *)
      let timeout = compute_timeout state in
      Lwt.choose [ (timeout >|= fun timesup -> `Timeout timesup) ;
                   (get_event () >|= fun e -> `Event e) ;
                 ] >>= function
        (* event matching *)
      | `Event (None | Some (Error _)) ->
          (* exit when the node is unavailable *)
          last_get_event := None ;
          lwt_log_error "Connection to node lost, %s exiting." name >>= fun () ->
          exit 1
      | `Event (Some (Ok event)) -> begin
          (* new event: cancel everything and execute callback *)
          last_get_event := None ;
          (* TODO: pretty-print events (requires passing a pp as argument) *)
          log_errors_and_continue @@ event_k cctxt state event
        end
      | `Timeout timesup ->
          (* main event: it's time *)
          lwt_debug "Waking up for %s." name >>= fun () ->
          (* core functionality *)
          log_errors_and_continue @@ timeout_k cctxt state timesup
    end >>= fun () ->
    (* and restart *)
    worker_loop () in

  (* ignition *)
  lwt_log_info "Starting %s daemon" name >>= fun () ->
  worker_loop ()
