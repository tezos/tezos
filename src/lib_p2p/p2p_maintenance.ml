(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

include Internal_event.Legacy_logging.Make (struct let name = "p2p.maintenance" end)

type bounds = {
  min_threshold: int ;
  min_target: int ;
  max_target: int ;
  max_threshold: int ;
}

type config = {
  maintenance_idle_time: Time.System.Span.t ;
  greylist_timeout: Time.System.Span.t ;
  private_mode: bool ;
}

type 'meta pool = Pool : ('msg, 'meta, 'meta_conn) P2p_pool.t -> 'meta pool

type 'meta t = {
  canceler: Lwt_canceler.t ;
  config: config ;
  bounds: bounds ;
  pool: 'meta pool ;
  discovery: P2p_discovery.t option ;
  just_maintained: unit Lwt_condition.t ;
  please_maintain: unit Lwt_condition.t ;
  mutable maintain_worker: unit Lwt.t ;
}

(** Select [expected] points among the disconnected known points.
    It ignores points which are greylisted, or for which a connection
    failed after [start_time] and the pointes that are banned. It
    first selects points with the oldest last tentative.
    Non-trusted points are also ignored if option --private-mode is set. *)
let connectable st start_time expected seen_points =
  let Pool pool = st.pool in
  let now = Systime_os.now () in
  let module Bounded_point_info =
    List.Bounded(struct
      type t = (Time.System.t option * P2p_point.Id.t)
      let compare (t1, _) (t2, _) =
        match t1, t2 with
        | None, None -> 0
        | None, Some _ -> 1
        | Some _, None -> -1
        | Some t1, Some t2 -> Time.System.compare t2 t1
    end) in
  let acc = Bounded_point_info.create expected in
  let seen_points =
    P2p_pool.Points.fold_known pool ~init:seen_points
      ~f:begin fun point pi seen_points ->
        (* consider the point only if:
           - it is not in seen_points and
           - it is not banned, and
           - it is trusted if we are in `closed` mode
        *)
        if P2p_point.Set.mem point seen_points ||
           P2p_pool.Points.banned pool point ||
           (st.config.private_mode && not (P2p_point_state.Info.trusted pi))
        then
          seen_points
        else
          let seen_points = P2p_point.Set.add point seen_points in
          match P2p_point_state.get pi with
          | Disconnected -> begin
              match P2p_point_state.Info.last_miss pi with
              | Some last when Time.System.(start_time < last)
                            || P2p_point_state.Info.greylisted ~now pi ->
                  seen_points
              | last ->
                  Bounded_point_info.insert (last, point) acc ;
                  seen_points
            end
          | _ -> seen_points
      end
  in
  List.map snd (Bounded_point_info.get acc), seen_points

(** Try to create connections to new peers. It tries to create at
    least [min_to_contact] connections, and will never creates more
    than [max_to_contact]. But, if after trying once all disconnected
    peers, it returns [false]. *)
let rec try_to_contact
    st ?(start_time = Systime_os.now ()) ~seen_points
    min_to_contact max_to_contact =
  let Pool pool = st.pool in
  if min_to_contact <= 0 then
    Lwt.return_true
  else
    let contactable, seen_points =
      connectable st start_time max_to_contact seen_points in
    if contactable = [] then
      Lwt_unix.yield () >>= fun () ->
      Lwt.return_false
    else
      List.fold_left
        (fun acc point ->
           protect ~canceler:st.canceler begin fun () ->
             P2p_pool.connect pool point
           end >>= function
           | Ok _ -> acc >|= succ
           | Error _ -> acc)
        (Lwt.return 0)
        contactable >>= fun established ->
      try_to_contact st ~start_time ~seen_points
        (min_to_contact - established) (max_to_contact - established)

(** Do a maintenance step. It will terminate only when the number
    of connections is between `min_threshold` and `max_threshold`.
    Do a pass in the list of banned peers and remove all peers that
    have been banned for more then xxx seconds *)
let rec maintain st =
  let Pool pool = st.pool in
  let n_connected = P2p_pool.active_connections pool in
  let older_than =
    Option.unopt_exn
      (Failure "P2p_maintenance.maintain: time overflow")
      (Ptime.add_span (Systime_os.now ()) (Ptime.Span.neg st.config.greylist_timeout))
  in
  P2p_pool.gc_greylist pool ~older_than ;
  if n_connected < st.bounds.min_threshold then
    too_few_connections st n_connected
  else if st.bounds.max_threshold < n_connected then
    too_many_connections st n_connected
  else begin
    (* end of maintenance when enough users have been reached *)
    Lwt_condition.broadcast st.just_maintained () ;
    lwt_debug "Maintenance step ended" >>= fun () ->
    return_unit
  end

and too_few_connections st n_connected =
  let Pool pool = st.pool in
  (* too few connections, try and contact many peers *)
  lwt_log_notice "Too few connections (%d)" n_connected >>= fun () ->
  let min_to_contact = st.bounds.min_target - n_connected in
  let max_to_contact = st.bounds.max_target - n_connected in
  try_to_contact
    st min_to_contact max_to_contact ~seen_points:P2p_point.Set.empty >>=
  fun success ->
  if success then begin
    maintain st
  end else begin
    (* not enough contacts, ask the pals of our pals,
       discover the local network and then wait *)
    P2p_pool.broadcast_bootstrap_msg pool ;
    Option.iter ~f:P2p_discovery.wakeup st.discovery ;
    protect ~canceler:st.canceler begin fun () ->
      Lwt.pick [
        P2p_pool.Pool_event.wait_new_peer pool ;
        P2p_pool.Pool_event.wait_new_point pool ;
        Lwt_unix.sleep 5.0 (* TODO exponential back-off ??
                                   or wait for the existence of a
                                   non grey-listed peer ?? *)
      ] >>= return
    end >>=? fun () ->
    maintain st
  end

and too_many_connections st n_connected =
  let Pool pool = st.pool in
  (* too many connections, start the russian roulette *)
  let to_kill = n_connected - st.bounds.max_target in
  lwt_log_notice "Too many connections, will kill %d" to_kill >>= fun () ->
  snd @@ P2p_pool.Connection.fold pool
    ~init:(to_kill, Lwt.return_unit)
    ~f:(fun _ conn (i, t) ->
        if i = 0 then (0, t)
        else if (P2p_pool.Connection.private_node conn
                 && P2p_pool.Connection.trusted_node conn) then
          (i, t)
        else
          (i - 1, t >>= fun () -> P2p_pool.disconnect conn))
  >>= fun () ->
  maintain st

let rec worker_loop st =
  let Pool pool = st.pool in
  begin
    protect ~canceler:st.canceler begin fun () ->
      Lwt.pick [
        Systime_os.sleep st.config.maintenance_idle_time ; (* default: every two minutes *)
        Lwt_condition.wait st.please_maintain ; (* when asked *)
        P2p_pool.Pool_event.wait_too_few_connections pool ; (* limits *)
        P2p_pool.Pool_event.wait_too_many_connections pool ;
      ] >>= fun () ->
      return_unit
    end >>=? fun () ->
    let n_connected = P2p_pool.active_connections pool in
    if n_connected < st.bounds.min_threshold
    || st.bounds.max_threshold < n_connected then
      maintain st
    else begin
      P2p_pool.send_swap_request pool ;
      return_unit
    end
  end >>= function
  | Ok () -> worker_loop st
  | Error [ Canceled ] -> Lwt.return_unit
  | Error _ -> Lwt.return_unit

let create ?discovery config bounds pool = {
  canceler = Lwt_canceler.create () ;
  config ;
  bounds ;
  discovery ;
  pool = Pool pool ;
  just_maintained = Lwt_condition.create () ;
  please_maintain = Lwt_condition.create () ;
  maintain_worker = Lwt.return_unit ;
}

let activate st =
  st.maintain_worker <-
    Lwt_utils.worker "maintenance"
      ~on_event:Internal_event.Lwt_worker_event.on_event
      ~run:(fun () -> worker_loop st)
      ~cancel:(fun () -> Lwt_canceler.cancel st.canceler) ;
  Option.iter st.discovery ~f:P2p_discovery.activate

let maintain { just_maintained ; please_maintain ; _ } =
  let wait = Lwt_condition.wait just_maintained in
  Lwt_condition.broadcast please_maintain () ;
  wait

let shutdown {
    canceler ;
    discovery ;
    maintain_worker ;
    just_maintained ;
    _ ;
  } =
  Lwt_canceler.cancel canceler >>= fun () ->
  Lwt_utils.may ~f:P2p_discovery.shutdown discovery >>= fun () ->
  maintain_worker >>= fun () ->
  Lwt_condition.broadcast just_maintained () ;
  Lwt.return_unit
