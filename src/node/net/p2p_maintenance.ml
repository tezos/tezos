(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open P2p_types
open P2p_connection_pool_types

include Logging.Make (struct let name = "p2p.maintenance" end)

type bounds = {
  min_threshold: int ;
  min_target: int ;
  max_target: int ;
  max_threshold: int ;
}

type 'meta pool = Pool : ('msg, 'meta) P2p_connection_pool.t -> 'meta pool

type 'meta t = {
  canceler: Canceler.t ;
  connection_timeout: float ;
  bounds: bounds ;
  pool: 'meta pool ;
  disco: P2p_discovery.t option ;
  just_maintained: unit Lwt_condition.t ;
  please_maintain: unit Lwt_condition.t ;
  mutable maintain_worker : unit Lwt.t ;
}

(** Select [expected] points amongst the disconnected known points.
    It ignores points which are greylisted, or for which a connection
    failed after [start_time]. It first selects points with the oldest
    last tentative. *)
let connectable st start_time expected =
  let Pool pool = st.pool in
  let now = Time.now () in
  let module Bounded_point_info =
    Utils.Bounded(struct
      type t = (Time.t option * Point.t)
      let compare (t1, _) (t2, _) =
        match t1, t2 with
        | None, None -> 0
        | None, Some _ -> 1
        | Some _, None -> -1
        | Some t1, Some t2 -> Time.compare t2 t1
    end) in
  let acc = Bounded_point_info.create expected in
  P2p_connection_pool.Points.fold_known pool ~init:()
    ~f:begin fun point pi () ->
      match Point_info.State.get pi with
      | Disconnected -> begin
          match Point_info.last_miss pi with
          | Some last when Time.(start_time < last)
                        || Point_info.greylisted ~now pi -> ()
          | last ->
              Bounded_point_info.insert (last, point) acc
        end
      | _ -> ()
    end ;
  List.map snd (Bounded_point_info.get acc)

(** Try to create connections to new peers. It tries to create at
    least [min_to_contact] connections, and will never creates more
    than [max_to_contact]. But, if after trying once all disconnected
    peers, it returns [false]. *)
let rec try_to_contact
    st ?(start_time = Time.now ())
    min_to_contact max_to_contact =
  let Pool pool = st.pool in
  if min_to_contact <= 0 then
    Lwt.return_true
  else
    let contactable =
      connectable st start_time max_to_contact in
    if contactable = [] then
      Lwt_unix.yield () >>= fun () ->
      Lwt.return_false
    else
      List.fold_left
        (fun acc point ->
           P2p_connection_pool.connect
             ~timeout:st.connection_timeout pool point >>= function
           | Ok _ -> acc >|= succ
           | Error _ -> acc)
        (Lwt.return 0)
        contactable >>= fun established ->
      try_to_contact st ~start_time
        (min_to_contact - established) (max_to_contact - established)

(** Do a maintenance step. It will terminate only when the number
    of connections is between `min_threshold` and `max_threshold`. *)
let rec maintain st =
  let Pool pool = st.pool in
  let n_connected = P2p_connection_pool.active_connections pool in
  if n_connected < st.bounds.min_threshold then
    too_few_connections st n_connected
  else if st.bounds.max_threshold < n_connected then
    too_many_connections st n_connected
  else begin
    (* end of maintenance when enough users have been reached *)
    Lwt_condition.broadcast st.just_maintained () ;
    lwt_debug "Maintenance step ended" >>= fun () ->
    return ()
  end

and too_few_connections st n_connected =
  let Pool pool = st.pool in
  (* too few connections, try and contact many peers *)
  lwt_log_notice "Too few connections (%d)" n_connected >>= fun () ->
  let min_to_contact = st.bounds.min_target - n_connected in
  let max_to_contact = st.bounds.max_target - n_connected in
  try_to_contact st min_to_contact max_to_contact >>= fun success ->
  if success then begin
    maintain st
  end else begin
    (* not enough contacts, ask the pals of our pals,
       discover the local network and then wait *)
    iter_option ~f:P2p_discovery.restart st.disco ;
    P2p_connection_pool.broadcast_bootstrap_msg pool ;
    Lwt_utils.protect ~canceler:st.canceler begin fun () ->
      Lwt.pick [
        P2p_connection_pool.Pool_event.wait_new_peer pool ;
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
  lwt_debug "Too many connections, will kill %d" to_kill >>= fun () ->
  snd @@ P2p_connection_pool.Connection.fold pool
    ~init:(to_kill, Lwt.return_unit)
    ~f:(fun _ conn (i, t) ->
        if i = 0 then (0, t)
        else (i - 1, t >>= fun () -> P2p_connection_pool.disconnect conn))
  >>= fun () ->
  maintain st

let rec worker_loop st =
  let Pool pool = st.pool in
  begin
    Lwt_utils.protect ~canceler:st.canceler begin fun () ->
      Lwt.pick [
        Lwt_unix.sleep 120. ; (* every two minutes *)
        Lwt_condition.wait st.please_maintain ; (* when asked *)
        P2p_connection_pool.Pool_event.wait_too_few_connections pool ; (* limits *)
        P2p_connection_pool.Pool_event.wait_too_many_connections pool
      ] >>= fun () ->
      return ()
    end >>=? fun () ->
    let n_connected = P2p_connection_pool.active_connections pool in
    if n_connected < st.bounds.min_threshold
       || st.bounds.max_threshold < n_connected then
      maintain st
    else begin
      P2p_connection_pool.send_swap_request pool ;
      return ()
    end
  end >>= function
  | Ok () -> worker_loop st
  | Error [Lwt_utils.Canceled] -> Lwt.return_unit
  | Error _ -> Lwt.return_unit

let run ~connection_timeout bounds pool disco =
  let canceler = Canceler.create () in
  let st = {
    canceler ;
    connection_timeout ;
    bounds ;
    pool = Pool pool ;
    disco ;
    just_maintained = Lwt_condition.create () ;
    please_maintain = Lwt_condition.create () ;
    maintain_worker = Lwt.return_unit ;
  } in
  st.maintain_worker <-
    Lwt_utils.worker "maintenance"
      (fun () -> worker_loop st)
      (fun () -> Canceler.cancel canceler) ;
  st

let maintain { just_maintained ; please_maintain } =
  let wait = Lwt_condition.wait just_maintained in
  Lwt_condition.broadcast please_maintain () ;
  wait

let shutdown {
    canceler ;
    maintain_worker ;
    just_maintained } =
  Canceler.cancel canceler >>= fun () ->
  maintain_worker >>= fun () ->
  Lwt_condition.broadcast just_maintained () ;
  Lwt.return_unit

