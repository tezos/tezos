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

open P2p_point

type 'data t =
  | Requested of { cancel: Lwt_canceler.t }
  | Accepted of { current_peer_id: P2p_peer.Id.t ;
                  cancel: Lwt_canceler.t }
  | Running of { data: 'data ;
                 current_peer_id: P2p_peer.Id.t }
  | Disconnected
type 'data state = 'data t

let pp ppf = function
  | Requested _ ->
      Format.fprintf ppf "requested"
  | Accepted { current_peer_id ; _ } ->
      Format.fprintf ppf "accepted %a" P2p_peer.Id.pp current_peer_id
  | Running { current_peer_id ; _ } ->
      Format.fprintf ppf "running %a" P2p_peer.Id.pp current_peer_id
  | Disconnected ->
      Format.fprintf ppf "disconnected"

module Info = struct

  type greylisting_config = {
    factor: float ;
    initial_delay: Time.System.Span.t ;
    disconnection_delay: Time.System.Span.t ;
  }

  type 'data t = {
    point : Id.t ;
    mutable trusted : bool ;
    mutable state : 'data state ;
    mutable last_failed_connection : Time.System.t option ;
    mutable last_rejected_connection : (P2p_peer.Id.t * Time.System.t) option ;
    mutable last_established_connection : (P2p_peer.Id.t * Time.System.t) option ;
    mutable known_public : bool ;
    mutable last_disconnection : (P2p_peer.Id.t * Time.System.t) option ;
    greylisting : greylisting_config ;
    mutable greylisting_delay : Time.System.Span.t ;
    mutable greylisting_end : Time.System.t ;
    events : Pool_event.t Ring.t ;
    watchers : Pool_event.t Lwt_watcher.input ;
  }
  type 'data point_info = 'data t

  let compare pi1 pi2 = Id.compare pi1.point pi2.point

  let log_size = 100

  let default_greylisting_config = {
    factor = 1.2 ;
    initial_delay = Ptime.Span.of_int_s 1 ;
    disconnection_delay = Ptime.Span.of_int_s 60 ;
  }

  let create
      ?(trusted = false)
      ?(greylisting_config = default_greylisting_config) addr  port = {
    point = (addr, port) ;
    trusted ;
    state = Disconnected ;
    last_failed_connection = None ;
    last_rejected_connection = None ;
    last_established_connection = None ;
    last_disconnection = None ;
    known_public = false ;
    events = Ring.create log_size ;
    greylisting = greylisting_config ;
    greylisting_delay = Ptime.Span.of_int_s 1 ;
    greylisting_end = Time.System.epoch ;
    watchers = Lwt_watcher.create_input () ;
  }

  let point s = s.point
  let trusted s = s.trusted
  let set_trusted gi = gi.trusted <- true
  let unset_trusted gi = gi.trusted <- false
  let last_established_connection s = s.last_established_connection
  let last_disconnection s = s.last_disconnection
  let last_failed_connection s = s.last_failed_connection
  let last_rejected_connection s = s.last_rejected_connection
  let known_public s = s.known_public
  let greylisted ?(now = Systime_os.now ()) s =
    Time.System.compare now s.greylisting_end <= 0
  let greylisted_until s = s.greylisting_end

  let last_seen s =
    Time.System.recent s.last_rejected_connection
      (Time.System.recent s.last_established_connection s.last_disconnection)
  let last_miss s =
    match
      s.last_failed_connection,
      (Option.map ~f:(fun (_, time) -> time) @@
       Time.System.recent s.last_rejected_connection s.last_disconnection) with
    | (None, None) -> None
    | (None, (Some _ as a))
    | (Some _ as a, None) -> a
    | (Some t1 as a1 , (Some t2 as a2)) ->
        if Time.System.compare t1 t2 < 0 then a2 else a1

  let log { events ; watchers ; _ } ?timestamp kind =
    let time = Option.unopt ~default:(Systime_os.now ()) timestamp in
    let event = Time.System.stamp ~time kind in
    Ring.add events event ;
    Lwt_watcher.notify watchers event

  let log_incoming_rejection ?timestamp point_info peer_id =
    log point_info ?timestamp (Rejecting_request peer_id)


  let fold { events ; _ } ~init ~f = Ring.fold events ~init ~f

  let watch { watchers ; _ } = Lwt_watcher.create_stream watchers

end

let get { Info.state ; _ } = state

let is_disconnected { Info.state ; _ } =
  match state with
  | Disconnected -> true
  | Requested _ | Accepted _ | Running _ -> false

let set_requested ?timestamp point_info cancel =
  assert begin
    match point_info.Info.state with
    | Requested _ -> true
    | Accepted _ | Running _ -> false
    | Disconnected -> true
  end ;
  point_info.state <- Requested { cancel } ;
  Info.log point_info ?timestamp Outgoing_request

let set_accepted
    ?(timestamp = Systime_os.now ())
    point_info current_peer_id cancel =
  (* log_notice "SET_ACCEPTED %a@." P2p_point.pp point_info.point ; *)
  assert begin
    match point_info.Info.state with
    | Accepted _ | Running _ -> false
    | Requested _ | Disconnected -> true
  end ;
  point_info.state <- Accepted { current_peer_id ; cancel } ;
  Info.log point_info ~timestamp (Accepting_request current_peer_id)

let set_running
    ?(timestamp = Systime_os.now ())
    ~known_private point_info peer_id data  =
  assert begin
    match point_info.Info.state with
    | Disconnected -> true (* request to unknown peer_id. *)
    | Running _ -> false
    | Accepted { current_peer_id ; _ } -> P2p_peer.Id.equal peer_id current_peer_id
    | Requested _ -> true
  end ;
  point_info.state <- Running { data ; current_peer_id = peer_id } ;
  point_info.known_public <- not known_private ;
  point_info.last_established_connection <- Some (peer_id, timestamp) ;
  Info.log point_info ~timestamp (Connection_established peer_id)

let set_greylisted timestamp point_info =
  point_info.Info.greylisting_end <-
    Option.unopt_exn
      (Failure "P2p_point_state.set_greylisted: overflow in time")
      (Ptime.add_span
         timestamp
         point_info.Info.greylisting_delay) ;
  point_info.greylisting_delay <-
    Time.System.Span.multiply_exn
      point_info.greylisting.factor
      point_info.greylisting_delay

let set_disconnected
    ?(timestamp = Systime_os.now ()) ?(requested = false) point_info =
  let event : Pool_event.kind =
    match point_info.Info.state with
    | Requested _ ->
        set_greylisted timestamp point_info ;
        point_info.last_failed_connection <- Some timestamp ;
        Request_rejected None
    | Accepted { current_peer_id ; _ } ->
        set_greylisted timestamp point_info ;
        point_info.last_rejected_connection <-
          Some (current_peer_id, timestamp) ;
        Request_rejected (Some current_peer_id)
    | Running { current_peer_id ; _ } ->
        point_info.greylisting_delay <-
          point_info.greylisting.initial_delay ;
        point_info.greylisting_end <-
          Option.unopt_exn
            (Failure "P2p_point_state.set_disconnected: overflow in time")
            (Ptime.add_span timestamp
               point_info.greylisting.disconnection_delay) ;
        point_info.last_disconnection <- Some (current_peer_id, timestamp) ;
        if requested
        then Disconnection current_peer_id
        else External_disconnection current_peer_id
    | Disconnected ->
        assert false
  in
  point_info.state <- Disconnected ;
  Info.log point_info ~timestamp event
