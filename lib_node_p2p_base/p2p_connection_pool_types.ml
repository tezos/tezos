(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open P2p_types

module Point_info = struct

  type 'data state =
    | Requested of { cancel: Lwt_canceler.t }
    | Accepted of { current_peer_id: Peer_id.t ;
                    cancel: Lwt_canceler.t }
    | Running of { data: 'data ;
                   current_peer_id: Peer_id.t }
    | Disconnected

  module Event = struct

    type kind =
      | Outgoing_request
      | Accepting_request of Peer_id.t
      | Rejecting_request of Peer_id.t
      | Request_rejected of Peer_id.t option
      | Connection_established of Peer_id.t
      | Disconnection of Peer_id.t
      | External_disconnection of Peer_id.t

    let kind_encoding =
      let open Data_encoding in
      let branch_encoding name obj =
        conv (fun x -> (), x) (fun ((), x) -> x)
          (merge_objs
             (obj1 (req "event_kind" (constant name))) obj) in
      union ~tag_size:`Uint8 [
        case ~tag:0 (branch_encoding "outgoing_request" empty)
          (function Outgoing_request -> Some () | _ -> None)
          (fun () -> Outgoing_request) ;
        case ~tag:1 (branch_encoding "accepting_request"
                       (obj1 (req "peer_id" Peer_id.encoding)))
          (function Accepting_request peer_id -> Some peer_id | _ -> None)
          (fun peer_id -> Accepting_request peer_id) ;
        case ~tag:2 (branch_encoding "rejecting_request"
                       (obj1 (req "peer_id" Peer_id.encoding)))
          (function Rejecting_request peer_id -> Some peer_id | _ -> None)
          (fun peer_id -> Rejecting_request peer_id) ;
        case ~tag:3 (branch_encoding "request_rejected"
                       (obj1 (opt "peer_id" Peer_id.encoding)))
          (function Request_rejected peer_id -> Some peer_id | _ -> None)
          (fun peer_id -> Request_rejected peer_id) ;
        case ~tag:4 (branch_encoding "rejecting_request"
                       (obj1 (req "peer_id" Peer_id.encoding)))
          (function Connection_established peer_id -> Some peer_id | _ -> None)
          (fun peer_id -> Connection_established peer_id) ;
        case ~tag:5 (branch_encoding "rejecting_request"
                       (obj1 (req "peer_id" Peer_id.encoding)))
          (function Disconnection peer_id -> Some peer_id | _ -> None)
          (fun peer_id -> Disconnection peer_id) ;
        case ~tag:6 (branch_encoding "rejecting_request"
                       (obj1 (req "peer_id" Peer_id.encoding)))
          (function External_disconnection peer_id -> Some peer_id | _ -> None)
          (fun peer_id -> External_disconnection peer_id) ;
      ]

    type t = {
      kind : kind ;
      timestamp : Time.t ;
    }

    let encoding =
      let open Data_encoding in
      conv
        (fun { kind ; timestamp ; } -> (kind, timestamp))
        (fun (kind, timestamp) -> { kind ; timestamp ; })
        (obj2
           (req "kind" kind_encoding)
           (req "timestamp" Time.encoding))
  end

  type greylisting_config = {
    factor: float ;
    initial_delay: int ;
    disconnection_delay: int ;
  }

  type 'data t = {
    point : Point.t ;
    mutable trusted : bool ;
    mutable state : 'data state ;
    mutable last_failed_connection : Time.t option ;
    mutable last_rejected_connection : (Peer_id.t * Time.t) option ;
    mutable last_established_connection : (Peer_id.t * Time.t) option ;
    mutable last_disconnection : (Peer_id.t * Time.t) option ;
    greylisting : greylisting_config ;
    mutable greylisting_delay : float ;
    mutable greylisting_end : Time.t ;
    events : Event.t Ring.t ;
    watchers : Event.t Lwt_watcher.input ;
  }
  type 'data point_info = 'data t

  let compare pi1 pi2 = Point.compare pi1.point pi2.point

  let log_size = 100

  let default_greylisting_config = {
    factor = 1.2 ;
    initial_delay = 1 ;
    disconnection_delay = 60 ;
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
    events = Ring.create log_size ;
    greylisting = greylisting_config ;
    greylisting_delay = 1. ;
    greylisting_end = Time.epoch ;
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
  let greylisted ?(now = Time.now ()) s =
    Time.compare now s.greylisting_end <= 0
  let greylisted_until s = s.greylisting_end

  let recent a1 a2 =
    match a1, a2 with
    | (None, None) -> None
    | (None, (Some _ as a))
    | (Some _ as a, None) -> a
    | (Some (_, t1), Some (_, t2)) ->
        if Time.compare t1 t2 < 0 then a2 else a1
  let last_seen s =
    recent s.last_rejected_connection
      (recent s.last_established_connection s.last_disconnection)
  let last_miss s =
    match
      s.last_failed_connection,
      (Option.map ~f:(fun (_, time) -> time) @@
       recent s.last_rejected_connection s.last_disconnection) with
    | (None, None) -> None
    | (None, (Some _ as a))
    | (Some _ as a, None) -> a
    | (Some t1 as a1 , (Some t2 as a2)) ->
        if Time.compare t1 t2 < 0 then a2 else a1

  let fold_events { events ; _ } ~init ~f = Ring.fold events ~init ~f

  let watch { watchers ; _ } = Lwt_watcher.create_stream watchers

  let log { events ; watchers ; _ } ?(timestamp = Time.now ()) kind =
    let event = { Event.kind ; timestamp } in
    Ring.add events event ;
    Lwt_watcher.notify watchers event

  let log_incoming_rejection ?timestamp point_info peer_id =
    log point_info ?timestamp (Rejecting_request peer_id)

  module State = struct

    type 'data t = 'data state =
      | Requested of { cancel: Lwt_canceler.t }
      | Accepted of { current_peer_id: Peer_id.t ;
                      cancel: Lwt_canceler.t }
      | Running of { data: 'data ;
                     current_peer_id: Peer_id.t }
      | Disconnected
    type 'data state = 'data t

    let pp ppf = function
      | Requested _ ->
          Format.fprintf ppf "requested"
      | Accepted { current_peer_id ; _ } ->
          Format.fprintf ppf "accepted %a" Peer_id.pp current_peer_id
      | Running { current_peer_id ; _ } ->
          Format.fprintf ppf "running %a" Peer_id.pp current_peer_id
      | Disconnected ->
          Format.fprintf ppf "disconnected"

    let get { state ; _ } = state

    let is_disconnected { state ; _ } =
      match state with
      | Disconnected -> true
      | Requested _ | Accepted _ | Running _ -> false

    let set_requested ?timestamp point_info cancel =
      assert begin
        match point_info.state with
        | Requested _ -> true
        | Accepted _ | Running _ -> false
        | Disconnected -> true
      end ;
      point_info.state <- Requested { cancel } ;
      log point_info ?timestamp Outgoing_request

    let set_accepted
        ?(timestamp = Time.now ())
        point_info current_peer_id cancel =
      (* log_notice "SET_ACCEPTED %a@." Point.pp point_info.point ; *)
      assert begin
        match point_info.state with
        | Accepted _ | Running _ -> false
        | Requested _ | Disconnected -> true
      end ;
      point_info.state <- Accepted { current_peer_id ; cancel } ;
      log point_info ~timestamp (Accepting_request current_peer_id)

    let set_running
        ?(timestamp = Time.now ())
        point_info peer_id data =
      assert begin
        match point_info.state with
        | Disconnected -> true (* request to unknown peer_id. *)
        | Running _ -> false
        | Accepted { current_peer_id ; _ } -> Peer_id.equal peer_id current_peer_id
        | Requested _ -> true
      end ;
      point_info.state <- Running { data ; current_peer_id = peer_id } ;
      point_info.last_established_connection <- Some (peer_id, timestamp) ;
      log point_info ~timestamp (Connection_established peer_id)

    let set_greylisted timestamp point_info =
      point_info.greylisting_end <-
        Time.add
          timestamp
          (Int64.of_float point_info.greylisting_delay) ;
      point_info.greylisting_delay <-
        point_info.greylisting_delay *. point_info.greylisting.factor

    let set_disconnected
        ?(timestamp = Time.now ()) ?(requested = false) point_info =
      let event : Event.kind =
        match point_info.state with
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
              float_of_int point_info.greylisting.initial_delay ;
            point_info.greylisting_end <-
              Time.add timestamp
                (Int64.of_int point_info.greylisting.disconnection_delay) ;
            point_info.last_disconnection <- Some (current_peer_id, timestamp) ;
            if requested
            then Disconnection current_peer_id
            else External_disconnection current_peer_id
        | Disconnected ->
            assert false
      in
      point_info.state <- Disconnected ;
      log point_info ~timestamp event

  end

end

module Peer_info = struct

  type 'data state =
    | Accepted of { current_point: Id_point.t ;
                    cancel: Lwt_canceler.t }
    | Running of { data: 'data ;
                   current_point: Id_point.t }
    | Disconnected

  module Event = struct

    type kind =
      | Accepting_request
      | Rejecting_request
      | Request_rejected
      | Connection_established
      | Disconnection
      | External_disconnection

    let kind_encoding =
      Data_encoding.string_enum [
        "incoming_request", Accepting_request ;
        "rejecting_request", Rejecting_request ;
        "request_rejected", Request_rejected ;
        "connection_established", Connection_established ;
        "disconnection", Disconnection ;
        "external_disconnection", External_disconnection ;
      ]

    type t = {
      kind : kind ;
      timestamp : Time.t ;
      point : Id_point.t ;
    }

    let encoding =
      let open Data_encoding in
      conv
        (fun { kind ; timestamp ; point = (addr, port) } ->
           (kind, timestamp, addr, port))
        (fun (kind, timestamp, addr, port) ->
           { kind ; timestamp ; point = (addr, port) })
        (obj4
           (req "kind" kind_encoding)
           (req "timestamp" Time.encoding)
           (req "addr" P2p_types.addr_encoding)
           (opt "port" int16))

  end

  type ('conn, 'meta) t = {
    peer_id : Peer_id.t ;
    created : Time.t ;
    mutable state : 'conn state ;
    mutable metadata : 'meta ;
    mutable trusted : bool ;
    mutable last_failed_connection : (Id_point.t * Time.t) option ;
    mutable last_rejected_connection : (Id_point.t * Time.t) option ;
    mutable last_established_connection : (Id_point.t * Time.t) option ;
    mutable last_disconnection : (Id_point.t * Time.t) option ;
    events : Event.t Ring.t ;
    watchers : Event.t Lwt_watcher.input ;
  }
  type ('conn, 'meta) peer_info = ('conn, 'meta) t

  let compare gi1 gi2 = Peer_id.compare gi1.peer_id gi2.peer_id

  let log_size = 100

  let create ?(created = Time.now ()) ?(trusted = false) ~metadata peer_id =
    { peer_id ;
      created ;
      state = Disconnected ;
      metadata ;
      trusted ;
      last_failed_connection = None ;
      last_rejected_connection = None ;
      last_established_connection = None ;
      last_disconnection = None ;
      events = Ring.create log_size ;
      watchers = Lwt_watcher.create_input () ;
    }

  let encoding metadata_encoding =
    let open Data_encoding in
    conv
      (fun { peer_id ; trusted ; metadata ; events ; created ;
             last_failed_connection ; last_rejected_connection ;
             last_established_connection ; last_disconnection ; _ } ->
        (peer_id, created, trusted, metadata, Ring.elements events,
         last_failed_connection, last_rejected_connection,
         last_established_connection, last_disconnection))
      (fun (peer_id, created, trusted, metadata, event_list,
            last_failed_connection, last_rejected_connection,
            last_established_connection, last_disconnection) ->
        let info = create ~trusted ~metadata peer_id in
        let events = Ring.create log_size in
        Ring.add_list info.events event_list ;
        { state = Disconnected ;
          trusted ; peer_id ; metadata ; created ;
          last_failed_connection ;
          last_rejected_connection ;
          last_established_connection ;
          last_disconnection ;
          events ;
          watchers = Lwt_watcher.create_input () ;
        })
      (obj9
         (req "peer_id" Peer_id.encoding)
         (req "created" Time.encoding)
         (dft "trusted" bool false)
         (req "metadata" metadata_encoding)
         (dft "events" (list Event.encoding) [])
         (opt "last_failed_connection"
            (tup2 Id_point.encoding Time.encoding))
         (opt "last_rejected_connection"
            (tup2 Id_point.encoding Time.encoding))
         (opt "last_established_connection"
            (tup2 Id_point.encoding Time.encoding))
         (opt "last_disconnection"
            (tup2 Id_point.encoding Time.encoding)))

  let peer_id { peer_id ; _ } = peer_id
  let created { created ; _ } = created
  let metadata { metadata ; _ } = metadata
  let set_metadata gi metadata = gi.metadata <- metadata
  let trusted { trusted ; _ } = trusted
  let set_trusted gi = gi.trusted <- true
  let unset_trusted gi = gi.trusted <- false
  let fold_events { events ; _ } ~init ~f = Ring.fold events ~init ~f

  let last_established_connection s = s.last_established_connection
  let last_disconnection s = s.last_disconnection
  let last_failed_connection s = s.last_failed_connection
  let last_rejected_connection s = s.last_rejected_connection

  let recent = Point_info.recent
  let last_seen s =
    recent
      s.last_established_connection
      (recent s.last_rejected_connection s.last_disconnection)
  let last_miss s =
    recent
      s.last_failed_connection
      (recent s.last_rejected_connection s.last_disconnection)

  let log { events ; watchers ; _ } ?(timestamp = Time.now ()) point kind =
    let event = { Event.kind ; timestamp ; point } in
    Ring.add events event ;
    Lwt_watcher.notify watchers event

  let watch { watchers ; _ } = Lwt_watcher.create_stream watchers

  let log_incoming_rejection ?timestamp peer_info point =
    log peer_info ?timestamp point Rejecting_request

  module State = struct

    type 'data t = 'data state =
      | Accepted of { current_point: Id_point.t ;
                      cancel: Lwt_canceler.t }
      | Running of { data: 'data ;
                     current_point: Id_point.t }
      | Disconnected
    type 'data state = 'data t

    let pp ppf = function
      | Accepted { current_point ; _ } ->
          Format.fprintf ppf "accepted %a" Id_point.pp current_point
      | Running { current_point ; _ } ->
          Format.fprintf ppf "running %a" Id_point.pp current_point
      | Disconnected ->
          Format.fprintf ppf "disconnected"

    let get { state ; _ } = state

    let is_disconnected { state ; _ } =
      match state with
      | Disconnected -> true
      | Accepted _ | Running _ -> false

    let set_accepted
        ?(timestamp = Time.now ())
        peer_info current_point cancel =
      assert begin
        match peer_info.state with
        | Accepted _ | Running _ -> false
        | Disconnected -> true
      end ;
      peer_info.state <- Accepted { current_point ; cancel } ;
      log peer_info ~timestamp current_point Accepting_request

    let set_running
        ?(timestamp = Time.now ())
        peer_info point data =
      assert begin
        match peer_info.state with
        | Disconnected -> true (* request to unknown peer_id. *)
        | Running _ -> false
        | Accepted { current_point ; _ } ->
            Id_point.equal point current_point
      end ;
      peer_info.state <- Running { data ; current_point = point } ;
      peer_info.last_established_connection <- Some (point, timestamp) ;
      log peer_info ~timestamp point Connection_established

    let set_disconnected
        ?(timestamp = Time.now ()) ?(requested = false) peer_info =
      let current_point, (event : Event.kind) =
        match peer_info.state with
        | Accepted { current_point ; _ } ->
            peer_info.last_rejected_connection <-
              Some (current_point, timestamp) ;
            current_point, Request_rejected
        | Running { current_point ; _ } ->
            peer_info.last_disconnection <-
              Some (current_point, timestamp) ;
            current_point,
            if requested then Disconnection else External_disconnection
        | Disconnected -> assert false
      in
      peer_info.state <- Disconnected ;
      log peer_info ~timestamp current_point event

  end

  module File = struct

    let load path metadata_encoding =
      let enc = Data_encoding.list (encoding metadata_encoding) in
      if path <> "/dev/null" && Sys.file_exists path then
        Data_encoding_ezjsonm.read_file path >>=? fun json ->
        return (Data_encoding.Json.destruct enc json)
      else
        return []

    let save path metadata_encoding peers =
      let open Data_encoding in
      Data_encoding_ezjsonm.write_file path @@
      Json.construct (list (encoding metadata_encoding)) peers

  end

end
