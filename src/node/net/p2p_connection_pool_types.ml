(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open P2p_types

module Point_info = struct

  type 'data state =
    | Requested of { cancel: Canceler.t }
    | Accepted of { current_gid: Gid.t ;
                    cancel: Canceler.t }
    | Running of { data: 'data ;
                   current_gid: Gid.t }
    | Disconnected

  module Event = struct

    type kind =
      | Outgoing_request
      | Accepting_request of Gid.t
      | Rejecting_request of Gid.t
      | Request_rejected of Gid.t option
      | Connection_established of Gid.t
      | Disconnection of Gid.t
      | External_disconnection of Gid.t

    type t = {
      kind : kind ;
      timestamp : Time.t ;
    }

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
    mutable last_rejected_connection : (Gid.t * Time.t) option ;
    mutable last_established_connection : (Gid.t * Time.t) option ;
    mutable last_disconnection : (Gid.t * Time.t) option ;
    greylisting : greylisting_config ;
    mutable greylisting_delay : float ;
    mutable greylisting_end : Time.t ;
    events : Event.t Ring.t ;
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
    greylisting_end = Time.now () ;
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
      (map_option ~f:(fun (_, time) -> time) @@
       recent s.last_rejected_connection s.last_disconnection) with
    | (None, None) -> None
    | (None, (Some _ as a))
    | (Some _ as a, None) -> a
    | (Some t1 as a1 , (Some t2 as a2)) ->
        if Time.compare t1 t2 < 0 then a2 else a1

  let fold_events { events } ~init ~f = Ring.fold events ~init ~f

  let log { events } ?(timestamp = Time.now ()) kind =
    Ring.add events { kind ; timestamp }

  let log_incoming_rejection ?timestamp point_info gid =
    log point_info ?timestamp (Rejecting_request gid)

  module State = struct

    type 'data t = 'data state =
      | Requested of { cancel: Canceler.t }
      | Accepted of { current_gid: Gid.t ;
                      cancel: Canceler.t }
      | Running of { data: 'data ;
                     current_gid: Gid.t }
      | Disconnected
    type 'data state = 'data t

    let pp ppf = function
      | Requested _ ->
          Format.fprintf ppf "requested"
      | Accepted { current_gid } ->
          Format.fprintf ppf "accepted %a" Gid.pp current_gid
      | Running { current_gid } ->
          Format.fprintf ppf "running %a" Gid.pp current_gid
      | Disconnected ->
          Format.fprintf ppf "disconnected"

    let get { state } = state

    let is_disconnected { state } =
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
        point_info current_gid cancel =
      (* log_notice "SET_ACCEPTED %a@." Point.pp point_info.point ; *)
      assert begin
        match point_info.state with
        | Accepted _ | Running _ -> false
        | Requested _ | Disconnected -> true
      end ;
      point_info.state <- Accepted { current_gid ; cancel } ;
      log point_info ~timestamp (Accepting_request current_gid)

    let set_running
        ?(timestamp = Time.now ())
        point_info gid data =
      assert begin
        match point_info.state with
        | Disconnected -> true (* request to unknown gid. *)
        | Running _ -> false
        | Accepted { current_gid } -> Gid.equal gid current_gid
        | Requested _ -> true
      end ;
      point_info.state <- Running { data ; current_gid = gid } ;
      point_info.last_established_connection <- Some (gid, timestamp) ;
      log point_info ~timestamp (Connection_established gid)

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
        | Accepted { current_gid } ->
            set_greylisted timestamp point_info ;
            point_info.last_rejected_connection <-
              Some (current_gid, timestamp) ;
            Request_rejected (Some current_gid)
        | Running { current_gid } ->
            point_info.greylisting_delay <-
              float_of_int point_info.greylisting.initial_delay ;
            point_info.greylisting_end <-
              Time.add timestamp
                (Int64.of_int point_info.greylisting.disconnection_delay) ;
            point_info.last_disconnection <- Some (current_gid, timestamp) ;
            if requested
            then Disconnection current_gid
            else External_disconnection current_gid
        | Disconnected ->
            assert false
      in
      point_info.state <- Disconnected ;
      log point_info ~timestamp event

  end

end

module Gid_info = struct

  type 'data state =
    | Accepted of { current_point: Id_point.t ;
                    cancel: Canceler.t }
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
      let open Data_encoding in
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
           (kind, timestamp, Ipaddr.V6.to_string addr, port))
        (fun (kind, timestamp, addr, port) ->
           let addr = Ipaddr.V6.of_string_exn addr in
           { kind ; timestamp ; point = (addr, port) })
        (obj4
           (req "kind" kind_encoding)
           (req "timestamp" Time.encoding)
           (req "addr" string)
           (opt "port" int16))

  end

  type ('conn, 'meta) t = {
    gid : Gid.t ;
    mutable state : 'conn state ;
    mutable metadata : 'meta ;
    mutable trusted : bool ;
    mutable last_failed_connection : (Id_point.t * Time.t) option ;
    mutable last_rejected_connection : (Id_point.t * Time.t) option ;
    mutable last_established_connection : (Id_point.t * Time.t) option ;
    mutable last_disconnection : (Id_point.t * Time.t) option ;
    events : Event.t Ring.t ;
  }
  type ('conn, 'meta) gid_info = ('conn, 'meta) t

  let compare gi1 gi2 = Gid.compare gi1.gid gi2.gid

  let log_size = 100

  let create ?(trusted = false) ~metadata gid =
    { gid ;
      state = Disconnected ;
      metadata ;
      trusted ;
      events = Ring.create log_size ;
      last_failed_connection = None ;
      last_rejected_connection = None ;
      last_established_connection = None ;
      last_disconnection = None ;
    }

  let encoding metadata_encoding =
    let open Data_encoding in
    conv
      (fun { gid ; trusted ; metadata ; events ;
           last_failed_connection ; last_rejected_connection ;
           last_established_connection ; last_disconnection } ->
         (gid, trusted, metadata, Ring.elements events,
          last_failed_connection, last_rejected_connection,
          last_established_connection, last_disconnection))
      (fun (gid, trusted, metadata, event_list,
          last_failed_connection, last_rejected_connection,
          last_established_connection, last_disconnection) ->
         let info = create ~trusted ~metadata gid in
         let events = Ring.create log_size in
         Ring.add_list info.events event_list ;
         { state = Disconnected ;
           trusted ; gid ; metadata ; events ;
           last_failed_connection ;
           last_rejected_connection ;
           last_established_connection ;
           last_disconnection ;
         })
      (obj8
         (req "gid" Gid.encoding)
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

  let gid { gid } = gid
  let metadata { metadata } = metadata
  let set_metadata gi metadata = gi.metadata <- metadata
  let trusted { trusted } = trusted
  let set_trusted gi = gi.trusted <- true
  let unset_trusted gi = gi.trusted <- false
  let fold_events { events } ~init ~f = Ring.fold events ~init ~f

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

  let log { events } ?(timestamp = Time.now ()) point kind =
    Ring.add events { kind ; timestamp ; point }

  let log_incoming_rejection ?timestamp gid_info point =
    log gid_info ?timestamp point Rejecting_request

  module State = struct

    type 'data t = 'data state =
      | Accepted of { current_point: Id_point.t ;
                      cancel: Canceler.t }
      | Running of { data: 'data ;
                     current_point: Id_point.t }
      | Disconnected
    type 'data state = 'data t

    let pp ppf = function
      | Accepted { current_point } ->
          Format.fprintf ppf "accepted %a" Id_point.pp current_point
      | Running { current_point } ->
          Format.fprintf ppf "running %a" Id_point.pp current_point
      | Disconnected ->
          Format.fprintf ppf "disconnected"

    let get { state } = state

    let is_disconnected { state } =
      match state with
      | Disconnected -> true
      | Accepted _ | Running _ -> false

    let set_accepted
        ?(timestamp = Time.now ())
        gid_info current_point cancel =
      assert begin
        match gid_info.state with
        | Accepted _ | Running _ -> false
        | Disconnected -> true
      end ;
      gid_info.state <- Accepted { current_point ; cancel } ;
      log gid_info ~timestamp current_point Accepting_request

    let set_running
        ?(timestamp = Time.now ())
        gid_info point data =
      assert begin
        match gid_info.state with
        | Disconnected -> true (* request to unknown gid. *)
        | Running _ -> false
        | Accepted { current_point } ->
            Id_point.equal point current_point
      end ;
      gid_info.state <- Running { data ; current_point = point } ;
      gid_info.last_established_connection <- Some (point, timestamp) ;
      log gid_info ~timestamp point Connection_established

    let set_disconnected
        ?(timestamp = Time.now ()) ?(requested = false) gid_info =
      let current_point, (event : Event.kind) =
        match gid_info.state with
        | Accepted { current_point } ->
            gid_info.last_rejected_connection <-
              Some (current_point, timestamp) ;
            current_point, Request_rejected
        | Running { current_point } ->
            gid_info.last_disconnection <-
              Some (current_point, timestamp) ;
            current_point,
            if requested then Disconnection else External_disconnection
        | Disconnected -> assert false
      in
      gid_info.state <- Disconnected ;
      log gid_info ~timestamp current_point event

  end

  module File = struct

    let load path metadata_encoding =
      let enc = Data_encoding.list (encoding metadata_encoding) in
      Data_encoding_ezjsonm.read_file path >|=
      map_option ~f:(Data_encoding.Json.destruct enc) >|=
      unopt ~default:[]

    let save path metadata_encoding peers =
      let open Data_encoding in
      Data_encoding_ezjsonm.write_file path @@
      Json.construct (list (encoding metadata_encoding)) peers

  end

end
