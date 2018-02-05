(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type peer_id = Crypto_box.Public_key_hash.t
let peer_id_encoding = Crypto_box.Public_key_hash.encoding
let peer_id_pp = Crypto_box.Public_key_hash.pp
let peer_id_equal = Crypto_box.Public_key_hash.equal

module Id = struct

  (* A net point (address x port). *)
  type t = P2p_addr.t * P2p_addr.port
  let compare (a1, p1) (a2, p2) =
    match Ipaddr.V6.compare a1 a2 with
    | 0 -> p1 - p2
    | x -> x
  let equal p1 p2 = compare p1 p2 = 0
  let hash = Hashtbl.hash
  let pp ppf (addr, port) =
    match Ipaddr.v4_of_v6 addr with
    | Some addr ->
        Format.fprintf ppf "%a:%d" Ipaddr.V4.pp_hum addr port
    | None ->
        Format.fprintf ppf "[%a]:%d" Ipaddr.V6.pp_hum addr port
  let pp_opt ppf = function
    | None -> Format.pp_print_string ppf "none"
    | Some point -> pp ppf point

  let is_local (addr, _) = Ipaddr.V6.is_private addr
  let is_global (addr, _) = not @@ Ipaddr.V6.is_private addr

  let check_port port =
    if TzString.mem_char port '[' ||
       TzString.mem_char port ']' ||
       TzString.mem_char port ':' then
      invalid_arg "Utils.parse_addr_port (invalid character in port)"

  let parse_addr_port s =
    let len = String.length s in
    if len = 0 then
      ("", "")
    else if s.[0] = '[' then begin (* inline IPv6 *)
      match String.rindex s ']' with
      | exception Not_found ->
          invalid_arg "Utils.parse_addr_port (missing ']')"
      | pos ->
          let addr = String.sub s 1 (pos - 1) in
          let port =
            if pos = len - 1 then
              ""
            else if s.[pos+1] <> ':' then
              invalid_arg "Utils.parse_addr_port (unexpected char after ']')"
            else
              String.sub s (pos + 2) (len - pos - 2) in
          check_port port ;
          addr, port
    end else begin
      match String.rindex s ']' with
      | _pos ->
          invalid_arg "Utils.parse_addr_port (unexpected char ']')"
      | exception Not_found ->
          match String.index s ':' with
          | exception _ -> s, ""
          | pos ->
              match String.index_from s (pos+1) ':'  with
              | exception _ ->
                  let addr = String.sub s 0 pos in
                  let port = String.sub s (pos + 1) (len - pos - 1) in
                  check_port port ;
                  addr, port
              | _pos ->
                  invalid_arg "Utils.parse_addr_port: IPv6 addresses must be bracketed"
    end

  let of_string_exn str =
    let addr, port = parse_addr_port str in
    let port = int_of_string port in
    if port < 0 && port > 1 lsl 16 - 1 then
      invalid_arg "port must be between 0 and 65535" ;
    match Ipaddr.of_string_exn addr with
    | V4 addr -> Ipaddr.v6_of_v4 addr, port
    | V6 addr -> addr, port

  let of_string str =
    try Ok (of_string_exn str) with
    | Invalid_argument s -> Error s
    | Failure s -> Error s
    | _ -> Error "P2p_point.of_string"

  let to_string saddr = Format.asprintf "%a" pp saddr

  let encoding =
    Data_encoding.conv to_string of_string_exn Data_encoding.string

end

module Map = Map.Make (Id)
module Set = Set.Make (Id)
module Table = Hashtbl.Make (Id)

module State = struct

  type t =
    | Requested
    | Accepted of peer_id
    | Running of peer_id
    | Disconnected

  let of_peer_id = function
    | Requested -> None
    | Accepted pi -> Some pi
    | Running pi -> Some pi
    | Disconnected -> None

  let of_peerid_state state pi =
    match state, pi with
    | Requested, _ -> Requested
    | Accepted _, Some pi -> Accepted pi
    | Running _, Some pi -> Running pi
    | Disconnected, _ -> Disconnected
    | _ -> invalid_arg "state_of_state_peerid"

  let pp_digram ppf = function
    | Requested -> Format.fprintf ppf "⚎"
    | Accepted _ -> Format.fprintf ppf "⚍"
    | Running _ -> Format.fprintf ppf "⚌"
    | Disconnected -> Format.fprintf ppf "⚏"

  let encoding =
    let open Data_encoding in
    let branch_encoding name obj =
      conv (fun x -> (), x) (fun ((), x) -> x)
        (merge_objs
           (obj1 (req "event_kind" (constant name))) obj) in
    union ~tag_size:`Uint8 [
      case (Tag 0) (branch_encoding "requested" empty)
        (function Requested -> Some () | _ -> None)
        (fun () -> Requested) ;
      case (Tag 1) (branch_encoding "accepted"
                      (obj1 (req "peer_id" peer_id_encoding)))
        (function Accepted peer_id -> Some peer_id | _ -> None)
        (fun peer_id -> Accepted peer_id) ;
      case (Tag 2) (branch_encoding "running"
                      (obj1 (req "peer_id" peer_id_encoding)))
        (function Running peer_id -> Some peer_id | _ -> None)
        (fun peer_id -> Running peer_id) ;
      case (Tag 3) (branch_encoding "disconnected" empty)
        (function Disconnected -> Some () | _ -> None)
        (fun () -> Disconnected) ;
    ]

end

module Info = struct

  type t = {
    trusted : bool ;
    greylisted_until : Time.t ;
    state : State.t ;
    last_failed_connection : Time.t option ;
    last_rejected_connection : (peer_id * Time.t) option ;
    last_established_connection : (peer_id * Time.t) option ;
    last_disconnection : (peer_id * Time.t) option ;
    last_seen : (peer_id * Time.t) option ;
    last_miss : Time.t option ;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun { trusted ; greylisted_until ; state ;
             last_failed_connection ; last_rejected_connection ;
             last_established_connection ; last_disconnection ;
             last_seen ; last_miss } ->
        let peer_id = State.of_peer_id state in
        (trusted, greylisted_until, state, peer_id,
         last_failed_connection, last_rejected_connection,
         last_established_connection, last_disconnection,
         last_seen, last_miss))
      (fun (trusted, greylisted_until, state, peer_id,
            last_failed_connection, last_rejected_connection,
            last_established_connection, last_disconnection,
            last_seen, last_miss) ->
        let state = State.of_peerid_state state peer_id in
        { trusted ; greylisted_until ; state ;
          last_failed_connection ; last_rejected_connection ;
          last_established_connection ; last_disconnection ;
          last_seen ; last_miss })
      (obj10
         (req "trusted" bool)
         (dft "greylisted_until" Time.encoding Time.epoch)
         (req "state" State.encoding)
         (opt "peer_id" peer_id_encoding)
         (opt "last_failed_connection" Time.encoding)
         (opt "last_rejected_connection" (tup2 peer_id_encoding Time.encoding))
         (opt "last_established_connection" (tup2 peer_id_encoding Time.encoding))
         (opt "last_disconnection" (tup2 peer_id_encoding Time.encoding))
         (opt "last_seen" (tup2 peer_id_encoding Time.encoding))
         (opt "last_miss" Time.encoding))

end

module Event = struct

  type kind =
    | Outgoing_request
    | Accepting_request of peer_id
    | Rejecting_request of peer_id
    | Request_rejected of peer_id option
    | Connection_established of peer_id
    | Disconnection of peer_id
    | External_disconnection of peer_id

  let kind_encoding =
    let open Data_encoding in
    let branch_encoding name obj =
      conv (fun x -> (), x) (fun ((), x) -> x)
        (merge_objs
           (obj1 (req "event_kind" (constant name))) obj) in
    union ~tag_size:`Uint8 [
      case (Tag 0) (branch_encoding "outgoing_request" empty)
        (function Outgoing_request -> Some () | _ -> None)
        (fun () -> Outgoing_request) ;
      case (Tag 1) (branch_encoding "accepting_request"
                      (obj1 (req "peer_id" peer_id_encoding)))
        (function Accepting_request peer_id -> Some peer_id | _ -> None)
        (fun peer_id -> Accepting_request peer_id) ;
      case (Tag 2) (branch_encoding "rejecting_request"
                      (obj1 (req "peer_id" peer_id_encoding)))
        (function Rejecting_request peer_id -> Some peer_id | _ -> None)
        (fun peer_id -> Rejecting_request peer_id) ;
      case (Tag 3) (branch_encoding "request_rejected"
                      (obj1 (opt "peer_id" peer_id_encoding)))
        (function Request_rejected peer_id -> Some peer_id | _ -> None)
        (fun peer_id -> Request_rejected peer_id) ;
      case (Tag 4) (branch_encoding "rejecting_request"
                      (obj1 (req "peer_id" peer_id_encoding)))
        (function Connection_established peer_id -> Some peer_id | _ -> None)
        (fun peer_id -> Connection_established peer_id) ;
      case (Tag 5) (branch_encoding "rejecting_request"
                      (obj1 (req "peer_id" peer_id_encoding)))
        (function Disconnection peer_id -> Some peer_id | _ -> None)
        (fun peer_id -> Disconnection peer_id) ;
      case (Tag 6) (branch_encoding "rejecting_request"
                      (obj1 (req "peer_id" peer_id_encoding)))
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

module Pool_info = struct

  type 'data state =
    | Requested of { cancel: Lwt_canceler.t }
    | Accepted of { current_peer_id: peer_id ;
                    cancel: Lwt_canceler.t }
    | Running of { data: 'data ;
                   current_peer_id: peer_id }
    | Disconnected

  type greylisting_config = {
    factor: float ;
    initial_delay: int ;
    disconnection_delay: int ;
  }

  type 'data t = {
    point : Id.t ;
    mutable trusted : bool ;
    mutable state : 'data state ;
    mutable last_failed_connection : Time.t option ;
    mutable last_rejected_connection : (peer_id * Time.t) option ;
    mutable last_established_connection : (peer_id * Time.t) option ;
    mutable last_disconnection : (peer_id * Time.t) option ;
    greylisting : greylisting_config ;
    mutable greylisting_delay : float ;
    mutable greylisting_end : Time.t ;
    events : Event.t Ring.t ;
    watchers : Event.t Lwt_watcher.input ;
  }
  type 'data point_info = 'data t

  let compare pi1 pi2 = Id.compare pi1.point pi2.point

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

  let last_seen s =
    Time.recent s.last_rejected_connection
      (Time.recent s.last_established_connection s.last_disconnection)
  let last_miss s =
    match
      s.last_failed_connection,
      (Option.map ~f:(fun (_, time) -> time) @@
       Time.recent s.last_rejected_connection s.last_disconnection) with
    | (None, None) -> None
    | (None, (Some _ as a))
    | (Some _ as a, None) -> a
    | (Some t1 as a1 , (Some t2 as a2)) ->
        if Time.compare t1 t2 < 0 then a2 else a1

  let log { events ; watchers ; _ } ?(timestamp = Time.now ()) kind =
    let event = { Event.kind ; timestamp } in
    Ring.add events event ;
    Lwt_watcher.notify watchers event

  let log_incoming_rejection ?timestamp point_info peer_id =
    log point_info ?timestamp (Rejecting_request peer_id)

end

module Pool_event = struct

  include Event

  let fold { Pool_info.events ; _ } ~init ~f = Ring.fold events ~init ~f

  let watch { Pool_info.watchers ; _ } = Lwt_watcher.create_stream watchers

end

module Pool_state = struct

  type 'data t = 'data Pool_info.state =
    | Requested of { cancel: Lwt_canceler.t }
    | Accepted of { current_peer_id: peer_id ;
                    cancel: Lwt_canceler.t }
    | Running of { data: 'data ;
                   current_peer_id: peer_id }
    | Disconnected
  type 'data state = 'data t

  let pp ppf = function
    | Requested _ ->
        Format.fprintf ppf "requested"
    | Accepted { current_peer_id ; _ } ->
        Format.fprintf ppf "accepted %a" peer_id_pp current_peer_id
    | Running { current_peer_id ; _ } ->
        Format.fprintf ppf "running %a" peer_id_pp current_peer_id
    | Disconnected ->
        Format.fprintf ppf "disconnected"

  let get { Pool_info.state ; _ } = state

  let is_disconnected { Pool_info.state ; _ } =
    match state with
    | Disconnected -> true
    | Requested _ | Accepted _ | Running _ -> false

  let set_requested ?timestamp point_info cancel =
    assert begin
      match point_info.Pool_info.state with
      | Requested _ -> true
      | Accepted _ | Running _ -> false
      | Disconnected -> true
    end ;
    point_info.state <- Requested { cancel } ;
    Pool_info.log point_info ?timestamp Outgoing_request

  let set_accepted
      ?(timestamp = Time.now ())
      point_info current_peer_id cancel =
    (* log_notice "SET_ACCEPTED %a@." P2p_point.pp point_info.point ; *)
    assert begin
      match point_info.Pool_info.state with
      | Accepted _ | Running _ -> false
      | Requested _ | Disconnected -> true
    end ;
    point_info.state <- Accepted { current_peer_id ; cancel } ;
    Pool_info.log point_info ~timestamp (Accepting_request current_peer_id)

  let set_running
      ?(timestamp = Time.now ())
      point_info peer_id data =
    assert begin
      match point_info.Pool_info.state with
      | Disconnected -> true (* request to unknown peer_id. *)
      | Running _ -> false
      | Accepted { current_peer_id ; _ } -> peer_id_equal peer_id current_peer_id
      | Requested _ -> true
    end ;
    point_info.state <- Running { data ; current_peer_id = peer_id } ;
    point_info.last_established_connection <- Some (peer_id, timestamp) ;
    Pool_info.log point_info ~timestamp (Connection_established peer_id)

  let set_greylisted timestamp point_info =
    point_info.Pool_info.greylisting_end <-
      Time.add
        timestamp
        (Int64.of_float point_info.Pool_info.greylisting_delay) ;
    point_info.greylisting_delay <-
      point_info.greylisting_delay *. point_info.greylisting.factor

  let set_disconnected
      ?(timestamp = Time.now ()) ?(requested = false) point_info =
    let event : Event.kind =
      match point_info.Pool_info.state with
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
    Pool_info.log point_info ~timestamp event

end
