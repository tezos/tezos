(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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

  let rpc_arg =
    RPC_arg.make
      ~name:"point"
      ~descr:"A network point (ipv4:port or [ipv6]:port)."
      ~destruct:of_string
      ~construct:to_string
      ()

end

module Map = Map.Make (Id)
module Set = Set.Make (Id)
module Table = Hashtbl.Make (Id)

module Filter = struct

  type t =
    | Requested
    | Accepted
    | Running
    | Disconnected

  let rpc_arg =
    RPC_arg.make
      ~name:"p2p.point.state_filter"
      ~destruct:(function
          | "requested" -> Ok Requested
          | "accepted" -> Ok Accepted
          | "running" -> Ok Running
          | "disconnected" -> Ok Disconnected
          | s -> Error (Format.asprintf "Invalid state: %s" s))
      ~construct:(function
          | Requested -> "requested"
          | Accepted -> "accepted"
          | Running -> "running"
          | Disconnected -> "disconnected")
      ()

end

module State = struct

  type t =
    | Requested
    | Accepted of P2p_peer_id.t
    | Running of P2p_peer_id.t
    | Disconnected

  let of_p2p_peer_id = function
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
      case (Tag 0)
        ~name:"requested"
        (branch_encoding "requested" empty)
        (function Requested -> Some () | _ -> None)
        (fun () -> Requested) ;
      case (Tag 1)
        ~name:"accepted"
        (branch_encoding "accepted"
           (obj1 (req "p2p_peer_id" P2p_peer_id.encoding)))
        (function Accepted p2p_peer_id -> Some p2p_peer_id | _ -> None)
        (fun p2p_peer_id -> Accepted p2p_peer_id) ;
      case (Tag 2)
        ~name:"running"
        (branch_encoding "running"
           (obj1 (req "p2p_peer_id" P2p_peer_id.encoding)))
        (function Running p2p_peer_id -> Some p2p_peer_id | _ -> None)
        (fun p2p_peer_id -> Running p2p_peer_id) ;
      case (Tag 3)
        ~name:"disconnected"
        (branch_encoding "disconnected" empty)
        (function Disconnected -> Some () | _ -> None)
        (fun () -> Disconnected) ;
    ]

  let raw_filter (f : Filter.t) (s : t) =
    match f, s with
    | Requested, Requested -> true
    | Requested, (Accepted _ | Running _ | Disconnected)
    | (Accepted | Running | Disconnected), Requested -> false
    | Accepted, Accepted _-> true
    | Accepted, (Running _ | Disconnected)
    | (Running | Disconnected), Accepted _ -> false
    | Running, Running _ -> true
    | Disconnected, Disconnected -> true
    | Running, Disconnected
    | Disconnected, Running _ -> false

  let filter filters state =
    List.exists (fun f -> raw_filter f state) filters

end

module Info = struct

  type t = {
    trusted : bool ;
    greylisted_until : Time.t ;
    state : State.t ;
    last_failed_connection : Time.t option ;
    last_rejected_connection : (P2p_peer_id.t * Time.t) option ;
    last_established_connection : (P2p_peer_id.t * Time.t) option ;
    last_disconnection : (P2p_peer_id.t * Time.t) option ;
    last_seen : (P2p_peer_id.t * Time.t) option ;
    last_miss : Time.t option ;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun { trusted ; greylisted_until ; state ;
             last_failed_connection ; last_rejected_connection ;
             last_established_connection ; last_disconnection ;
             last_seen ; last_miss } ->
        let p2p_peer_id = State.of_p2p_peer_id state in
        (trusted, greylisted_until, state, p2p_peer_id,
         last_failed_connection, last_rejected_connection,
         last_established_connection, last_disconnection,
         last_seen, last_miss))
      (fun (trusted, greylisted_until, state, p2p_peer_id,
            last_failed_connection, last_rejected_connection,
            last_established_connection, last_disconnection,
            last_seen, last_miss) ->
        let state = State.of_peerid_state state p2p_peer_id in
        { trusted ; greylisted_until ; state ;
          last_failed_connection ; last_rejected_connection ;
          last_established_connection ; last_disconnection ;
          last_seen ; last_miss })
      (obj10
         (req "trusted" bool)
         (dft "greylisted_until" Time.encoding Time.epoch)
         (req "state" State.encoding)
         (opt "p2p_peer_id" P2p_peer_id.encoding)
         (opt "last_failed_connection" Time.encoding)
         (opt "last_rejected_connection" (tup2 P2p_peer_id.encoding Time.encoding))
         (opt "last_established_connection" (tup2 P2p_peer_id.encoding Time.encoding))
         (opt "last_disconnection" (tup2 P2p_peer_id.encoding Time.encoding))
         (opt "last_seen" (tup2 P2p_peer_id.encoding Time.encoding))
         (opt "last_miss" Time.encoding))

end

module Pool_event = struct

  type kind =
    | Outgoing_request
    | Accepting_request of P2p_peer_id.t
    | Rejecting_request of P2p_peer_id.t
    | Request_rejected of P2p_peer_id.t option
    | Connection_established of P2p_peer_id.t
    | Disconnection of P2p_peer_id.t
    | External_disconnection of P2p_peer_id.t

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
                      (obj1 (req "p2p_peer_id" P2p_peer_id.encoding)))
        (function Accepting_request p2p_peer_id -> Some p2p_peer_id | _ -> None)
        (fun p2p_peer_id -> Accepting_request p2p_peer_id) ;
      case (Tag 2) (branch_encoding "rejecting_request"
                      (obj1 (req "p2p_peer_id" P2p_peer_id.encoding)))
        (function Rejecting_request p2p_peer_id -> Some p2p_peer_id | _ -> None)
        (fun p2p_peer_id -> Rejecting_request p2p_peer_id) ;
      case (Tag 3) (branch_encoding "request_rejected"
                      (obj1 (opt "p2p_peer_id" P2p_peer_id.encoding)))
        (function Request_rejected p2p_peer_id -> Some p2p_peer_id | _ -> None)
        (fun p2p_peer_id -> Request_rejected p2p_peer_id) ;
      case (Tag 4) (branch_encoding "rejecting_request"
                      (obj1 (req "p2p_peer_id" P2p_peer_id.encoding)))
        (function Connection_established p2p_peer_id -> Some p2p_peer_id | _ -> None)
        (fun p2p_peer_id -> Connection_established p2p_peer_id) ;
      case (Tag 5) (branch_encoding "rejecting_request"
                      (obj1 (req "p2p_peer_id" P2p_peer_id.encoding)))
        (function Disconnection p2p_peer_id -> Some p2p_peer_id | _ -> None)
        (fun p2p_peer_id -> Disconnection p2p_peer_id) ;
      case (Tag 6) (branch_encoding "rejecting_request"
                      (obj1 (req "p2p_peer_id" P2p_peer_id.encoding)))
        (function External_disconnection p2p_peer_id -> Some p2p_peer_id | _ -> None)
        (fun p2p_peer_id -> External_disconnection p2p_peer_id) ;
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
