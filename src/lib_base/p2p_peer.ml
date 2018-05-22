(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Id = P2p_peer_id

module Table = Id.Table
module Map = Id.Map
module Set = Id.Set

module State = struct

  type t =
    | Accepted
    | Running
    | Disconnected

  let pp_digram ppf = function
    | Accepted -> Format.fprintf ppf "⚎"
    | Running -> Format.fprintf ppf "⚌"
    | Disconnected -> Format.fprintf ppf "⚏"

  let encoding =
    let open Data_encoding in
    string_enum [
      "accepted", Accepted ;
      "running", Running ;
      "disconnected", Disconnected ;
    ]

end

module Info = struct

  type 'conn_meta t = {
    score : float ;
    trusted : bool ;
    conn_metadata : 'conn_meta option;
    state : State.t ;
    id_point : P2p_connection.Id.t option ;
    stat : P2p_stat.t ;
    last_failed_connection : (P2p_connection.Id.t * Time.t) option ;
    last_rejected_connection : (P2p_connection.Id.t * Time.t) option ;
    last_established_connection : (P2p_connection.Id.t * Time.t) option ;
    last_disconnection : (P2p_connection.Id.t * Time.t) option ;
    last_seen : (P2p_connection.Id.t * Time.t) option ;
    last_miss : (P2p_connection.Id.t * Time.t) option ;
  }

  let encoding conn_metadata_encoding =
    let open Data_encoding in
    conv
      (fun (
         { score ; trusted ; conn_metadata ; state ; id_point ; stat ;
           last_failed_connection ; last_rejected_connection ;
           last_established_connection ; last_disconnection ;
           last_seen ; last_miss }) ->
         ((score, trusted, conn_metadata, state, id_point, stat),
          (last_failed_connection, last_rejected_connection,
           last_established_connection, last_disconnection,
           last_seen, last_miss)))
      (fun ((score, trusted, conn_metadata, state, id_point, stat),
            (last_failed_connection, last_rejected_connection,
             last_established_connection, last_disconnection,
             last_seen, last_miss)) ->
        { score ; trusted ; conn_metadata ; state ; id_point ; stat ;
          last_failed_connection ; last_rejected_connection ;
          last_established_connection ; last_disconnection ;
          last_seen ; last_miss })
      (merge_objs
         (obj6
            (req "score" float)
            (req "trusted" bool)
            (opt "conn_metadata" conn_metadata_encoding)
            (req "state" State.encoding)
            (opt "reachable_at" P2p_connection.Id.encoding)
            (req "stat" P2p_stat.encoding))
         (obj6
            (opt "last_failed_connection" (tup2 P2p_connection.Id.encoding Time.encoding))
            (opt "last_rejected_connection" (tup2 P2p_connection.Id.encoding Time.encoding))
            (opt "last_established_connection" (tup2 P2p_connection.Id.encoding Time.encoding))
            (opt "last_disconnection" (tup2 P2p_connection.Id.encoding Time.encoding))
            (opt "last_seen" (tup2 P2p_connection.Id.encoding Time.encoding))
            (opt "last_miss" (tup2 P2p_connection.Id.encoding Time.encoding))))

end

module Pool_event = struct

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
    point : P2p_connection.Id.t ;
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
         (req "addr" P2p_addr.encoding)
         (opt "port" int16))

end
