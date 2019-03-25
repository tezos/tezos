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

module Id = P2p_peer_id

module Table = Id.Table
module Map = Id.Map
module Set = Id.Set

module Filter = struct

  type t =
    | Accepted
    | Running
    | Disconnected

  let rpc_arg =
    RPC_arg.make
      ~name:"p2p.point.state_filter"
      ~destruct:(function
          | "accepted" -> Ok Accepted
          | "running" -> Ok Running
          | "disconnected" -> Ok Disconnected
          | s -> Error (Format.asprintf "Invalid state: %s" s))
      ~construct:(function
          | Accepted -> "accepted"
          | Running -> "running"
          | Disconnected -> "disconnected")
      ()

end

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

  let raw_filter (f : Filter.t) (s : t) =
    match f, s with
    | Accepted, Accepted -> true
    | Accepted, (Running | Disconnected)
    | (Running | Disconnected), Accepted -> false
    | Running, Running -> true
    | Disconnected, Disconnected -> true
    | Running, Disconnected
    | Disconnected, Running -> false

  let filter filters state =
    List.exists (fun f -> raw_filter f state) filters

end

module Info = struct

  type ('peer_meta, 'conn_meta) t = {
    score : float ;
    trusted : bool ;
    conn_metadata : 'conn_meta option ;
    peer_metadata : 'peer_meta ;
    state : State.t ;
    id_point : P2p_connection.Id.t option ;
    stat : P2p_stat.t ;
    last_failed_connection : (P2p_connection.Id.t * Time.System.t) option ;
    last_rejected_connection : (P2p_connection.Id.t * Time.System.t) option ;
    last_established_connection : (P2p_connection.Id.t * Time.System.t) option ;
    last_disconnection : (P2p_connection.Id.t * Time.System.t) option ;
    last_seen : (P2p_connection.Id.t * Time.System.t) option ;
    last_miss : (P2p_connection.Id.t * Time.System.t) option ;
  }

  let encoding peer_metadata_encoding conn_metadata_encoding =
    let open Data_encoding in
    conv
      (fun (
         { score ; trusted ; conn_metadata ; peer_metadata ;
           state ; id_point ; stat ;
           last_failed_connection ; last_rejected_connection ;
           last_established_connection ; last_disconnection ;
           last_seen ; last_miss }) ->
         ((score, trusted, conn_metadata, peer_metadata,
           state, id_point, stat),
          (last_failed_connection, last_rejected_connection,
           last_established_connection, last_disconnection,
           last_seen, last_miss)))
      (fun ((score, trusted, conn_metadata, peer_metadata,
             state, id_point, stat),
            (last_failed_connection, last_rejected_connection,
             last_established_connection, last_disconnection,
             last_seen, last_miss)) ->
        { score ; trusted ; conn_metadata ; peer_metadata ;
          state ; id_point ; stat ;
          last_failed_connection ; last_rejected_connection ;
          last_established_connection ; last_disconnection ;
          last_seen ; last_miss })
      (merge_objs
         (obj7
            (req "score" float)
            (req "trusted" bool)
            (opt "conn_metadata" conn_metadata_encoding)
            (req "peer_metadata" peer_metadata_encoding)
            (req "state" State.encoding)
            (opt "reachable_at" P2p_connection.Id.encoding)
            (req "stat" P2p_stat.encoding))
         (obj6
            (opt "last_failed_connection" (tup2 P2p_connection.Id.encoding Time.System.encoding))
            (opt "last_rejected_connection" (tup2 P2p_connection.Id.encoding Time.System.encoding))
            (opt "last_established_connection" (tup2 P2p_connection.Id.encoding Time.System.encoding))
            (opt "last_disconnection" (tup2 P2p_connection.Id.encoding Time.System.encoding))
            (opt "last_seen" (tup2 P2p_connection.Id.encoding Time.System.encoding))
            (opt "last_miss" (tup2 P2p_connection.Id.encoding Time.System.encoding))))

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
    timestamp : Time.System.t ;
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
         (req "timestamp" Time.System.encoding)
         (req "addr" P2p_addr.encoding)
         (opt "port" uint16))

end
