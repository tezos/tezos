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

let wait_query =
  let open RPC_query in
  query (fun wait -> object
          method wait = wait
        end)
  |+ flag "wait" (fun t -> t#wait)
  |> seal

let monitor_query =
  let open RPC_query in
  query (fun monitor -> object
          method monitor = monitor
        end)
  |+ flag "monitor" (fun t -> t#monitor)
  |> seal

let timeout_query =
  let open RPC_query in
  query (fun timeout -> object
          method timeout = timeout
        end)
  |+ field
    "timeout"
    Time.System.Span.rpc_arg
    (Time.System.Span.of_seconds_exn 10.)
    (fun t -> t#timeout)
  |> seal

module S = struct

  let self =
    RPC_service.get_service
      ~description:"Return the node's peer id"
      ~query: RPC_query.empty
      ~output: P2p_peer.Id.encoding
      RPC_path.(root / "network" / "self")

  let versions =
    RPC_service.get_service
      ~description:"Supported network layer versions."
      ~query: RPC_query.empty
      ~output: (Data_encoding.list Network_version.encoding)
      RPC_path.(root / "network" / "versions")

  let stat =
    RPC_service.get_service
      ~description:"Global network bandwidth statistics in B/s."
      ~query: RPC_query.empty
      ~output: P2p_stat.encoding
      RPC_path.(root / "network" / "stat")

  let events =
    RPC_service.get_service
      ~description:"Stream of all network events"
      ~query: RPC_query.empty
      ~output: P2p_connection.Pool_event.encoding
      RPC_path.(root / "network" / "log")

  let connect =
    RPC_service.put_service
      ~description:"Connect to a peer"
      ~query: timeout_query
      ~input: Data_encoding.empty
      ~output: Data_encoding.empty
      RPC_path.(root / "network" / "points" /: P2p_point.Id.rpc_arg)

end

open RPC_context
let self ctxt = make_call S.self ctxt () () ()
let stat ctxt = make_call S.stat ctxt () () ()
let versions ctxt = make_call S.versions ctxt () () ()
let events ctxt = make_streamed_call S.events ctxt () () ()
let connect ctxt ~timeout peer_id =
  make_call1 S.connect ctxt peer_id (object method timeout = timeout end) ()

module Connections = struct

  type connection_info = Connection_metadata.t P2p_connection.Info.t

  let connection_info_encoding =
    P2p_connection.Info.encoding Connection_metadata.encoding

  module S = struct

    let list =
      RPC_service.get_service
        ~description:"List the running P2P connection."
        ~query: RPC_query.empty
        ~output: (Data_encoding.list connection_info_encoding)
        RPC_path.(root / "network" / "connections")

    let info =
      RPC_service.get_service
        ~query: RPC_query.empty
        ~output: connection_info_encoding
        ~description:"Details about the current P2P connection to the given peer."
        RPC_path.(root / "network" / "connections" /: P2p_peer.Id.rpc_arg)

    let kick =
      RPC_service.delete_service
        ~query: wait_query
        ~output: Data_encoding.empty
        ~description:"Forced close of the current P2P connection to the given peer."
        RPC_path.(root / "network" / "connections" /: P2p_peer.Id.rpc_arg)

  end

  let list ctxt = make_call S.list ctxt () () ()
  let info ctxt peer_id = make_call1 S.info ctxt peer_id () ()
  let kick ctxt ?(wait = false) peer_id =
    make_call1 S.kick ctxt peer_id (object method wait = wait end) ()

end

module Points = struct

  module S = struct

    let info =
      RPC_service.get_service
        ~query: RPC_query.empty
        ~output: P2p_point.Info.encoding
        ~description: "Details about a given `IP:addr`."
        RPC_path.(root / "network" / "points" /: P2p_point.Id.rpc_arg)

    let events =
      RPC_service.get_service
        ~query: monitor_query
        ~output: (Data_encoding.list
                    P2p_point.Pool_event.encoding)
        ~description: "Monitor network events related to an `IP:addr`."
        RPC_path.(root / "network" / "points" /: P2p_point.Id.rpc_arg / "log")

    let list =
      let filter_query =
        let open RPC_query in
        query (fun filters -> object
                method filters = filters
              end)
        |+ multi_field "filter" P2p_point.Filter.rpc_arg (fun t -> t#filters)
        |> seal in
      RPC_service.get_service
        ~query: filter_query
        ~output:
          Data_encoding.(list (tup2
                                 P2p_point.Id.encoding
                                 P2p_point.Info.encoding))
        ~description:"List the pool of known `IP:port` \
                      used for establishing P2P connections."
        RPC_path.(root / "network" / "points")

    let ban =
      RPC_service.get_service
        ~query: RPC_query.empty
        ~output: Data_encoding.empty
        ~description:"Blacklist the given address and remove it from the \
                      whitelist if present."
        RPC_path.(root / "network" / "points" /: P2p_point.Id.rpc_arg / "ban" )

    let unban =
      RPC_service.get_service
        ~query: RPC_query.empty
        ~output: Data_encoding.empty
        ~description:"Remove an address from the blacklist."
        RPC_path.(root / "network" / "points" /: P2p_point.Id.rpc_arg / "unban" )

    let trust =
      RPC_service.get_service
        ~query: RPC_query.empty
        ~output: Data_encoding.empty
        ~description:"Trust a given address permanently and remove it \
                      from the blacklist if present. Connections from \
                      this address can still be closed on \
                      authentication if the peer is greylisted."
        RPC_path.(root / "network" / "points" /: P2p_point.Id.rpc_arg / "trust" )

    let untrust =
      RPC_service.get_service
        ~query: RPC_query.empty
        ~output: Data_encoding.empty
        ~description:"Remove an address from the whitelist."
        RPC_path.(root / "network" / "points" /: P2p_point.Id.rpc_arg / "untrust" )

    let banned =
      RPC_service.get_service
        ~query: RPC_query.empty
        ~output: Data_encoding.bool
        ~description:"Check is a given address is blacklisted or \
                      greylisted."
        RPC_path.(root / "network" / "points" /: P2p_point.Id.rpc_arg / "banned" )

  end

  open RPC_context
  let info ctxt peer_id = make_call1 S.info ctxt peer_id () ()
  let events ctxt point =
    make_streamed_call S.events ctxt ((), point)
      (object method monitor = true end) ()
  let list ?(filter = []) ctxt = make_call S.list ctxt ()
      (object method filters = filter end) ()
  let ban ctxt peer_id = make_call1 S.ban ctxt peer_id () ()
  let unban ctxt peer_id = make_call1 S.unban ctxt peer_id () ()
  let trust ctxt peer_id = make_call1 S.trust ctxt peer_id () ()
  let untrust ctxt peer_id = make_call1 S.untrust ctxt peer_id () ()
  let banned ctxt peer_id = make_call1 S.banned ctxt peer_id () ()

end

module Peers = struct

  module S = struct

    let info =
      RPC_service.get_service
        ~query: RPC_query.empty
        ~output: (P2p_peer.Info.encoding Peer_metadata.encoding
                    Connection_metadata.encoding)
        ~description:"Details about a given peer."
        RPC_path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg)

    let events =
      RPC_service.get_service
        ~query: monitor_query
        ~output: (Data_encoding.list
                    P2p_peer.Pool_event.encoding)
        ~description:"Monitor network events related to a given peer."
        RPC_path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg / "log")

    let list =
      let filter =
        let open RPC_query in
        query (fun filters -> object
                method filters = filters
              end)
        |+ multi_field "filter" P2p_peer.Filter.rpc_arg (fun t -> t#filters)
        |> seal in
      RPC_service.get_service
        ~query: filter
        ~output:
          Data_encoding.(list (tup2
                                 P2p_peer.Id.encoding
                                 (P2p_peer.Info.encoding Peer_metadata.encoding
                                    Connection_metadata.encoding)))
        ~description:"List the peers the node ever met."
        RPC_path.(root / "network" / "peers")

    let ban =
      RPC_service.get_service
        ~query: RPC_query.empty
        ~output: Data_encoding.empty
        ~description:"Blacklist the given peer and remove it from the \
                      whitelist if present."
        RPC_path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg / "ban" )

    let unban =
      RPC_service.get_service
        ~query: RPC_query.empty
        ~output: Data_encoding.empty
        ~description:"Remove the given peer from the blacklist."
        RPC_path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg / "unban" )

    let trust =
      RPC_service.get_service
        ~query: RPC_query.empty
        ~output: Data_encoding.empty
        ~description:"Whitelist a given peer permanently and remove it \
                      from the blacklist if present. The peer cannot \
                      be blocked (but its host IP still can)."
        RPC_path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg / "trust" )

    let untrust =
      RPC_service.get_service
        ~query: RPC_query.empty
        ~output: Data_encoding.empty
        ~description:"Remove a given peer from the whitelist."
        RPC_path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg / "untrust" )

    let banned =
      RPC_service.get_service
        ~query: RPC_query.empty
        ~output: Data_encoding.bool
        ~description:"Check if a given peer is blacklisted or \
                      greylisted."
        RPC_path.(root / "network" / "peers" /: P2p_peer.Id.rpc_arg / "banned" )

  end

  let info ctxt peer_id = make_call1 S.info ctxt peer_id () ()
  let events ctxt point =
    make_streamed_call S.events ctxt ((), point)
      (object method monitor = true end) ()
  let list ?(filter = []) ctxt =
    make_call S.list ctxt () (object method filters = filter end) ()
  let ban ctxt point_id = make_call1 S.ban ctxt point_id () ()
  let unban ctxt point_id = make_call1 S.unban ctxt point_id () ()
  let trust ctxt point_id = make_call1 S.trust ctxt point_id () ()
  let untrust ctxt point_id = make_call1 S.untrust ctxt point_id () ()
  let banned ctxt point_id = make_call1 S.banned ctxt point_id () ()

end

module ACL = struct

  module S = struct

    let clear =
      RPC_service.get_service
        ~query: RPC_query.empty
        ~output: Data_encoding.empty
        ~description:"Clear all greylists tables."
        RPC_path.(root / "network" / "greylist" / "clear" )

  end

  let clear ctxt = make_call S.clear ctxt () ()

end
