(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module S = struct

  let versions =
    RPC_service.post_service
      ~description:"Supported network layer versions."
      ~query: RPC_query.empty
      ~input: Data_encoding.empty
      ~output: (Data_encoding.list P2p_version.encoding)
      RPC_path.(root / "p2p" / "versions")

  let stat =
    RPC_service.post_service
      ~description:"Global network bandwidth statistics in B/s."
      ~query: RPC_query.empty
      ~input: Data_encoding.empty
      ~output: P2p_stat.encoding
      RPC_path.(root / "p2p" / "stat")

  let events =
    RPC_service.post_service
      ~description:"Stream of all network events"
      ~query: RPC_query.empty
      ~input: Data_encoding.empty
      ~output: P2p_connection.Pool_event.encoding
      RPC_path.(root / "p2p" / "log")

  let connect =
    RPC_service.post_service
      ~description:"Connect to a peer"
      ~query: RPC_query.empty
      ~input: Data_encoding.(obj1 (dft "timeout" float 5.))
      ~output: Data_encoding.empty
      RPC_path.(root / "p2p" / "connect" /: P2p_point.Id.rpc_arg)

end

open RPC_context
let stat ctxt = make_call S.stat ctxt () () ()
let versions ctxt = make_call S.versions ctxt () () ()
let events ctxt = make_streamed_call S.events ctxt () () ()
let connect ctxt ~timeout peer_id =
  make_call1 S.connect ctxt peer_id () timeout

let monitor_encoding = Data_encoding.(obj1 (dft "monitor" bool false))

module Connections = struct

  module S = struct

    let list =
      RPC_service.post_service
        ~description:"List the running P2P connection."
        ~query: RPC_query.empty
        ~input: Data_encoding.empty
        ~output: (Data_encoding.list P2p_connection.Info.encoding)
        RPC_path.(root / "p2p" / "connections")

    let info =
      RPC_service.post_service
        ~query: RPC_query.empty
        ~input: Data_encoding.empty
        ~output: P2p_connection.Info.encoding
        ~description:"Details about the current P2P connection to the given peer."
        RPC_path.(root / "p2p" / "connections" /: P2p_peer.Id.rpc_arg)

    let kick =
      RPC_service.post_service
        ~query: RPC_query.empty
        ~input: Data_encoding.(obj1 (req "wait" bool))
        ~output: Data_encoding.empty
        ~description:"Forced close of the current P2P connection to the given peer."
        RPC_path.(root / "p2p" / "connections" /: P2p_peer.Id.rpc_arg / "kick")

  end

  let list ctxt = make_call S.list ctxt () () ()
  let info ctxt peer_id = make_call1 S.info ctxt peer_id () ()
  let kick ctxt ?(wait = false) peer_id = make_call1 S.kick ctxt peer_id () wait

end

module Points = struct

  module S = struct

    let info =
      RPC_service.post_service
        ~query: RPC_query.empty
        ~input: Data_encoding.empty
        ~output: P2p_point.Info.encoding
        ~description: "Details about a given `IP:addr`."
        RPC_path.(root / "p2p" / "points" /: P2p_point.Id.rpc_arg)

    let events =
      RPC_service.post_service
        ~query: RPC_query.empty
        ~input: monitor_encoding
        ~output: (Data_encoding.list
                    P2p_point.Pool_event.encoding)
        ~description: "Monitor network events related to an `IP:addr`."
        RPC_path.(root / "p2p" / "points" /: P2p_point.Id.rpc_arg / "log")

    let list =
      let filter =
        let open Data_encoding in
        obj1 (dft "filter" (list P2p_point.State.encoding) []) in
      RPC_service.post_service
        ~query: RPC_query.empty
        ~input: filter
        ~output:
          Data_encoding.(list (tup2
                                 P2p_point.Id.encoding
                                 P2p_point.Info.encoding))
        ~description:"List the pool of known `IP:port` \
                      used for establishing P2P connections ."
        RPC_path.(root / "networks" / "point")

  end

  open RPC_context
  let info ctxt peer_id = make_call1 S.info ctxt peer_id () ()
  let events ctxt point =
    make_streamed_call S.events ctxt ((), point) () true
  let list ?(filter = []) ctxt = make_call S.list ctxt () () filter

end

module Peers = struct

  module S = struct

    let info =
      RPC_service.post_service
        ~query: RPC_query.empty
        ~input: Data_encoding.empty
        ~output: P2p_peer.Info.encoding
        ~description:"Details about a given peer."
        RPC_path.(root / "p2p" / "peers" /: P2p_peer.Id.rpc_arg)

    let events =
      RPC_service.post_service
        ~query: RPC_query.empty
        ~input: monitor_encoding
        ~output: (Data_encoding.list
                    P2p_peer.Pool_event.encoding)
        ~description:"Monitor network events related to a given peer."
        RPC_path.(root / "p2p" / "peers" /: P2p_peer.Id.rpc_arg / "log")

    let list =
      let filter =
        let open Data_encoding in
        obj1 (dft "filter" (list P2p_peer.State.encoding) []) in
      RPC_service.post_service
        ~query: RPC_query.empty
        ~input: filter
        ~output:
          Data_encoding.(list (tup2
                                 P2p_peer.Id.encoding
                                 P2p_peer.Info.encoding))
        ~description:"List the peers the node ever met."
        RPC_path.(root / "p2p" / "peers")

  end

  let info ctxt peer_id = make_call1 S.info ctxt peer_id () ()
  let events ctxt point =
    make_streamed_call S.events ctxt ((), point) () true
  let list ?(filter = []) ctxt = make_call S.list ctxt () () filter

end
