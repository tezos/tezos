(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let (peer_id_arg : P2p_peer.Id.t RPC_arg.arg) =
  Crypto_box.Public_key_hash.rpc_arg

let point_arg =
  RPC_arg.make
    ~name:"point"
    ~descr:"A network point (ipv4:port or [ipv6]:port)."
    ~destruct:P2p_point.Id.of_string
    ~construct:P2p_point.Id.to_string
    ()

let versions =
  RPC_service.post_service
    ~description:"Supported network layer versions."
    ~query: RPC_query.empty
    ~input: Data_encoding.empty
    ~output: (Data_encoding.list P2p_version.encoding)
    ~error: Data_encoding.empty
    RPC_path.(root / "network" / "versions")

let stat =
  RPC_service.post_service
    ~description:"Global network bandwidth statistics in B/s."
    ~query: RPC_query.empty
    ~input: Data_encoding.empty
    ~output: P2p_stat.encoding
    ~error: Data_encoding.empty
    RPC_path.(root / "network" / "stat")

let events =
  RPC_service.post_service
    ~description:"Stream of all network events"
    ~query: RPC_query.empty
    ~input: Data_encoding.empty
    ~output: P2p_connection.Pool_event.encoding
    ~error: Data_encoding.empty
    RPC_path.(root / "network" / "log")

let connect =
  RPC_service.post_service
    ~description:"Connect to a peer"
    ~query: RPC_query.empty
    ~input: Data_encoding.(obj1 (dft "timeout" float 5.))
    ~output: (RPC_error.wrap Data_encoding.empty)
    ~error: Data_encoding.empty
    RPC_path.(root / "network" / "connect" /: point_arg)

let monitor_encoding = Data_encoding.(obj1 (dft "monitor" bool false))

module Connection = struct

  let list =
    RPC_service.post_service
      ~description:"List the running P2P connection."
      ~query: RPC_query.empty
      ~input: Data_encoding.empty
      ~output: (Data_encoding.list P2p_connection.Info.encoding)
      ~error: Data_encoding.empty
      RPC_path.(root / "network" / "connection")

  let info =
    RPC_service.post_service
      ~query: RPC_query.empty
      ~input: Data_encoding.empty
      ~output: (Data_encoding.option P2p_connection.Info.encoding)
      ~error: Data_encoding.empty
      ~description:"Details about the current P2P connection to the given peer."
      RPC_path.(root / "network" / "connection" /: peer_id_arg)

  let kick =
    RPC_service.post_service
      ~query: RPC_query.empty
      ~input: Data_encoding.(obj1 (req "wait" bool))
      ~output: Data_encoding.empty
      ~error: Data_encoding.empty
      ~description:"Forced close of the current P2P connection to the given peer."
      RPC_path.(root / "network" / "connection" /: peer_id_arg / "kick")

end

module Point = struct

  let info =
    RPC_service.post_service
      ~query: RPC_query.empty
      ~input: Data_encoding.empty
      ~output: (Data_encoding.option P2p_point.Info.encoding)
      ~error: Data_encoding.empty
      ~description: "Details about a given `IP:addr`."
      RPC_path.(root / "network" / "point" /: point_arg)

  let events =
    RPC_service.post_service
      ~query: RPC_query.empty
      ~input: monitor_encoding
      ~output: (Data_encoding.list
                  P2p_point.Pool_event.encoding)
      ~error: Data_encoding.empty
      ~description: "Monitor network events related to an `IP:addr`."
      RPC_path.(root / "network" / "point" /: point_arg / "log")

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
      ~error: Data_encoding.empty
      ~description:"List the pool of known `IP:port` \
                    used for establishing P2P connections ."
      RPC_path.(root / "network" / "point")

end

module Peer_id = struct

  let info =
    RPC_service.post_service
      ~query: RPC_query.empty
      ~input: Data_encoding.empty
      ~output: (Data_encoding.option P2p_peer.Info.encoding)
      ~error: Data_encoding.empty
      ~description:"Details about a given peer."
      RPC_path.(root / "network" / "peer_id" /: peer_id_arg)

  let events =
    RPC_service.post_service
      ~query: RPC_query.empty
      ~input: monitor_encoding
      ~output: (Data_encoding.list
                  P2p_peer.Pool_event.encoding)
      ~error: Data_encoding.empty
      ~description:"Monitor network events related to a given peer."
      RPC_path.(root / "network" / "peer_id" /: peer_id_arg / "log")

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
      ~error: Data_encoding.empty
      ~description:"List the peers the node ever met."
      RPC_path.(root / "network" / "peer_id")

end
