(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Data_encoding

module Prevalidators = struct

  let (net_id_arg : Net_id.t RPC_arg.t) =
    RPC_arg.make
      ~name:"net_id"
      ~descr:"The network identifier of whom the prevalidator is responsible."
      ~destruct:(fun s -> try
                    Ok (Net_id.of_b58check_exn s)
                  with Failure msg -> Error msg)
      ~construct:Net_id.to_b58check
      ()

  let list =
    RPC_service.post_service
      ~description:"Lists the Prevalidator workers and their status."
      ~query: RPC_query.empty
      ~error: Data_encoding.empty
      ~input: empty
      ~output:
        (list
           (obj2
              (req "net_id" Net_id.encoding)
              (req "status" (Worker_types.worker_status_encoding  RPC_error.encoding))))
      RPC_path.(root / "workers" / "prevalidators")

  let state =
    let open Data_encoding in
    RPC_service.post_service
      ~description:"Introspect the state of a prevalidator worker."
      ~query: RPC_query.empty
      ~error: Data_encoding.empty
      ~input: empty
      ~output:
        (Worker_types.full_status_encoding
           Prevalidator_worker_state.Request.encoding
           (Prevalidator_worker_state.Event.encoding RPC_error.encoding)
           RPC_error.encoding)
      RPC_path.(root / "workers" / "prevalidators" /: net_id_arg )

end

module Block_validator = struct

  let state =
    let open Data_encoding in
    RPC_service.post_service
      ~description:"Introspect the state of the block_validator worker."
      ~query: RPC_query.empty
      ~error: Data_encoding.empty
      ~input: empty
      ~output:
        (Worker_types.full_status_encoding
           Block_validator_worker_state.Request.encoding
           (Block_validator_worker_state.Event.encoding RPC_error.encoding)
           RPC_error.encoding)
      RPC_path.(root / "workers" / "block_validator")

end

module Peer_validators = struct

  let (net_id_arg : Net_id.t RPC_arg.t) =
    RPC_arg.make
      ~name:"net_id"
      ~descr:"The network identifier the peer validator is associated to."
      ~destruct:(fun s -> try
                    Ok (Net_id.of_b58check_exn s)
                  with Failure msg -> Error msg)
      ~construct:Net_id.to_b58check
      ()

  let (peer_id_arg : P2p_peer.Id.t RPC_arg.t) =
    RPC_arg.make
      ~name:"peer_id"
      ~descr:"The peer identifier of whom the prevalidator is responsible."
      ~destruct:(fun s -> try
                    Ok (P2p_peer.Id.of_b58check_exn s)
                  with Failure msg -> Error msg)
      ~construct:P2p_peer.Id.to_b58check
      ()

  let list =
    RPC_service.post_service
      ~description:"Lists the peer validator workers and their status."
      ~query: RPC_query.empty
      ~error: Data_encoding.empty
      ~input: empty
      ~output:
        (list
           (obj2
              (req "peer_id" P2p_peer.Id.encoding)
              (req "status" (Worker_types.worker_status_encoding RPC_error.encoding))))
      RPC_path.(root / "workers" / "peer_validators" /: net_id_arg)

  let state =
    let open Data_encoding in
    RPC_service.post_service
      ~description:"Introspect the state of a peer validator worker."
      ~query: RPC_query.empty
      ~error: Data_encoding.empty
      ~input: empty
      ~output:
        (Worker_types.full_status_encoding
           Peer_validator_worker_state.Request.encoding
           (Peer_validator_worker_state.Event.encoding RPC_error.encoding)
           RPC_error.encoding)
      RPC_path.(root / "workers" / "peer_validators" /: net_id_arg /: peer_id_arg)

end

module Net_validators = struct

  let (net_id_arg : Net_id.t RPC_arg.t) =
    RPC_arg.make
      ~name:"net_id"
      ~descr:"The network identifier of whom the net validator is responsible."
      ~destruct:(fun s -> try
                    Ok (Net_id.of_b58check_exn s)
                  with Failure msg -> Error msg)
      ~construct:Net_id.to_b58check
      ()

  let list =
    RPC_service.post_service
      ~description:"Lists the net validator workers and their status."
      ~query: RPC_query.empty
      ~error: Data_encoding.empty
      ~input: empty
      ~output:
        (list
           (obj2
              (req "net_id" Net_id.encoding)
              (req "status" (Worker_types.worker_status_encoding RPC_error.encoding))))
      RPC_path.(root / "workers" / "net_validators")

  let state =
    let open Data_encoding in
    RPC_service.post_service
      ~description:"Introspect the state of a net validator worker."
      ~query: RPC_query.empty
      ~error: Data_encoding.empty
      ~input: empty
      ~output:
        (Worker_types.full_status_encoding
           Net_validator_worker_state.Request.encoding
           (Net_validator_worker_state.Event.encoding RPC_error.encoding)
           RPC_error.encoding)
      RPC_path.(root / "workers" / "net_validators" /: net_id_arg )

end

end
