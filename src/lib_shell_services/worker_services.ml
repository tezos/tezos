(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Data_encoding

module Prevalidators = struct

  module S = struct

    let (chain_id_arg : Chain_id.t RPC_arg.t) =
      RPC_arg.like
        Chain_id.rpc_arg
        ~descr:"The chain identifier of whom the prevalidator is responsible."
        "chain_id"

    let list =
      RPC_service.post_service
        ~description:"Lists the Prevalidator workers and their status."
        ~query: RPC_query.empty
        ~input: empty
        ~output:
          (list
             (obj2
                (req "chain_id" Chain_id.encoding)
                (req "status" (Worker_types.worker_status_encoding  RPC_error.encoding))))
        RPC_path.(root / "workers" / "prevalidators")

    let state =
      let open Data_encoding in
      RPC_service.post_service
        ~description:"Introspect the state of a prevalidator worker."
        ~query: RPC_query.empty
        ~input: empty
        ~output:
          (Worker_types.full_status_encoding
             Prevalidator_worker_state.Request.encoding
             Prevalidator_worker_state.Event.encoding
             RPC_error.encoding)
        RPC_path.(root / "workers" / "prevalidators" /: Chain_id.rpc_arg )

  end

  open RPC_context
  let list ctxt = make_call S.list ctxt () () ()
  let state ctxt h = make_call1 S.state ctxt h () ()

end

module Block_validator = struct

  module S = struct

    let state =
      let open Data_encoding in
      RPC_service.post_service
        ~description:"Introspect the state of the block_validator worker."
        ~query: RPC_query.empty
        ~input: empty
        ~output:
          (Worker_types.full_status_encoding
             Block_validator_worker_state.Request.encoding
             Block_validator_worker_state.Event.encoding
             RPC_error.encoding)
        RPC_path.(root / "workers" / "block_validator")

  end

  open RPC_context
  let state ctxt = make_call S.state ctxt () () ()

end

module Peer_validators = struct

  module S = struct

    let (chain_id_arg : Chain_id.t RPC_arg.t) =
      RPC_arg.like
        Chain_id.rpc_arg
        ~descr:"The chain identifier the peer validator is associated to."
        "chain_id"

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
        ~input: empty
        ~output:
          (list
             (obj2
                (req "peer_id" P2p_peer.Id.encoding)
                (req "status" (Worker_types.worker_status_encoding RPC_error.encoding))))
        RPC_path.(root / "workers" / "peer_validators" /: chain_id_arg)

    let state =
      let open Data_encoding in
      RPC_service.post_service
        ~description:"Introspect the state of a peer validator worker."
        ~query: RPC_query.empty
        ~input: empty
        ~output:
          (Worker_types.full_status_encoding
             Peer_validator_worker_state.Request.encoding
             Peer_validator_worker_state.Event.encoding
             RPC_error.encoding)
        RPC_path.(root / "workers" / "peer_validators" /: chain_id_arg /: peer_id_arg)

  end

  open RPC_context
  let list ctxt n = make_call1 S.list ctxt n () ()
  let state ctxt n h = make_call2 S.state ctxt n h () ()

end

module Chain_validators = struct

  module S = struct
    let (chain_id_arg : Chain_id.t RPC_arg.t) =
      RPC_arg.like
        Chain_id.rpc_arg
        ~descr:"The chain identifier of whom the chain validator is responsible."
        "chain_id"

    let list =
      RPC_service.post_service
        ~description:"Lists the chain validator workers and their status."
        ~query: RPC_query.empty
        ~input: empty
        ~output:
          (list
             (obj2
                (req "chain_id" Chain_id.encoding)
                (req "status" (Worker_types.worker_status_encoding RPC_error.encoding))))
        RPC_path.(root / "workers" / "chain_validators")

    let state =
      let open Data_encoding in
      RPC_service.post_service
        ~description:"Introspect the state of a chain validator worker."
        ~query: RPC_query.empty
        ~input: empty
        ~output:
          (Worker_types.full_status_encoding
             Chain_validator_worker_state.Request.encoding
             Chain_validator_worker_state.Event.encoding
             RPC_error.encoding)
        RPC_path.(root / "workers" / "chain_validators" /: chain_id_arg )

  end

  open RPC_context
  let list ctxt = make_call S.list ctxt () () ()
  let state ctxt h = make_call1 S.state ctxt h () ()

end
