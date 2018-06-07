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

    let list =
      RPC_service.get_service
        ~description:"Lists the Prevalidator workers and their status."
        ~query: RPC_query.empty
        ~output:
          (list
             (obj2
                (req "chain_id" Chain_id.encoding)
                (req "status" (Worker_types.worker_status_encoding  RPC_error.encoding))))
        RPC_path.(root / "workers" / "prevalidators")

    let state =
      RPC_service.get_service
        ~description:"Introspect the state of a prevalidator worker."
        ~query: RPC_query.empty
        ~output:
          (Worker_types.full_status_encoding
             Prevalidator_worker_state.Request.encoding
             Prevalidator_worker_state.Event.encoding
             RPC_error.encoding)
        RPC_path.(root / "workers" / "prevalidators" /: Chain_services.chain_arg )

  end

  open RPC_context
  let list ctxt = make_call S.list ctxt () () ()
  let state ctxt h = make_call1 S.state ctxt h () ()

end

module Block_validator = struct

  module S = struct

    let state =
      RPC_service.get_service
        ~description:"Introspect the state of the block_validator worker."
        ~query: RPC_query.empty
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

    let list =
      RPC_service.get_service
        ~description:"Lists the peer validator workers and their status."
        ~query: RPC_query.empty
        ~output:
          (list
             (obj2
                (req "peer_id" P2p_peer.Id.encoding)
                (req "status" (Worker_types.worker_status_encoding RPC_error.encoding))))
        RPC_path.(root / "workers" / "chain_validators" /: Chain_services.chain_arg / "peers_validators" )

    let state =
      RPC_service.get_service
        ~description:"Introspect the state of a peer validator worker."
        ~query: RPC_query.empty
        ~output:
          (Worker_types.full_status_encoding
             Peer_validator_worker_state.Request.encoding
             Peer_validator_worker_state.Event.encoding
             RPC_error.encoding)
        RPC_path.(root / "workers" / "chain_validators" /: Chain_services.chain_arg / "peers_validators" /: P2p_peer.Id.rpc_arg)

  end

  open RPC_context
  let list ctxt n = make_call1 S.list ctxt n () ()
  let state ctxt n h = make_call2 S.state ctxt n h () ()

end

module Chain_validators = struct

  module S = struct

    let list =
      RPC_service.get_service
        ~description:"Lists the chain validator workers and their status."
        ~query: RPC_query.empty
        ~output:
          (list
             (obj2
                (req "chain_id" Chain_id.encoding)
                (req "status" (Worker_types.worker_status_encoding RPC_error.encoding))))
        RPC_path.(root / "workers" / "chain_validators")

    let state =
      RPC_service.get_service
        ~description:"Introspect the state of a chain validator worker."
        ~query: RPC_query.empty
        ~output:
          (Worker_types.full_status_encoding
             Chain_validator_worker_state.Request.encoding
             Chain_validator_worker_state.Event.encoding
             RPC_error.encoding)
        RPC_path.(root / "workers" / "chain_validators" /: Chain_services.chain_arg )

  end

  open RPC_context
  let list ctxt = make_call S.list ctxt () () ()
  let state ctxt h = make_call1 S.state ctxt h () ()

end
