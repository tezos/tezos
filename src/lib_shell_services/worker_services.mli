(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Prevalidators : sig

  open Prevalidator_worker_state

  val list :
    ([ `POST ],  unit,
     unit, unit, unit,
     (Net_id.t * Worker_types.worker_status) list) RPC_service.t

  val state :
    ([ `POST ], unit,
     unit * Net_id.t, unit, unit,
     (Request.view, Event.t) Worker_types.full_status) RPC_service.t

end

module Block_validator : sig

  open Block_validator_worker_state

  val state :
    ([ `POST ], unit,
     unit, unit, unit,
     (Request.view, Event.t) Worker_types.full_status) RPC_service.t

end

module Peer_validators : sig

  open Peer_validator_worker_state

  val list :
    ([ `POST ],  unit,
     unit * Net_id.t, unit, unit,
     (P2p_peer.Id.t * Worker_types.worker_status) list) RPC_service.t

  val state :
    ([ `POST ], unit,
     (unit * Net_id.t) * P2p_peer.Id.t, unit, unit,
     (Request.view, Event.t) Worker_types.full_status) RPC_service.t

end

module Net_validators : sig

  open Net_validator_worker_state

  val list :
    ([ `POST ],  unit,
     unit, unit, unit,
     (Net_id.t * Worker_types.worker_status) list) RPC_service.t

  val state :
    ([ `POST ], unit,
     unit * Net_id.t, unit, unit,
     (Request.view, Event.t) Worker_types.full_status) RPC_service.t

end
