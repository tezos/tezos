(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open RPC_context

module Prevalidators : sig

  open Prevalidator_worker_state

  val list:
    #simple -> (Chain_id.t * Worker_types.worker_status) list tzresult Lwt.t
  val state:
    #simple -> Chain_services.chain -> (Request.view, Event.t) Worker_types.full_status tzresult Lwt.t

  module S : sig

    val list :
      ([ `GET ],  unit,
       unit, unit, unit,
       (Chain_id.t * Worker_types.worker_status) list) RPC_service.t

    val state :
      ([ `GET ], unit,
       unit * Chain_services.chain, unit, unit,
       (Request.view, Event.t) Worker_types.full_status) RPC_service.t

  end

end

module Block_validator : sig

  open Block_validator_worker_state

  val state:
    #simple -> (Request.view, Event.t) Worker_types.full_status tzresult Lwt.t

  module S : sig

    val state :
      ([ `GET ], unit,
       unit, unit, unit,
       (Request.view, Event.t) Worker_types.full_status) RPC_service.t

  end

end

module Peer_validators : sig

  open Peer_validator_worker_state

  val list:
    #simple -> Chain_services.chain ->
    (P2p_peer.Id.t * Worker_types.worker_status) list tzresult Lwt.t

  val state:
    #simple ->
    Chain_services.chain -> P2p_peer.Id.t -> (Request.view, Event.t) Worker_types.full_status tzresult Lwt.t

  module S : sig

    val list :
      ([ `GET ],  unit,
       unit * Chain_services.chain, unit, unit,
       (P2p_peer.Id.t * Worker_types.worker_status) list) RPC_service.t

    val state :
      ([ `GET ], unit,
       (unit * Chain_services.chain) * P2p_peer.Id.t, unit, unit,
       (Request.view, Event.t) Worker_types.full_status) RPC_service.t

  end

end

module Chain_validators : sig

  open Chain_validator_worker_state

  val list:
    #simple -> (Chain_id.t * Worker_types.worker_status) list tzresult Lwt.t
  val state:
    #simple -> Chain_services.chain -> (Request.view, Event.t) Worker_types.full_status tzresult Lwt.t

  module S : sig

    val list :
      ([ `GET ],  unit,
       unit, unit, unit,
       (Chain_id.t * Worker_types.worker_status) list) RPC_service.t

    val state :
      ([ `GET ], unit,
       unit * Chain_services.chain, unit, unit,
       (Request.view, Event.t) Worker_types.full_status) RPC_service.t

  end

end
