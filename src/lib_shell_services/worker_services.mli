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
