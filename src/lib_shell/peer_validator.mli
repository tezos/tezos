(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type t

type limits = {
  new_head_request_timeout: Time.System.Span.t ;
  block_header_timeout: Time.System.Span.t ;
  block_operations_timeout: Time.System.Span.t ;
  protocol_timeout: Time.System.Span.t ;
  worker_limits: Worker_types.limits
}

val peer_id: t -> P2p_peer.Id.t
val bootstrapped: t -> bool
val current_head: t -> Block_header.t

val create:
  ?notify_new_block: (State.Block.t -> unit) ->
  ?notify_bootstrapped: (unit -> unit) ->
  ?notify_termination: (unit -> unit) ->
  limits ->
  Block_validator.t ->
  Distributed_db.chain_db -> P2p_peer.Id.t -> t tzresult Lwt.t
val shutdown: t -> unit Lwt.t

val notify_branch: t -> Block_locator.t -> unit
val notify_head: t -> Block_header.t -> unit

val running_workers: unit -> ((Chain_id.t * P2p_peer.Id.t) * t) list
val status: t -> Worker_types.worker_status

val current_request : t -> (Time.System.t * Time.System.t * Peer_validator_worker_state.Request.view) option
val last_events : t -> (Internal_event.level * Peer_validator_worker_state.Event.t list) list
