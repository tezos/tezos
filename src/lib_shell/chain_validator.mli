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
  bootstrap_threshold: int ;
  worker_limits: Worker_types.limits
}

val create:
  ?max_child_ttl:int ->
  start_prevalidator:bool ->
  start_testchain:bool ->
  active_chains: t Chain_id.Table.t ->
  Peer_validator.limits ->
  Prevalidator.limits ->
  Block_validator.t ->
  State.Block.t Lwt_watcher.input ->
  (Chain_id.t * bool) Lwt_watcher.input ->
  Distributed_db.t ->
  State.Chain.t ->
  limits ->
  t tzresult Lwt.t

val bootstrapped: t -> unit Lwt.t

val chain_id: t -> Chain_id.t
val chain_state: t -> State.Chain.t
val prevalidator: t -> Prevalidator.t option
val chain_db: t -> Distributed_db.chain_db
val child: t -> t option

val validate_block:
  t ->
  ?force:bool ->
  Block_hash.t -> Block_header.t -> Operation.t list list ->
  State.Block.t option tzresult Lwt.t

val shutdown: t -> unit Lwt.t

val valid_block_watcher: t -> State.Block.t Lwt_stream.t * Lwt_watcher.stopper
val new_head_watcher: t -> State.Block.t Lwt_stream.t * Lwt_watcher.stopper

val running_workers: unit -> (Chain_id.t * t) list
val status: t -> Worker_types.worker_status

val pending_requests : t -> (Time.System.t * Chain_validator_worker_state.Request.view) list
val current_request : t -> (Time.System.t * Time.System.t * Chain_validator_worker_state.Request.view) option
val last_events : t -> (Internal_event.level * Chain_validator_worker_state.Event.t list) list
