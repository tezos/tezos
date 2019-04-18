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

(** Tezos Shell - Prevalidation of pending operations (a.k.a Mempool) *)

(** The prevalidator is in charge of the "mempool" (a.k.a. the
    set of known not-invalid-for-sure operations that are not yet
    included in the blockchain).

    The prevalidator also maintains a sorted subset of the mempool that
    might correspond to a valid block on top of the current head. The
    "in-progress" context produced by the application of those
    operations is called the (pre)validation context.

    Before including an operation into the mempool, the prevalidation
    worker tries to append the operation the prevalidation context. If
    the operation is (strongly) refused, it will not be added into the
    mempool and then it will be ignored by the node and never
    broadcast. If the operation is only "branch_refused" or
    "branch_delayed", the operation won't be appended in the
    prevalidation context, but still broadcast.

*)



(** An (abstract) prevalidator context. Separate prevalidator contexts should be
 * used for separate chains (e.g., mainchain vs testchain). *)
type t

type limits = {
  max_refused_operations : int ;
  operation_timeout : Time.System.Span.t ;
  worker_limits : Worker_types.limits ;
}

(** Creates/tear-down a new prevalidator context. *)
val create:
  limits ->
  (module Registered_protocol.T) ->
  Distributed_db.chain_db ->
  t tzresult Lwt.t
val shutdown: t -> unit Lwt.t

(** Notify the prevalidator that the identified peer has sent a bunch of
 * operations relevant to the specified context. *)
val notify_operations: t -> P2p_peer.Id.t -> Mempool.t -> unit Lwt.t

(** Notify the prevalidator worker of a new injected operation. *)
val inject_operation: t -> Operation.t -> unit tzresult Lwt.t

(** Notify the prevalidator that a new head has been selected. *)
val flush: t -> Block_hash.t -> unit tzresult Lwt.t

(** Returns the timestamp of the prevalidator worker, that is the timestamp of the last
    reset of the prevalidation context *)
val timestamp: t -> Time.System.t

(** Returns the fitness of the current prevalidation context *)
val fitness: t -> Fitness.t Lwt.t

(** Returns the list of valid operations known to this prevalidation worker *)
val operations: t -> (error Preapply_result.t * Operation.t Operation_hash.Map.t)

(** Returns the list of pending operations known to this prevalidation worker *)
val pending: ?block:State.Block.t -> t -> Operation.t Operation_hash.Map.t Lwt.t

(** Returns the list of prevalidation contexts running and their associated chain *)
val running_workers: unit -> (Chain_id.t * Protocol_hash.t * t) list

(** Two functions that are useful for managing the prevalidator's transition
 * from one protocol to the next. *)

(** Returns the hash of the protocol the prevalidator was instantiated with *)
val protocol_hash: t -> Protocol_hash.t

(** Returns the parameters the prevalidator was created with. *)
val parameters: t -> limits * Distributed_db.chain_db

(** Worker status and events *)

(* None indicates the there are no workers for the current protocol. *)
val status: t -> Worker_types.worker_status
val pending_requests : t -> (Time.System.t * Prevalidator_worker_state.Request.view) list
val current_request : t -> (Time.System.t * Time.System.t * Prevalidator_worker_state.Request.view) option
val last_events : t -> (Internal_event.level * Prevalidator_worker_state.Event.t list) list

val rpc_directory : t option RPC_directory.t
