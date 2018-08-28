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

(** Tezos Shell - Prevalidation of pending operations (a.k.a Mempool) *)

(** The prevalidation worker is in charge of the "mempool" (a.k.a. the
    set of known not-invalid-for-sure operations that are not yet
    included in the blockchain).

    The worker also maintains a sorted subset of the mempool that
    might correspond to a valid block on top of the current head. The
    "in-progress" context produced by the application of those
    operations is called the (pre)validation context.

    Before to include an operation into the mempool, the prevalidation
    worker tries to append the operation the prevalidation context. If
    the operation is (strongly) refused, it will not be added into the
    mempool and then it will be ignored by the node and never
    broadcasted. If the operation is only "branch_refused" or
    "branch_delayed", the operation won't be appended in the
    prevalidation context, but still broadcasted.

*)

type t

type limits = {
  max_refused_operations : int ;
  operation_timeout : float ;
  worker_limits : Worker_types.limits ;
}

type error += Closed of Chain_id.t

(** Creates a new worker. Each chain is associated with a prevalidator. Typically,
    this is the case for the main chain and a test chain *)
val create: limits -> Distributed_db.chain_db -> t Lwt.t

val shutdown: t -> unit Lwt.t

(** Notify the prevalidator worker of a set of operations (in the form of a mempool)
    received from a peer. *)
val notify_operations: t -> P2p_peer.Id.t -> Mempool.t -> unit

(** Notify the prevalidator worker of a new injected operation. This will be added
    to the mempool of the worker *)
val inject_operation: t -> Operation.t -> unit tzresult Lwt.t

(** Notify the prevalidator worker that a new head was received. The new head will
    cause the reset of the prevalidation context *)
val flush: t -> Block_hash.t -> unit tzresult Lwt.t

(** Returns the timestamp of the prevalidator worker, that is the timestamp of the last
    reset of the prevalidation context *)
val timestamp: t -> Time.t

(** Returns the list of valid operations known to this prevalidation worker *)
val operations: t -> error Preapply_result.t * Operation.t Operation_hash.Map.t

(** Returns the list of pending operations known to this prevalidation worker *)
val pending: ?block:State.Block.t -> t -> Operation.t Operation_hash.Map.t Lwt.t

(** Returns the list of prevalidation workers running and their associated chain *)
val running_workers: unit -> (Chain_id.t * t) list

(** Worker status and events *)

val status: t -> Worker_types.worker_status
val pending_requests : t -> (Time.t * Prevalidator_worker_state.Request.view) list
val current_request : t -> (Time.t * Time.t * Prevalidator_worker_state.Request.view) option
val last_events : t -> (Lwt_log_core.level * Prevalidator_worker_state.Event.t list) list

val rpc_directory : t option RPC_directory.t
