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

(** Distributing validation work between different workers, one for each peer. *)

type limits = {
  worker_limits : Worker_types.limits ;
}

module type T = sig
  module Proto: Registered_protocol.T
  module Mempool_worker: Mempool_worker.T with module Proto = Proto

  (** The type of a peer worker. Each peer worker should be used for treating
   * all the operations from a given peer. *)
  type t

  (** Types for calls into this module *)

  (** [input] are the batches of operations that are given to a peer worker to
   * validate. These hashes are gossiped on the network, and the mempool checks
   * their validity before gossiping them furhter. *)
  type input = Operation_hash.t list

  (** [result] are the different possible outcome of the validation of a single
   * operation. It signals either errors in the validation process, or results
   * from further down the validation system. *)
  type result =
    | Cannot_download of error list
    | Cannot_parse of error list
    | Cannot_validate of error list
    | Mempool_result of Mempool_worker.result

  (** [output] are the outcome of the validation of a batch of operations. *)
  type output = result Operation_hash.Map.t

  (** [create limits peer_id] creates a peer worker meant to be used for
   * validating operations sent by the peer [peer_id]. *)
  val create: limits -> P2p_peer.Id.t -> t Lwt.t

  (** [shutdown t] closes the peer worker [t]. *)
  val shutdown: t -> unit Lwt.t

  (** [validate mempool_worker worker input] validates the batch of operations
   * [input]. The work is performed by [worker] and the underlying validation of
   * each operation is performed by [mempool_worker]. *)
  val validate: Mempool_worker.t -> t -> input -> output tzresult Lwt.t

  (** [bypass_peer_workers mempool_worker input] validates the batch of
   * operations [input]. Unlike [validate] above, the work is not performed by a
   * specific worker.
   *
   * This is intended to be used for cases where the [input] cannot be
   * attributed to a specific peer. Typically, this happens when injecting an
   * operation from the local client, or when recycling pending operations after
   * a protocol change.
   *
   * Note that, unlike [validate], this bypasses the worker mechanics entirely.
   * As a result, there is no possible introspection and the work from spearate
   * calls to [bypass_peer_workers] is not sequentialised. *)
  val bypass_peer_workers: Mempool_worker.t -> input -> output Lwt.t

end


module Make (Mempool_worker : Mempool_worker.T) : T with module Proto = Mempool_worker.Proto
