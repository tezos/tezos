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

(** Distributing validation work between different workers, one for each peer. *)

type limits = {
  max_promises_per_request : int ;
  worker_limits : Worker_types.limits ;
}

module type T = sig
  module Mempool_worker: Mempool_worker.T

  (** The type of a peer worker. Each peer worker should be used for treating
      all the operations from a given peer. *)
  type t

  (** Types for calls into this module *)

  (** [input] are the batches of operations that are given to a peer worker to
      validate. These hashes are gossiped on the network, and the mempool checks
      their validity before gossiping them furhter. *)
  type input = Operation_hash.t list

  (** [create limits peer_id mempool_worker] creates a peer worker meant
      to be used for validating batches of operations sent by the peer
      [peer_id]. The validation of each operations is delegated to the
      associated [mempool_worker]. *)
  val create: limits -> P2p_peer.Id.t -> Mempool_worker.t -> t tzresult Lwt.t

  (** [shutdown t] closes the peer worker [t]. It returns a list of operation
      hashes that can be recycled when a new worker is created for the same peer.
  *)
  val shutdown: t -> input Lwt.t

  (** [validate worker input] validates the batch of operations [input]. The
      work is performed by [worker] and the underlying validation of each
      operation is performed by the [mempool_worker] that was used to [create]
      [worker]. *)
  val validate: t -> input -> unit tzresult Lwt.t

end


module type STATIC = sig
  val max_pending_requests : int
end

module Make (Static: STATIC) (Mempool_worker: Mempool_worker.T)
  : T with module Mempool_worker = Mempool_worker
