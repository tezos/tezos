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

(** Tezos Shell Module - Manging the current head. *)

(** The genesis block of the chain. On a test chain,
    the test protocol has been promoted as "main" protocol. *)
val genesis: State.Chain.t -> State.Block.t Lwt.t

(** The current head of the chain. *)
val head: State.Chain.t -> State.Block.t Lwt.t
val locator: State.Chain.t -> Block_locator.seed -> Block_locator.t Lwt.t

(** All the available chain data. *)
type data = {
  current_head: State.Block.t ;
  current_mempool: Mempool.t ;
  live_blocks: Block_hash.Set.t ;
  live_operations: Operation_hash.Set.t ;
  test_chain: Chain_id.t option ;
}

(** Reading atomically all the chain data. *)
val data: State.Chain.t -> data Lwt.t

(** The current head and all the known (valid) alternate heads. *)
val known_heads: State.Chain.t -> State.Block.t list Lwt.t

(** Test whether a block belongs to the current mainchain. *)
val mem: State.Chain.t -> Block_hash.t -> bool Lwt.t

(** Record a block as the current head of the chain.
    It returns the previous head. *)
val set_head: State.Chain.t -> State.Block.t -> State.Block.t Lwt.t

(** Atomically change the current head of the chain.
    This returns [true] whenever the change succeeded, or [false]
    when the current head os not equal to the [old] argument. *)
val test_and_set_head:
  State.Chain.t -> old:State.Block.t -> State.Block.t -> bool Lwt.t

(** Restores the data about the current head at startup
    (recomputes the sets of live blocks and operations). *)
val init_head: State.Chain.t -> unit Lwt.t
