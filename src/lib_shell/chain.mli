(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
