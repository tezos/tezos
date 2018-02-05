(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Shell Module - Manging the current head. *)

open State

(** The genesis block of the network's blockchain. On a test network,
    the test protocol has been promoted as "main" protocol. *)
val genesis: Net.t -> Block.t Lwt.t

(** The current head of the network's blockchain. *)
val head: Net.t -> Block.t Lwt.t
val locator: Net.t -> Block_locator.t Lwt.t

(** All the available chain data. *)
type data = {
  current_head: Block.t ;
  current_mempool: Mempool.t ;
  live_blocks: Block_hash.Set.t ;
  live_operations: Operation_hash.Set.t ;
  locator: Block_locator.t Lwt.t lazy_t ;
}

(** Reading atomically all the chain data. *)
val data: Net.t -> data Lwt.t


(** The current head and all the known (valid) alternate heads. *)
val known_heads: Net.t -> Block.t list Lwt.t

(** Test whether a block belongs to the current mainnet. *)
val mem: Net.t -> Block_hash.t -> bool Lwt.t

(** Record a block as the current head of the network's blockchain.
    It returns the previous head. *)
val set_head: Net.t -> Block.t -> Block.t Lwt.t

(** Atomically change the current head of the network's blockchain.
    This returns [true] whenever the change succeeded, or [false]
    when the current head os not equal to the [old] argument. *)
val test_and_set_head:
  Net.t -> old:Block.t -> Block.t -> bool Lwt.t

(** Restores the data about the current head at startup
    (recomputes the sets of live blocks and operations). *)
val init_head: Net.t -> unit Lwt.t
