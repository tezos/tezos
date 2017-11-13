(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Shell Module - Manging the current head. *)

open State

val genesis: Net.t -> Block.t Lwt.t
(** The genesis block of the network's blockchain. On a test network,
    the test protocol has been promoted as "main" protocol. *)

val head: Net.t -> Block.t Lwt.t
(** The current head of the network's blockchain. *)

val known_heads: Net.t -> Block.t list Lwt.t
(** The current head and all the known (valid) alternate heads. *)

val mem: Net.t -> Block_hash.t -> bool Lwt.t
(** Test whether a block belongs to the current mainnet. *)

val set_head: Net.t -> Block.t -> Block.t Lwt.t
(** Record a block as the current head of the network's blockchain.
    It returns the previous head. *)

val test_and_set_head:
  Net.t -> old:Block.t -> Block.t -> bool Lwt.t
(** Atomically change the current head of the network's blockchain.
    This returns [true] whenever the change succeeded, or [false]
    when the current head os not equal to the [old] argument. *)

