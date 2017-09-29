(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


open State

val genesis: Net.t -> Block.t Lwt.t
(** The genesis block of the network's blockchain. On a test network,
    the test protocol has been promoted as "main" protocol. *)

val head: Net.t -> Block.t Lwt.t
(** The current head of the network's blockchain. *)

val known_heads: Net.t -> Block.t list Lwt.t

val mem: Net.t -> Block_hash.t -> bool Lwt.t
(** Test whether a block belongs to the current mainnet. *)

val set_head: Net.t -> Block.t -> unit Lwt.t
(** Record a block as the current head of the network's blockchain. *)

val set_reversed_mempool: Net.t -> Operation_hash.t list -> unit Lwt.t
(** Record a list as the current list of pending operations. *)

val mempool: Net.t -> Operation_hash.t list Lwt.t

val test_and_set_head:
  Net.t -> old:Block.t -> Block.t -> bool Lwt.t
(** Atomically change the current head of the network's blockchain.
    This returns [true] whenever the change succeeded, or [false]
    when the current head os not equal to the [old] argument. *)

val find_new:
  Net.t -> Block_hash.t list -> int -> Block_hash.t list Lwt.t
  (** [find_new net locator max_length], where [locator] is a sparse block
      locator (/à la/ Bitcoin), returns the missing block when compared
      with the current branch of [net]. *)
