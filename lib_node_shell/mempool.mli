(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Shell Module - Mempool, a.k.a. the operations safe to be
    broadcasted. *)

type t = State.mempool = {
  known_valid: Operation_hash.t list ;
  (** A valid sequence of operations on top of the current head. *)
  pending: Operation_hash.Set.t ;
  (** Set of known not-invalid operation. *)
}
type mempool = t

val encoding: mempool Data_encoding.t

val empty: mempool
(** Empty mempool. *)

val get: State.Net.t -> (Block_header.t * mempool) Lwt.t
(** The current mempool,  *)

val set: State.Net.t -> head:Block_hash.t -> mempool -> unit Lwt.t
(** Set the current mempool. It is ignored if the current head is
    not the provided one. *)

