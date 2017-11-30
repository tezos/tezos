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

type t = {
  known_valid: Operation_hash.t list ;
  (** A valid sequence of operations on top of the current head. *)
  pending: Operation_hash.Set.t ;
  (** Set of known not-invalid operation. *)
}
type mempool = t

val encoding: mempool Data_encoding.t

val empty: mempool
(** Empty mempool. *)
