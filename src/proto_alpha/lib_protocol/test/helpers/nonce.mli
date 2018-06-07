(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha

(** Returns a fresh nonce and its corresponding hash (and stores them). *)
val generate: unit -> Nonce_hash.t * Alpha_context.Nonce.t
val get: Nonce_hash.t -> Alpha_context.Nonce.t
val forget_all: unit -> unit
