(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha

(** Wrappers around Services_registration calls *)

val endorsement_rights :
  tc:Tezos_context.context -> unit ->
  (int * Tezos_context.public_key_hash) list tzresult Lwt.t

val baking_rights :
  tc:Tezos_context.context -> unit ->
  (int * Tezos_context.public_key_hash) list tzresult Lwt.t
