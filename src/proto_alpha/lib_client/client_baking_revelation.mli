(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Tezos_context

val inject_seed_nonce_revelation:
  #RPC_context.simple ->
  Block_services.block ->
  ?async:bool ->
  (Raw_level.t * Nonce.t) list ->
  Operation_hash.t tzresult Lwt.t

val forge_seed_nonce_revelation:
  #Client_commands.full_context ->
  Block_services.block ->
  (Raw_level.t * Nonce.t) list ->
  unit tzresult Lwt.t
