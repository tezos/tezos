(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val inject_seed_nonce_revelation:
  #Client_rpcs.ctxt ->
  Client_proto_rpcs.block ->
  ?force:bool ->
  ?async:bool ->
  (Raw_level.t * Nonce.t) list ->
  Operation_hash.t tzresult Lwt.t

val forge_seed_nonce_revelation:
  Client_commands.full_context ->
  Client_proto_rpcs.block ->
  ?force:bool ->
  (Raw_level.t * Nonce.t) list ->
  unit tzresult Lwt.t
