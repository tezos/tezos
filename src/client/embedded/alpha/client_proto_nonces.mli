(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val mem:
  Client_commands.context ->
  Block_hash.t -> bool Lwt.t
val find:
  Client_commands.context ->
  Block_hash.t -> Nonce.t option Lwt.t
val add:
  Client_commands.context ->
  Block_hash.t -> Nonce.t -> unit tzresult Lwt.t
val del:
  Client_commands.context ->
  Block_hash.t -> unit tzresult Lwt.t
val dels:
  Client_commands.context ->
  Block_hash.t list -> unit tzresult Lwt.t
