(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Ed25519 = Environment.Ed25519

module Public_key_hash :
  Client_aliases.Alias with type t = Ed25519.Public_key_hash.t
module Public_key : Client_aliases.Alias with type t = Ed25519.Public_key.t
module Secret_key : Client_aliases.Alias with type t = Ed25519.Secret_key.t

val get_key:
  Client_commands.context ->
  Public_key_hash.t ->
  ( string * Public_key.t * Secret_key.t ) tzresult Lwt.t


val commands: unit -> Client_commands.command list
