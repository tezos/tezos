(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Public_key_hash :
  Client_aliases.Alias with type t = Ed25519.Public_key_hash.t
module Public_key : Client_aliases.Alias with type t = Ed25519.Public_key.t
module Secret_key : Client_aliases.Alias with type t = Ed25519.Secret_key.t

val get_key:
  Client_commands.full_context ->
  Public_key_hash.t ->
  ( string * Public_key.t * Secret_key.t ) tzresult Lwt.t

val get_keys:
  #Client_commands.wallet ->
  ( string * Public_key_hash.t * Public_key.t * Secret_key.t ) list tzresult Lwt.t

val list_keys:
  Client_commands.full_context ->
  (string * Public_key_hash.t * bool * bool) list tzresult Lwt.t

val gen_keys:
  ?force:bool ->
  ?seed: Ed25519.Seed.t ->
  #Client_commands.wallet ->
  string ->
  unit tzresult Lwt.t

val force_switch : (bool, Client_commands.full_context) Cli_entries.arg

val commands: unit -> Client_commands.command list
