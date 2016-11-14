(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


module Public_key_hash :
  Client_aliases.Alias with type t = Ed25519.Public_key_hash.t
module Public_key : Client_aliases.Alias with type t = Ed25519.public_key
module Secret_key : Client_aliases.Alias with type t = Ed25519.secret_key

val get_key:
  Public_key_hash.t ->
  ( string * Public_key.t * Secret_key.t ) tzresult Lwt.t


val commands: unit -> Cli_entries.command list
