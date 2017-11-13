(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Ed25519 = Tezos_protocol_environment.Ed25519

module Public_key_hash :
  Client_aliases.Alias with type t = Ed25519.Public_key_hash.t
module Public_key : Client_aliases.Alias with type t = Ed25519.Public_key.t
module Secret_key : Client_aliases.Alias with type t = Ed25519.Secret_key.t

module Seed : sig
  val to_hex : Sodium.Sign.seed -> string
  val of_hex : string -> Sodium.Sign.seed
  val generate : unit -> Sodium.Sign.seed
  val extract : Secret_key.t -> Sodium.Sign.seed
end

val get_key:
  Client_commands.context ->
  Public_key_hash.t ->
  ( string * Public_key.t * Secret_key.t ) tzresult Lwt.t

val get_keys:
  Client_commands.context ->
  ( string * Public_key_hash.t * Public_key.t * Secret_key.t ) list tzresult Lwt.t

val list_keys:
  Client_commands.context ->
  (string * Public_key_hash.t * bool * bool) list tzresult Lwt.t

val gen_keys:
  ?seed: Sodium.Sign.seed ->
  Client_commands.context ->
  string ->
  unit tzresult Lwt.t

val commands: unit -> Client_commands.command list
