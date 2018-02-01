(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** {2 Location of keys using schemes} *)

type sk_locator = Sk_locator of { scheme : string ; location : string }
type pk_locator = Pk_locator of { scheme : string ; location : string }

module type LOCATOR = sig
  val name : string
  type t

  val create : scheme:string -> location:string -> t
  val scheme : t -> string
  val location : t -> string
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

module Secret_key_locator : LOCATOR with type t = sk_locator
module Public_key_locator : LOCATOR with type t = pk_locator

(** {2 Cryptographic keys tables } *)

module Public_key_hash :
  Client_aliases.Alias with type t = Ed25519.Public_key_hash.t
module Public_key :
  Client_aliases.Alias with type t = pk_locator
module Secret_key :
  Client_aliases.Alias with type t = sk_locator

(** {2 Interface for external signing modules.} *)

module type SIGNER = sig
  type secret_key
  type public_key

  val scheme : string
  (** [scheme] is the name of the scheme implemented by this signer
      module. *)

  val sk_locator_of_human_input :
    Client_commands.logging_wallet ->
    string list -> sk_locator tzresult Lwt.t
  (** [sk_locator_of_human_input wallet spec] is the [sk_locator]
      corresponding to the human readable specification [spec] (plugin
      dependent). *)

  val pk_locator_of_human_input :
    Client_commands.logging_wallet ->
    string list -> pk_locator tzresult Lwt.t
  (** [pk_locator_of_human_input wallet spec] is the [pk_locator]
      corresponding to the human readable specification [spec] (plugin
      dependent). *)

  val sk_of_locator : sk_locator -> secret_key tzresult Lwt.t
  (** [sk_of_locator skloc] is the secret key at [skloc]. *)

  val pk_of_locator : pk_locator -> public_key tzresult Lwt.t
  (** [pk_of_locator pkloc] is the public key at [pkloc]. *)

  val sk_to_locator : secret_key -> sk_locator Lwt.t
  (** [sk_to_locator sk] is the location of secret key [sk]. *)

  val pk_to_locator : public_key -> pk_locator Lwt.t
  (** [pk_to_locator pk] is the location of public key [pk]. *)

  val neuterize : secret_key -> public_key Lwt.t
  (** [neuterize sk] is the corresponding [pk]. *)

  val public_key : public_key -> Ed25519.Public_key.t Lwt.t
  (** [public_key pk] is the Ed25519 version of [pk]. *)

  val public_key_hash : public_key -> Ed25519.Public_key_hash.t Lwt.t
  (** [public_key_hash pk] is the hash of [pk]. *)

  val sign : secret_key -> MBytes.t -> Ed25519.Signature.t tzresult Lwt.t
  (** [sign sk data] is signature obtained by signing [data] with
      [sk]. *)
end

val register_signer : (module SIGNER) -> unit
(** [register_signer signer] sets first-class module [signer] as
    signer for keys with scheme [(val signer : SIGNER).scheme]. *)

val find_signer_for_key : scheme:string -> (module SIGNER) tzresult
val sign : sk_locator -> MBytes.t -> Ed25519.Signature.t tzresult Lwt.t
val append : sk_locator -> MBytes.t -> MBytes.t tzresult Lwt.t

val get_key:
  Client_commands.full_context ->
  Public_key_hash.t ->
  (string * Ed25519.Public_key.t * sk_locator) tzresult Lwt.t

val get_keys:
  #Client_commands.wallet ->
  (string * Public_key_hash.t * Ed25519.Public_key.t * sk_locator) list tzresult Lwt.t

val force_switch : (bool, Client_commands.full_context) Cli_entries.arg

val commands: unit -> Client_commands.command list
