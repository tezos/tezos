(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** {2 Location of keys using schemes} *)

type location = string list
type sk_uri = Sk_locator of { scheme : string ; location : location }
type pk_uri = Pk_locator of { scheme : string ; location : location }

module type LOCATOR = sig
  val name : string
  type t

  val create : scheme:string -> location:location -> t
  val scheme : t -> string
  val location : t -> location
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

module Secret_key_locator : LOCATOR with type t = sk_uri
module Public_key_locator : LOCATOR with type t = pk_uri

(** {2 Cryptographic keys tables } *)

module Public_key_hash :
  Client_aliases.Alias with type t = Signature.Public_key_hash.t
module Public_key :
  Client_aliases.Alias with type t = pk_uri
module Secret_key :
  Client_aliases.Alias with type t = sk_uri

(** {2 Interface for external signing modules.} *)

module type SIGNER = sig
  type secret_key
  type public_key

  val scheme : string
  (** [scheme] is the name of the scheme implemented by this signer
      module. *)

  val title : string
  (** [title] is a one-line human readable description of the signer. *)

  val description : string
  (** [description] is a multi-line human readable description of the
      signer, that should include the format of key specifications. *)

  val init :
    #Client_context.io_wallet -> unit tzresult Lwt.t
  (** [init wallet] initialized the signer module (plugin
      dependent). *)

  val sk_locator_of_human_input :
    #Client_context.io_wallet -> string list -> sk_uri tzresult Lwt.t
  (** [sk_locator_of_human_input wallet spec] is the [sk_locator]
      corresponding to the human readable specification [spec] (plugin
      dependent). *)

  val pk_locator_of_human_input :
    #Client_context.io_wallet -> string list -> pk_uri tzresult Lwt.t
  (** [pk_locator_of_human_input wallet spec] is the [pk_locator]
      corresponding to the human readable specification [spec] (plugin
      dependent). *)

  val sk_of_locator : sk_uri -> secret_key tzresult Lwt.t
  (** [sk_of_locator skloc] is the secret key at [skloc]. *)

  val pk_of_locator : pk_uri -> public_key tzresult Lwt.t
  (** [pk_of_locator pkloc] is the public key at [pkloc]. *)

  val sk_to_locator : secret_key -> sk_uri Lwt.t
  (** [sk_to_locator sk] is the location of secret key [sk]. *)

  val pk_to_locator : public_key -> pk_uri Lwt.t
  (** [pk_to_locator pk] is the location of public key [pk]. *)

  val neuterize : secret_key -> public_key Lwt.t
  (** [neuterize sk] is the corresponding [pk]. *)

  val public_key : public_key -> Signature.Public_key.t tzresult Lwt.t
  (** [public_key pk] is the Ed25519 version of [pk]. *)

  val public_key_hash : public_key -> Signature.Public_key_hash.t tzresult Lwt.t
  (** [public_key_hash pk] is the hash of [pk]. *)

  val sign :
    ?watermark: Signature.watermark ->
    secret_key -> MBytes.t -> Signature.t tzresult Lwt.t
    (** [sign ?watermark sk data] is signature obtained by signing [data] with
        [sk]. *)
end

val register_signer : (module SIGNER) -> unit
(** [register_signer signer] registers first-class module [signer] as
    signer for keys with scheme [(val signer : SIGNER).scheme]. *)

val registered_signers : unit -> (string * (module SIGNER)) list

val find_signer_for_key :
  #Client_context.io_wallet -> scheme:string -> (module SIGNER) tzresult Lwt.t

val sign :
  ?watermark:Signature.watermark ->
  #Client_context.io_wallet ->
  sk_uri -> MBytes.t -> Signature.t tzresult Lwt.t

val append :
  ?watermark:Signature.watermark ->
  #Client_context.io_wallet ->
  sk_uri -> MBytes.t -> MBytes.t tzresult Lwt.t

val gen_keys :
  ?force:bool ->
  ?algo:Signature.algo ->
  ?seed:MBytes.t ->
  #Client_context.io_wallet -> string -> unit tzresult Lwt.t

val register_key :
  #Client_context.wallet ->
  ?force:bool ->
  (Signature.Public_key_hash.t *
   Signature.Public_key.t *
   Signature.Secret_key.t) -> string -> unit tzresult Lwt.t

val gen_keys_containing :
  ?prefix:bool ->
  ?force:bool ->
  containing:string list ->
  name:string ->
  #Client_context.io_wallet -> unit tzresult Lwt.t

val list_keys :
  #Client_context.wallet ->
  (string * Public_key_hash.t * pk_uri option * sk_uri option) list tzresult Lwt.t

val alias_keys :
  #Client_context.wallet -> string ->
  (Public_key_hash.t * pk_uri option * sk_uri option) option tzresult Lwt.t

val get_key:
  #Client_context.io_wallet ->
  Public_key_hash.t ->
  (string * Signature.Public_key.t * sk_uri) tzresult Lwt.t

val get_public_key:
  #Client_context.io_wallet ->
  Public_key_hash.t ->
  (string * Signature.Public_key.t) tzresult Lwt.t

val get_keys:
  #Client_context.io_wallet ->
  (string * Public_key_hash.t * Signature.Public_key.t * sk_uri) list tzresult Lwt.t

val force_switch : unit -> (bool, 'ctx) Clic.arg
