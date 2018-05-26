(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** {2 Cryptographic keys tables } *)

type pk_uri = private Uri.t
type sk_uri = private Uri.t

module Public_key_hash :
  Client_aliases.Alias with type t = Signature.Public_key_hash.t
module Public_key :
  Client_aliases.Alias with type t = pk_uri
module Secret_key :
  Client_aliases.Alias with type t = sk_uri

(** {2 Interface for external signing modules.} *)

module type SIGNER = sig

  val scheme : string
  (** [scheme] is the name of the scheme implemented by this signer
      module. *)

  val title : string
  (** [title] is a one-line human readable description of the signer. *)

  val description : string
  (** [description] is a multi-line human readable description of the
      signer, that should include the format of key specifications. *)

  val neuterize : sk_uri -> pk_uri tzresult Lwt.t
  (** [neuterize sk] is the corresponding [pk]. *)

  val public_key : pk_uri -> Signature.Public_key.t tzresult Lwt.t
  (** [public_key pk] is the Ed25519 version of [pk]. *)

  val public_key_hash : pk_uri -> Signature.Public_key_hash.t tzresult Lwt.t
  (** [public_key_hash pk] is the hash of [pk]. *)

  val sign :
    ?watermark: Signature.watermark ->
    sk_uri -> MBytes.t -> Signature.t tzresult Lwt.t
    (** [sign ?watermark sk data] is signature obtained by signing [data] with
        [sk]. *)
end

val register_signer : (module SIGNER) -> unit
(** [register_signer signer] registers first-class module [signer] as
    signer for keys with scheme [(val signer : SIGNER).scheme]. *)

val registered_signers : unit -> (string * (module SIGNER)) list

val public_key : pk_uri -> Signature.Public_key.t tzresult Lwt.t

val public_key_hash : pk_uri -> Signature.Public_key_hash.t tzresult Lwt.t

val neuterize : sk_uri -> pk_uri tzresult Lwt.t

val sign :
  ?watermark:Signature.watermark ->
  sk_uri -> MBytes.t -> Signature.t tzresult Lwt.t

val append :
  ?watermark:Signature.watermark ->
  sk_uri -> MBytes.t -> MBytes.t tzresult Lwt.t

val register_key :
  #Client_context.wallet ->
  ?force:bool ->
  (Signature.Public_key_hash.t * pk_uri * sk_uri) -> string -> unit tzresult Lwt.t

val list_keys :
  #Client_context.wallet ->
  (string * Public_key_hash.t * Signature.public_key option * sk_uri option) list tzresult Lwt.t

val alias_keys :
  #Client_context.wallet -> string ->
  (Public_key_hash.t * Signature.public_key option * sk_uri option) option tzresult Lwt.t

val get_key :
  #Client_context.wallet ->
  Public_key_hash.t ->
  (string * Signature.Public_key.t * sk_uri) tzresult Lwt.t

val get_public_key :
  #Client_context.wallet ->
  Public_key_hash.t ->
  (string * Signature.Public_key.t) tzresult Lwt.t

val get_keys:
  #Client_context.wallet ->
  (string * Public_key_hash.t * Signature.Public_key.t * sk_uri) list tzresult Lwt.t

val force_switch : unit -> (bool, 'ctx) Clic.arg

(**/**)

val make_pk_uri : Uri.t -> pk_uri
val make_sk_uri : Uri.t -> sk_uri

