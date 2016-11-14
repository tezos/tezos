(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos - Ed25519 cryptography *)


(** {2 Signature} ************************************************************)

(** An Ed25519 public key *)
type public_key = Sodium.Sign.public_key

(** An Ed25519 secret key *)
type secret_key = Sodium.Sign.secret_key

(** The result of signing a sequence of bytes with a secret key *)
type signature

(** Signs a sequence of bytes with a secret key *)
val sign : secret_key -> MBytes.t -> signature

val append_signature : secret_key -> MBytes.t -> MBytes.t

(** Checks a signature *)
val check_signature : public_key -> signature -> MBytes.t -> bool

(** {2 Hashed public keys for user ID} ***************************************)

module Public_key_hash : Hash.HASH

(** Hashes an Ed25519 public key *)
val hash : public_key -> Public_key_hash.t

(** {2 Serializers} **********************************************************)

val public_key_encoding : public_key Data_encoding.t

val secret_key_encoding : secret_key Data_encoding.t

val signature_encoding : signature Data_encoding.t

(** {2 Key pairs generation} *************************************************)

val generate_key : unit -> Public_key_hash.t * public_key * secret_key
