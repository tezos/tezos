(** Tezos - Ed25519 cryptography *)


(** {2 Signature} ************************************************************)

(** An Ed25519 public key *)
type public_key

(** An Ed25519 secret key *)
type secret_key

(** The result of signing a sequence of bytes with a secret key *)
type signature

(** Signs a sequence of bytes with a secret key *)
val sign : secret_key -> MBytes.t -> signature

(** Checks a signature *)
val check_signature : public_key -> signature -> MBytes.t -> bool

(** {2 Hashed public keys for user ID} ***************************************)

module Public_key_hash : Hash.HASH

(** A Sha256 hash of an Ed25519 public key for use as an ID *)
type public_key_hash = Public_key_hash.t

(** Hashes an Ed25519 public key *)
val hash : public_key -> public_key_hash

(** For using IDs as keys in the database *)
val hash_path : public_key_hash -> string list

(** ID comparison *)
val equal_hash : public_key_hash -> public_key_hash -> bool

(** ID comparison *)
val compare_hash : public_key_hash -> public_key_hash -> int

(** {2 Serializers} **********************************************************)

val public_key_hash_encoding : public_key_hash Data_encoding.t

val public_key_encoding : public_key Data_encoding.t

val secret_key_encoding : secret_key Data_encoding.t

val signature_encoding : signature Data_encoding.t
