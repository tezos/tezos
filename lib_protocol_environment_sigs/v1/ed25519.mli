(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos - Ed25519 cryptography *)

(** {2 Hashed public keys for user ID} ***************************************)

module Public_key_hash : Hash.HASH


(** {2 Signature} ************************************************************)

module Public_key : sig

  include Compare.S
  val encoding: t Data_encoding.t

  val hash: t -> Public_key_hash.t

  type Base58.data +=
    | Public_key of t

  val of_b58check_exn: string -> t
  val of_b58check_opt: string -> t option
  val to_b58check: t -> string

  val of_bytes: Bytes.t -> t

  val to_hex: t -> [ `Hex of string ]
  val of_hex: [ `Hex of string ] -> t option
  val of_hex_exn: [ `Hex of string ] -> t

end

module Secret_key : sig

  type t
  val encoding: t Data_encoding.t

  type Base58.data +=
    | Secret_key of t

  val of_b58check_exn: string -> t
  val of_b58check_opt: string -> t option
  val to_b58check: t -> string

  val of_bytes: Bytes.t -> t

end

module Signature : sig

  type t
  val encoding: t Data_encoding.t

  type Base58.data +=
    | Signature of t

  val of_b58check_exn: string -> t
  val of_b58check_opt: string -> t option
  val to_b58check: t -> string

  val of_bytes: Bytes.t -> t

  (** Checks a signature *)
  val check: Public_key.t -> t -> MBytes.t -> bool

  (** Append a signature *)
  val append: Secret_key.t -> MBytes.t -> MBytes.t

end

val sign: Secret_key.t -> MBytes.t -> Signature.t

val generate_key: unit -> (Public_key_hash.t * Public_key.t * Secret_key.t)
