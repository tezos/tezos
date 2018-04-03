(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos - Ed25519 cryptography *)

open Error_monad

(** {2 Hashed public keys for user ID} ***************************************)

module Public_key_hash : S.HASH

(** {2 Signature} ************************************************************)

module Public_key : sig

  include Compare.S
  val pp : Format.formatter -> t -> unit

  val hash: t -> Public_key_hash.t

  type Base58.data +=
    | Public_key of t

  val of_b58check_exn: string -> t
  val of_b58check_opt: string -> t option
  val of_b58check: string -> t tzresult
  val to_b58check: t -> string

  val to_hex: t -> Hex.t
  val of_hex: Hex.t -> t option
  val of_hex_exn: Hex.t -> t

  val of_bytes_exn: MBytes.t -> t
  val of_bytes_opt: MBytes.t -> t option
  val to_bytes: t -> MBytes.t

  val size: int

  val encoding: t Data_encoding.t
  val param:
    ?name:string ->
    ?desc:string ->
    ('a, 'b) Clic.params ->
    (t -> 'a, 'b) Clic.params

end

module Secret_key : sig

  type t
  val pp : Format.formatter -> t -> unit

  val to_public_key: t -> Public_key.t

  type Base58.data +=
    | Secret_key of t

  val of_b58check_exn: string -> t
  val of_b58check_opt: string -> t option
  val of_b58check: string -> t tzresult
  val to_b58check: t -> string

  val of_bytes_exn: MBytes.t -> t
  val of_bytes_opt: MBytes.t -> t option
  val to_bytes: t -> MBytes.t

  val size: int

  val encoding: t Data_encoding.t
  val param:
    ?name:string ->
    ?desc:string ->
    ('a, 'b) Clic.params ->
    (t -> 'a, 'b) Clic.params

end

module Signature : sig

  type t
  val pp : Format.formatter -> t -> unit

  type Base58.data +=
    | Signature of t

  val of_b58check_exn: string -> t
  val of_b58check_opt: string -> t option
  val of_b58check: string -> t tzresult
  val to_b58check: t -> string

  val of_bytes_exn: MBytes.t -> t
  val of_bytes_opt: MBytes.t -> t option
  val to_bytes: t -> MBytes.t

  val encoding: t Data_encoding.t
  val param:
    ?name:string ->
    ?desc:string ->
    ('a, 'b) Clic.params ->
    (t -> 'a, 'b) Clic.params

  val size: int

  val zero: t

  (** Check a signature *)
  val check: Public_key.t -> t -> MBytes.t -> bool

  (** Append a signature *)
  val append: Secret_key.t -> MBytes.t -> MBytes.t
  val concat: MBytes.t -> t -> MBytes.t

end

module Seed : sig
  type t
  val generate : unit -> t
  val extract : Secret_key.t -> t
end

val sign: Secret_key.t -> MBytes.t -> Signature.t

val generate_key: unit -> (Public_key_hash.t * Public_key.t * Secret_key.t)
val generate_seeded_key: Seed.t -> (Public_key_hash.t * Public_key.t * Secret_key.t)

