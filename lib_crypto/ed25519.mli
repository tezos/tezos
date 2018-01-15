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

module Public_key_hash : S.INTERNAL_HASH

(** {2 Signature} ************************************************************)

module Public_key : sig

  include Compare.S
  val encoding: t Data_encoding.t

  val param:
    ?name:string ->
    ?desc:string ->
    ('a, 'b, 'c) Cli_entries.params ->
    (t -> 'a, 'b, 'c) Cli_entries.params

  val hash: t -> Public_key_hash.t

  type Base58.data +=
    | Public_key of t

  val of_b58check: string -> t tzresult
  val of_b58check_exn: string -> t
  val of_b58check_opt: string -> t option
  val to_b58check: t -> string

  val to_hex: t -> Hex.t
  val of_hex: Hex.t -> t option
  val of_hex_exn: Hex.t -> t

  val of_bytes: Bytes.t -> t

end

module Secret_key : sig

  type t
  val encoding: t Data_encoding.t

  val param:
    ?name:string ->
    ?desc:string ->
    ('a, 'b, 'c) Cli_entries.params ->
    (t -> 'a, 'b, 'c) Cli_entries.params

  val to_public_key: t -> Public_key.t

  type Base58.data +=
    | Secret_key of t

  val of_b58check: string -> t tzresult
  val of_b58check_exn: string -> t
  val of_b58check_opt: string -> t option
  val to_b58check: t -> string

  val of_bytes: Bytes.t -> t

end

module Signature : sig

  type t
  val encoding: t Data_encoding.t

  val param:
    ?name:string ->
    ?desc:string ->
    ('a, 'b, 'c) Cli_entries.params ->
    (t -> 'a, 'b, 'c) Cli_entries.params

  type Base58.data +=
    | Signature of t

  val of_b58check: string -> t tzresult
  val of_b58check_exn: string -> t
  val of_b58check_opt: string -> t option
  val to_b58check: t -> string

  val of_bytes: Bytes.t -> t

  (** Checks a signature *)
  val check: Public_key.t -> t -> MBytes.t -> bool

  (** Append a signature *)
  val append: Secret_key.t -> MBytes.t -> MBytes.t
  val concat: MBytes.t -> t -> MBytes.t

end

module Seed : sig
  type t
  val to_hex : t -> string
  val of_hex : string -> t
  val generate : unit -> t
  val extract : Secret_key.t -> t
end

val sign: Secret_key.t -> MBytes.t -> Signature.t

val generate_key: unit -> (Public_key_hash.t * Public_key.t * Secret_key.t)
val generate_seeded_key: Seed.t -> (Public_key_hash.t * Public_key.t * Secret_key.t)

