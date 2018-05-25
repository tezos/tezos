(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type public_key_hash =
  | Ed25519 of Ed25519.Public_key_hash.t
  | Secp256k1 of Secp256k1.Public_key_hash.t

type public_key =
  | Ed25519 of Ed25519.Public_key.t
  | Secp256k1 of Secp256k1.Public_key.t

type secret_key =
  | Ed25519 of Ed25519.Secret_key.t
  | Secp256k1 of Secp256k1.Secret_key.t

type watermark =
  | Block_header
  | Endorsement
  | Generic_operation
  | Custom of MBytes.t

val bytes_of_watermark: watermark -> MBytes.t

include S.SIGNATURE with type Public_key_hash.t = public_key_hash
                     and type Public_key.t = public_key
                     and type Secret_key.t = secret_key
                     and type watermark := watermark

val append : ?watermark:watermark -> secret_key -> MBytes.t -> MBytes.t
(** [append sk buf] is the concatenation of [buf] and the
    serialization of the signature of [buf] signed by [sk]. *)

val concat : MBytes.t -> t -> MBytes.t
(** [concat buf t] is the concatenation of [buf] and the serialization
    of [t]. *)

include S.RAW_DATA with type t := t

type algo =
  | Ed25519
  | Secp256k1

val algo_param: unit -> (algo, 'a) Clic.parameter

val generate_key:
  ?algo:algo ->
  ?seed:MBytes.t ->
  unit -> public_key_hash * public_key * secret_key
