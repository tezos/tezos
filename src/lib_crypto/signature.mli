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

include S.SIGNATURE with type Public_key_hash.t = public_key_hash
                     and type Public_key.t = public_key
                     and type Secret_key.t = secret_key

include S.RAW_DATA with type t := t

type algo =
  | Ed25519
  | Secp256k1

val algo_param: unit -> (algo, 'a) Clic.parameter

val generate_key:
  ?algo:algo ->
  ?seed:Ed25519.Seed.t ->
  unit -> public_key_hash * public_key * secret_key
