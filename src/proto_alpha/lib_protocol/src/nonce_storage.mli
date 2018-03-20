(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error +=
  | Too_late_revelation
  | Too_early_revelation
  | Previously_revealed_nonce
  | Unexpected_nonce

type t = Seed_repr.nonce
type nonce = t
val encoding: nonce Data_encoding.t

type unrevealed = Storage.Seed.unrevealed_nonce = {
  nonce_hash: Nonce_hash.t ;
  delegate: Ed25519.Public_key_hash.t ;
  deposit: Tez_repr.t ;
  rewards: Tez_repr.t ;
  fees: Tez_repr.t ;
}

type status =
  | Unrevealed of unrevealed
  | Revealed of Seed_repr.nonce

val get: Raw_context.t -> Level_repr.t -> status tzresult Lwt.t

val record_hash:
  Raw_context.t -> unrevealed -> Raw_context.t tzresult Lwt.t

val reveal:
  Raw_context.t -> Level_repr.t -> nonce -> Raw_context.t tzresult Lwt.t

val of_bytes: MBytes.t -> nonce tzresult
val hash: nonce -> Nonce_hash.t
val check_hash: nonce -> Nonce_hash.t -> bool
