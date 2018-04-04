(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type bootstrap_account = {
  public_key : Ed25519.Public_key.t ;
  amount : Tez_repr.t ;
}

type t = {
  bootstrap_accounts : bootstrap_account list ;
  commitments : (Unclaimed_public_key_hash.t * Commitment_repr.t) list
}

val encoding: t Data_encoding.t
