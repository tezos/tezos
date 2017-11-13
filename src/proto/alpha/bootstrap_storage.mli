(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type account = {
  public_key_hash : Ed25519.Public_key_hash.t ;
  public_key : Ed25519.Public_key.t ;
}

val account_encoding: account Data_encoding.t

val accounts: Storage.t -> account list

val init: Storage.t -> Storage.t tzresult Lwt.t

val refill: Storage.t -> Storage.t tzresult Lwt.t
