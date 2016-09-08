(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type account = {
  public_key_hash : Ed25519.public_key_hash ;
  public_key : Ed25519.public_key ;
  secret_key : Ed25519.secret_key ;
}

val account_encoding: account Data_encoding.t

val accounts: account list

val init: Storage.t -> Storage.t tzresult Lwt.t
