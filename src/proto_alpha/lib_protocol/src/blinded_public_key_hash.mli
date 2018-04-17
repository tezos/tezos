(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include S.HASH

type secret
val secret_encoding : secret Data_encoding.t

val of_ed25519_pkh : secret -> Ed25519.Public_key_hash.t -> t

val secret_of_hex : string -> secret
