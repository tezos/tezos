(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t
val encoding : t Data_encoding.t

type secret
val secret_encoding : secret Data_encoding.t

val of_ed25519_pkh : secret -> Ed25519.Public_key_hash.t -> t

val of_hex : string -> t
val secret_of_hex : string -> secret

val compare : t -> t -> int
val (=) : t -> t -> bool
