(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include (module type of Tezos_crypto.Crypto_box)

val public_key_encoding : public_key Data_encoding.t
val secret_key_encoding : secret_key Data_encoding.t
val nonce_encoding : nonce Data_encoding.t
