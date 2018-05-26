(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Client_keys.SIGNER

val make_pk: Signature.public_key -> Client_keys.pk_uri
val make_sk: Signature.secret_key -> Client_keys.sk_uri
