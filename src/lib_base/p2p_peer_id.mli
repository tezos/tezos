(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Tezos_crypto.S.HASH with type t = Crypto_box.Public_key_hash.t

module Logging: sig
  val tag: t Tag.def
  val tag_opt: t option Tag.def
  val tag_source: t option Tag.def
end
