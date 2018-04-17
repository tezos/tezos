(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos - Ed25519 cryptography *)

include S.SIGNATURE

include S.RAW_DATA with type t := t

module Seed : sig
  type t
  val generate : unit -> t
  val extract : Secret_key.t -> t
end

val generate_seeded_key: Seed.t -> (Public_key_hash.t * Public_key.t * Secret_key.t)
