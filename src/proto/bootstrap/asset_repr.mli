(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t
type asset = t

val encoding: asset Data_encoding.t
val of_int32 : int32 -> asset

module Map : sig
  type t
  val empty: t
  val add:
    t -> asset -> Ed25519.public_key_hash -> Tez_repr.tez -> t tzresult
  val encoding: t Data_encoding.t
end
