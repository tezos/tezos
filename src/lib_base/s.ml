(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module type T = sig

  type t

  val compare: t -> t -> int
  val equal: t -> t -> bool

  val (=): t -> t -> bool
  val (<>): t -> t -> bool
  val (<): t -> t -> bool
  val (<=): t -> t -> bool
  val (>=): t -> t -> bool
  val (>): t -> t -> bool
  val min: t -> t -> t
  val max: t -> t -> t

  val pp: Format.formatter -> t -> unit

  val encoding: t Data_encoding.t
  val to_bytes: t -> MBytes.t
  val of_bytes: MBytes.t -> t option

end

module type HASHABLE = sig

  include T

  type hash
  val hash: t -> hash
  val hash_raw: MBytes.t -> hash

end
