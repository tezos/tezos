(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module type COMPARABLE = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type t
  val (=) : t -> t -> bool
  val (<>) : t -> t -> bool
  val (<) : t -> t -> bool
  val (<=) : t -> t -> bool
  val (>=) : t -> t -> bool
  val (>) : t -> t -> bool
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val max : t -> t -> t
  val min : t -> t -> t
end

module Make (P : COMPARABLE) : S with type t := P.t

module Char : S with type t = char
module Bool : S with type t = bool
module Int : S with type t = int
module Int32 : S with type t = int32
module Uint32 : S with type t = int32
module Int64 : S with type t = int64
module Uint64 : S with type t = int64
module Float : S with type t = float
module String : S with type t = string
module Z : S with type t = Z.t

module List (P : COMPARABLE) : S with type t = P.t list
module Option (P : COMPARABLE) : S with type t = P.t option
