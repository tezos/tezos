(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Low-level byte array querying and manipulation.

    Default layout for numeric operations is big-endian.
    Little-endian operations in the LE submodule. **)

include module type of Bigstring
include Compare.S with type t := t

include EndianBigstring.EndianBigstringSig
module LE : EndianBigstring.EndianBigstringSig

val make : int -> char -> t
val of_hex : Hex.t -> t
val to_hex : t -> Hex.t
val pp_hex : Format.formatter -> t -> unit
