(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


(** {2 Hash Types} ************************************************************)

(** The signature of an abstract hash type, as produced by functor
    {!Make_Blake2B}. The {!t} type is abstracted for separating the
    various kinds of hashes in the system at typing time. Each type is
    equipped with functions to use it as is of as keys in the database
    or in memory sets and maps. *)

module type MINIMAL_HASH = sig

  type t

  val name: string
  val title: string

  val hash_bytes: Cstruct.buffer list -> t
  val hash_string: string list -> t
  val size: int (* in bytes *)
  val compare: t -> t -> int
  val equal: t -> t -> bool

  val to_hex: t -> string
  val of_hex: string -> t option
  val of_hex_exn: string -> t

  val to_string: t -> string
  val of_string: string -> t option
  val of_string_exn: string -> t

  val to_bytes: t -> Cstruct.buffer
  val of_bytes_opt: Cstruct.buffer -> t option
  val of_bytes_exn: Cstruct.buffer -> t

  val read: Cstruct.buffer -> int -> t
  val write: Cstruct.buffer -> int -> t -> unit

  val to_path: t -> string list -> string list
  val of_path: string list -> t option
  val of_path_exn: string list -> t

  val prefix_path: string -> string list
  val path_length: int

end

module type HASH = sig

  include MINIMAL_HASH

  val zero: t

  val to_b58check: t -> string
  val to_short_b58check: t -> string

  val of_b58check_exn: string -> t
  val of_b58check_opt: string -> t option

  type Base58.data += Hash of t
  val b58check_encoding: t Base58.encoding

  val pp: Format.formatter -> t -> unit
  val pp_short: Format.formatter -> t -> unit

end

module type MERKLE_TREE = sig

  type elt
  val elt_bytes: elt -> Cstruct.buffer

  include HASH

  val compute: elt list -> t
  val empty: t

  type path =
    | Left of path * t
    | Right of t * path
    | Op

  val compute_path: elt list -> int -> path
  val check_path: path -> elt -> t * int

end
