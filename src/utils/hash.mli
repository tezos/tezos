(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

(** Tezos - Manipulation and creation of hashes *)


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

  val hash_bytes: MBytes.t list -> t
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

  val to_bytes: t -> MBytes.t
  val of_bytes: MBytes.t -> t option
  val of_bytes_exn: MBytes.t -> t

  val read: MBytes.t -> int -> t
  val write: MBytes.t -> int -> t -> unit

  val to_path: t -> string list -> string list
  val of_path: string list -> t option
  val of_path_exn: string list -> t

  val prefix_path: string -> string list
  val path_length: int

end

module type INTERNAL_MINIMAL_HASH = sig
  include MINIMAL_HASH
  module Table : Hashtbl.S with type key = t
end

module type HASH = sig

  include MINIMAL_HASH

  val of_b58check_exn: string -> t
  val of_b58check_opt: string -> t option
  val to_b58check: t -> string
  val to_short_b58check: t -> string
  val encoding: t Data_encoding.t
  val pp: Format.formatter -> t -> unit
  val pp_short: Format.formatter -> t -> unit
  type Base58.data += Hash of t
  val b58check_encoding: t Base58.encoding

  module Set : sig
    include Set.S with type elt = t
    val encoding: t Data_encoding.t
  end

  module Map : sig
    include Map.S with type key = t
    val encoding: 'a Data_encoding.t -> 'a t Data_encoding.t
  end

end

module type INTERNAL_HASH = sig
  include HASH
  val of_b58check: string -> t tzresult
  val param:
    ?name:string ->
    ?desc:string ->
    ('a, 'arg, 'ret) Cli_entries.params ->
    (t -> 'a, 'arg, 'ret) Cli_entries.params
  val random_set_elt: Set.t -> t
  module Table : Hashtbl.S with type key = t
end

module type INTERNAL_MERKLE_TREE = sig
  type elt
  include INTERNAL_HASH
  val compute: elt list -> t
  val empty: t
  type path =
    | Left of path * t
    | Right of t * path
    | Op
  val compute_path: elt list -> int -> path
  val check_path: path -> elt -> t * int
  val path_encoding: path Data_encoding.t
end

module type MERKLE_TREE = sig
  type elt
  include HASH
  val compute: elt list -> t
  val empty: t
  type path =
    | Left of path * t
    | Right of t * path
    | Op
  val compute_path: elt list -> int -> path
  val check_path: path -> elt -> t * int
  val path_encoding: path Data_encoding.t
end

(** {2 Building Hashes} *******************************************************)

(** The parameters for creating a new Hash type using
    {!Make_Blake2B}. Both {!name} and {!title} are only informative,
    used in error messages and serializers. *)

module type Name = sig
  val name : string
  val title : string
  val size : int option
end

module type PrefixedName = sig
  include Name
  val b58check_prefix : string
end

(** Builds a new Hash type using Blake2B. *)
module Make_minimal_Blake2B (Name : Name) : INTERNAL_MINIMAL_HASH
module Make_Blake2B
    (Register : sig
       val register_encoding:
         prefix: string ->
         length: int ->
         to_raw: ('a -> string) ->
         of_raw: (string -> 'a option) ->
         wrap: ('a -> Base58.data) ->
         'a Base58.encoding
     end)
    (Name : PrefixedName) : INTERNAL_HASH

(** {2 Predefined Hashes } ****************************************************)

(** Blocks hashes / IDs. *)
module Block_hash : INTERNAL_HASH

(** Operations hashes / IDs. *)
module Operation_hash : INTERNAL_HASH

(** List of operations hashes / IDs. *)
module Operation_list_hash :
  INTERNAL_MERKLE_TREE with type elt = Operation_hash.t

module Operation_list_list_hash :
  INTERNAL_MERKLE_TREE with type elt = Operation_list_hash.t

(** Protocol versions / source hashes. *)
module Protocol_hash : INTERNAL_HASH

module Net_id : sig
  include INTERNAL_HASH
  val of_block_hash: Block_hash.t -> t
end

module Generic_hash : INTERNAL_MINIMAL_HASH

(**/**)

module Generic_Merkle_tree (H : sig
    type t
    type elt
    val encoding : t Data_encoding.t
    val empty : t
    val leaf : elt -> t
    val node : t -> t -> t
  end) : sig
  val compute : H.elt list -> H.t
  type path =
    | Left of path * H.t
    | Right of H.t * path
    | Op
  val compute_path: H.elt list -> int -> path
  val check_path: path -> H.elt -> H.t * int
end
