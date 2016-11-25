
(** Tezos - Manipulation and creation of hashes *)

(** {2 Hash Types} ************************************************************)

(** The signature of an abstract hash type, as produced by functor
    {!Make_SHA256}. The {!t} type is abstracted for separating the
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
  val of_hex: string -> t
  val to_hex: t -> string
  val of_string: string -> t
  val to_string: t -> string
  val to_bytes: t -> MBytes.t
  val of_bytes: MBytes.t -> t
  val read: MBytes.t -> int -> t
  val write: MBytes.t -> int -> t -> unit
  val to_path: t -> string list
  val of_path: string list -> t
  val prefix_path: string -> string list
  val path_len: int

end

module type HASH = sig

  include MINIMAL_HASH

  val of_b48check: string -> t
  val to_b48check: t -> string
  val to_short_b48check: t -> string
  val encoding: t Data_encoding.t
  val pp: Format.formatter -> t -> unit
  val pp_short: Format.formatter -> t -> unit
  type Base48.data += Hash of t
  val b48check_encoding: t Base48.encoding

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
  val b48check_prefix : string
end

(** Builds a new Hash type using Sha256. *)

module Make_minimal_Blake2B (Name : Name) : MINIMAL_HASH
module Make_Blake2B
    (Register : sig
       val register_encoding:
         prefix: string ->
         to_raw: ('a -> string) ->
         of_raw: (string -> 'a option) ->
         wrap: ('a -> Base48.data) ->
         'a Base48.encoding
     end)
    (Name : PrefixedName) : HASH

(** Builds a Set of values of some Hash type. *)
module Hash_set (Hash : HASH) : sig
  include Set.S with type elt = Hash.t
  val encoding: t Data_encoding.t
end

(** Builds a Map using some Hash type as keys. *)
module Hash_map (Hash : HASH) : sig
  include Map.S with type key = Hash.t
  val encoding: 'a Data_encoding.t -> 'a t Data_encoding.t
end

(** {2 Predefined Hashes } ****************************************************)

(** Blocks hashes / IDs. *)
module Block_hash : HASH
module Block_hash_set : Set.S with type elt = Block_hash.t
module Block_hash_map : module type of Hash_map (Block_hash)

(** Operations hashes / IDs. *)
module Operation_hash : HASH
module Operation_hash_set : Set.S with type elt = Operation_hash.t
module Operation_hash_map : module type of Hash_map (Operation_hash)

(** Protocol versions / source hashes. *)
module Protocol_hash : HASH
module Protocol_hash_set : Set.S with type elt = Protocol_hash.t
module Protocol_hash_map : module type of Hash_map (Protocol_hash)
