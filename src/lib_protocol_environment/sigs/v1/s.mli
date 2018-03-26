(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Generic interface for a datatype with comparison, pretty-printer
    and serialization functions. *)
module type T = sig

  type t
  include Compare.S with type t := t

  val pp: Format.formatter -> t -> unit

  val encoding: t Data_encoding.t
  val to_bytes: t -> MBytes.t
  val of_bytes: MBytes.t -> t option

end

(** Generic interface for a datatype with comparison, pretty-printer,
    serialization functions and a hashing function. *)
module type HASHABLE = sig

  include T

  type hash
  val hash: t -> hash
  val hash_raw: MBytes.t -> hash

end

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

  val to_hex: t -> string
  val of_hex: string -> t option
  val of_hex_exn: string -> t

  val to_string: t -> string
  val of_string: string -> t option
  val of_string_exn: string -> t

  val to_bytes: t -> MBytes.t
  val of_bytes_opt: MBytes.t -> t option
  val of_bytes_exn: MBytes.t -> t

  val read: MBytes.t -> int -> t
  val write: MBytes.t -> int -> t -> unit

  val to_path: t -> string list -> string list
  val of_path: string list -> t option
  val of_path_exn: string list -> t

  val prefix_path: string -> string list
  val path_length: int

  val zero: t

end

module type SET = sig
  type elt
  type t
  val empty: t
  val is_empty: t -> bool
  val mem: elt -> t -> bool
  val add: elt -> t -> t
  val singleton: elt -> t
  val remove: elt -> t -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val iter: (elt -> unit) -> t -> unit
  val map: (elt -> elt) -> t -> t
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (elt -> bool) -> t -> bool
  val exists: (elt -> bool) -> t -> bool
  val filter: (elt -> bool) -> t -> t
  val partition: (elt -> bool) -> t -> t * t
  val cardinal: t -> int
  val elements: t -> elt list
  val min_elt: t -> elt
  val min_elt_opt: t -> elt option
  val max_elt: t -> elt
  val max_elt_opt: t -> elt option
  val choose: t -> elt
  val choose_opt: t -> elt option
  val split: elt -> t -> t * bool * t
  val find: elt -> t -> elt
  val find_opt: elt -> t -> elt option
  val find_first: (elt -> bool) -> t -> elt
  val find_first_opt: (elt -> bool) -> t -> elt option
  val find_last: (elt -> bool) -> t -> elt
  val find_last_opt: (elt -> bool) -> t -> elt option
  val of_list: elt list -> t
end

module type MAP = sig
  type key
  type (+'a) t
  val empty: 'a t
  val is_empty: 'a t -> bool
  val mem: key -> 'a t -> bool
  val add: key -> 'a -> 'a t -> 'a t
  val update: key -> ('a option -> 'a option) -> 'a t -> 'a t
  val singleton: key -> 'a -> 'a t
  val remove: key -> 'a t -> 'a t
  val merge:
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val union: (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all: (key -> 'a -> bool) -> 'a t -> bool
  val exists: (key -> 'a -> bool) -> 'a t -> bool
  val filter: (key -> 'a -> bool) -> 'a t -> 'a t
  val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal: 'a t -> int
  val bindings: 'a t -> (key * 'a) list
  val min_binding: 'a t -> (key * 'a)
  val min_binding_opt: 'a t -> (key * 'a) option
  val max_binding: 'a t -> (key * 'a)
  val max_binding_opt: 'a t -> (key * 'a) option
  val choose: 'a t -> (key * 'a)
  val choose_opt: 'a t -> (key * 'a) option
  val split: key -> 'a t -> 'a t * 'a option * 'a t
  val find: key -> 'a t -> 'a
  val find_opt: key -> 'a t -> 'a option
  val find_first: (key -> bool) -> 'a t -> key * 'a
  val find_first_opt: (key -> bool) -> 'a t -> (key * 'a) option
  val find_last: (key -> bool) -> 'a t -> key * 'a
  val find_last_opt: (key -> bool) -> 'a t -> (key * 'a) option
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
end

module type HASH = sig

  include MINIMAL_HASH

  val encoding: t Data_encoding.t

  val to_b58check: t -> string
  val to_short_b58check: t -> string
  type Base58.data += Hash of t
  val b58check_encoding: t Base58.encoding

  val of_b58check_exn: string -> t
  val of_b58check_opt: string -> t option

  val pp: Format.formatter -> t -> unit
  val pp_short: Format.formatter -> t -> unit

  val rpc_arg: t RPC_arg.t

  module Set : sig
    include SET with type elt = t
    val encoding: t Data_encoding.t
  end

  module Map : sig
    include MAP with type key = t
    val encoding: 'a Data_encoding.t -> 'a t Data_encoding.t
  end

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
