(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

module type T = sig

  type t
  include Compare.S with type t := t

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
  val min_elt_opt: t -> elt option
  val max_elt_opt: t -> elt option
  val choose_opt: t -> elt option
  val split: elt -> t -> t * bool * t
  val find_opt: elt -> t -> elt option
  val find_first_opt: (elt -> bool) -> t -> elt option
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
  val min_binding_opt: 'a t -> (key * 'a) option
  val max_binding_opt: 'a t -> (key * 'a) option
  val choose_opt: 'a t -> (key * 'a) option
  val split: key -> 'a t -> 'a t * 'a option * 'a t
  val find_opt: key -> 'a t -> 'a option
  val find_first_opt: (key -> bool) -> 'a t -> (key * 'a) option
  val find_last_opt: (key -> bool) -> 'a t -> (key * 'a) option
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
end

module type MINIMAL_HASH = Tezos_crypto.S.MINIMAL_HASH

module type HASH = sig

  include Tezos_crypto.S.MINIMAL_HASH

  val encoding: t Data_encoding.t

  val to_b58check: t -> string
  val to_short_b58check: t -> string
  type Tezos_crypto.Base58.data += Hash of t
  val b58check_encoding: t Tezos_crypto.Base58.encoding

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

module type INTERNAL_HASH = sig

  include Tezos_crypto.S.HASH

  val of_b58check: string -> t tzresult
  val of_bytes: MBytes.t -> t tzresult

  val encoding: t Data_encoding.t
  val rpc_arg: t RPC_arg.t

  val param:
    ?name:string ->
    ?desc:string ->
    ('a, 'arg) Clic.params ->
    (t -> 'a, 'arg) Clic.params

  module Set : sig
    include Set.S with type elt = t
    val random_elt: t -> elt
    val encoding: t Data_encoding.t
  end

  module Map : sig
    include Map.S with type key = t
    val encoding: 'a Data_encoding.t -> 'a t Data_encoding.t
  end

  module Table : sig
    include Hashtbl.S with type key = t
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

module type INTERNAL_MERKLE_TREE = sig

  include Tezos_crypto.S.MERKLE_TREE

  val path_encoding: path Data_encoding.t

  val of_b58check: string -> t tzresult
  val of_bytes: MBytes.t -> t tzresult

  val encoding: t Data_encoding.t
  val rpc_arg: t RPC_arg.t

  val param:
    ?name:string ->
    ?desc:string ->
    ('a, 'arg) Clic.params ->
    (t -> 'a, 'arg) Clic.params

  module Set : sig
    include Set.S with type elt = t
    val random_elt: t -> elt
    val encoding: t Data_encoding.t
  end

  module Map : sig
    include Map.S with type key = t
    val encoding: 'a Data_encoding.t -> 'a t Data_encoding.t
  end

  module Table : sig
    include Hashtbl.S with type key = t
    val encoding: 'a Data_encoding.t -> 'a t Data_encoding.t
  end

end
