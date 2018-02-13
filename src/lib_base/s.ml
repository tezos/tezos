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
    include Set.S with type elt = t
    val encoding: t Data_encoding.t
  end

  module Map : sig
    include Map.S with type key = t
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
    ('a, 'arg) Cli_entries.params ->
    (t -> 'a, 'arg) Cli_entries.params

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
    ('a, 'arg) Cli_entries.params ->
    (t -> 'a, 'arg) Cli_entries.params

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
