(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Protocol Environment - Basic data structures *)

(** Generic interface for a datatype with comparison, pretty-printer
    and serialization functions. *)
module type DATA = sig

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

(** Generic interface for a datatype with comparison, pretty-printer,
    serialization functions and a hashing function. *)
module type HASHABLE_DATA = sig

  include DATA

  type hash
  val hash: t -> hash
  val hash_raw: MBytes.t -> hash

end

(** The fitness of a block is defined as a list of bytes,
    compared in a lexicographical order (longer list are greater). *)
module Fitness : DATA with type t = MBytes.t list

(** Tezos operations. *)
module Operation : sig

  type shell_header = {
    branch: Block_hash.t ;
    (** The operation is only valid in a branch containing the
        block [branch]. *)
  }
  val shell_header_encoding: shell_header Data_encoding.t

  type t = {
    shell: shell_header ;
    proto: MBytes.t ;
  }

  include HASHABLE_DATA with type t := t
                         and type hash := Operation_hash.t

end

module Block_header : sig

  type shell_header = {
    net_id: Net_id.t ;
    level: Int32.t ;
    (** The number of preceding block in this chain, i.e. the genesis
        has level 0. *)
    proto_level: int ;
    (** The number of preceding protocol change in the chain (modulo 256), 
        i.e the genesis has proto_level 0. *)
    predecessor: Block_hash.t ;
    timestamp: Time.t ;
    validation_passes: int ;
    operations_hash: Operation_list_list_hash.t ;
    fitness: MBytes.t list ;
  }

  val shell_header_encoding: shell_header Data_encoding.t

  type t = {
    shell: shell_header ;
    proto: MBytes.t ;
  }

  include HASHABLE_DATA with type t := t
                         and type hash := Block_hash.t

end

module Protocol : sig

  type t = {
    expected_env: env_version ;
    components: component list ;
  }

  (** An OCaml source component of a protocol implementation. *)
  and component = {
    (* The OCaml module name. *)
    name : string ;
    (* The OCaml interface source code *)
    interface : string option ;
    (* The OCaml source code *)
    implementation : string ;
  }

  and env_version = V1

  val component_encoding: component Data_encoding.t
  val env_version_encoding: env_version Data_encoding.t

  include HASHABLE_DATA with type t := t
                         and type hash := Protocol_hash.t

end
