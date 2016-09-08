(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos - Simple imperative (key x value) store *)

type key = string list
type value = MBytes.t

module type TYPED_IMPERATIVE_STORE = sig
  type t
  type key
  type value
  val mem: t -> key -> bool Lwt.t
  val get: t -> key -> value option Lwt.t
  val get_exn: t -> key -> value Lwt.t
  val set: t -> key -> value -> unit Lwt.t
  val del: t -> key -> unit Lwt.t
end

module type IMPERATIVE_STORE = sig
  type t
  val mem: t -> key -> bool Lwt.t
  val get: t -> key -> value option Lwt.t
  val get_exn: t -> key -> value Lwt.t
  val set: t -> key -> value -> unit Lwt.t
  val del: t -> key -> unit Lwt.t
  val list: t -> key list -> key list Lwt.t
  val remove_rec: t -> key -> unit Lwt.t
end

(** A generic (key x value) store. *)
type generic_store
type block_store
type blockchain_store
type operation_store

type store = private {
  block: block_store Persist.shared_ref ;
  blockchain: blockchain_store Persist.shared_ref ;
  operation: operation_store Persist.shared_ref ;
  global_store: generic_store Persist.shared_ref ;
  net_init: ?expiration:Time.t -> genesis -> net_store Lwt.t ;
  net_read: net_id -> net_store tzresult Lwt.t ;
  net_destroy: net_store -> unit Lwt.t ;
}

and net_store = private {
  net_genesis: genesis ;
  net_expiration: Time.t option ;
  net_store: generic_store Persist.shared_ref ;
}

and genesis = {
  time: Time.t ;
  block: Block_hash.t ;
  protocol: Protocol_hash.t ;
}

and net_id = Net of Block_hash.t

val net_id_encoding: net_id Data_encoding.t
val pp_net_id: Format.formatter -> net_id -> unit

(** Open or initialize a store at a given path. *)
val init: string -> store Lwt.t

(** {2 Generic interface} ****************************************************)

(** The generic primitives do work on the direct root, but in a
    "data/" subdirectory and do not colide with following block and
    operation specific functions. *)
include IMPERATIVE_STORE with type t = generic_store

(** {2 Types} ****************************************************************)

(** Raw operations in the database (partially parsed).
    See [State.Operation.t] for detailled description. *)
type shell_operation = {
  net_id: net_id ;
}
type operation = {
  shell: shell_operation ;
  proto: MBytes.t ;
}

val shell_operation_encoding: shell_operation Data_encoding.t
val operation_encoding: operation Data_encoding.t

(** Raw blocks in the database (partially parsed). *)
type shell_block_header = {
  net_id: net_id ;
  predecessor: Block_hash.t ;
  timestamp: Time.t ;
  fitness: MBytes.t list ;
  operations: Operation_hash.t list ;
}
type block_header = {
  shell: shell_block_header ;
  proto: MBytes.t ;
}
val shell_block_header_encoding: shell_block_header Data_encoding.t
val block_header_encoding: block_header Data_encoding.t

(** {2 Block and operations store} ********************************************)

module Block : sig

  val of_bytes: MBytes.t -> block_header option
  val to_bytes: block_header -> MBytes.t
  val hash: block_header -> Block_hash.t

  include TYPED_IMPERATIVE_STORE
    with type t = block_store
     and type key = Block_hash.t
     and type value =
           Block_hash.t * block_header Time.timed_data option Lwt.t Lazy.t

  val compare: block_header -> block_header -> int
  val equal: block_header -> block_header -> bool

  val raw_get: t -> Block_hash.t -> MBytes.t option Lwt.t
  val full_get: t -> Block_hash.t -> block_header Time.timed_data option Lwt.t

  val full_set: t -> Block_hash.t -> block_header Time.timed_data -> unit Lwt.t

end

module Block_valid_succs : TYPED_IMPERATIVE_STORE
  with type t = generic_store
   and type key = Block_hash.t
   and type value = Block_hash_set.t

module Block_invalid_succs : TYPED_IMPERATIVE_STORE
  with type t = generic_store
   and type key = Block_hash.t
   and type value = Block_hash_set.t

module Blockchain : TYPED_IMPERATIVE_STORE
  with type t = blockchain_store
   and type key = Block_hash.t
   and type value = Time.t

module Blockchain_succ : TYPED_IMPERATIVE_STORE
  with type t = blockchain_store
   and type key = Block_hash.t
   and type value = Block_hash.t

module Blockchain_test_succ : TYPED_IMPERATIVE_STORE
  with type t = blockchain_store
   and type key = Block_hash.t
   and type value = Block_hash.t

module Operation : sig

  val of_bytes: MBytes.t -> operation option
  val to_bytes: operation -> MBytes.t

  (** Computes the hash of a raw operation
      (including both abstract and parsed parts) *)
  val hash: operation -> Operation_hash.t

  include TYPED_IMPERATIVE_STORE
    with type t = operation_store
     and type key = Operation_hash.t
     and type value = operation tzresult Time.timed_data

  val compare: operation -> operation -> int
  val equal: operation -> operation -> bool

  val raw_get: t -> Operation_hash.t -> MBytes.t option Lwt.t

end

(**/**) (* For testing only *)

(* module LwtUnixStore : sig *)
  (* include Persist.STORE with type t = generic_store *)
  (* val init : string -> t Lwt.t *)
(* end *)

module IrminPath = Irmin.Path.String_list
module MBytesContent : Irmin.Contents.S with type t = MBytes.t
                                         and module Path = IrminPath

module Faked_functional_operation :
  Persist.TYPED_STORE with type t = Operation.t
                       and type value = Operation.value
                       and type key = Operation.key

module Faked_functional_block :
  Persist.TYPED_STORE with type t = Block.t
                       and type value = Block.value
                       and type key = Block.key

module Faked_functional_store : Persist.STORE with type t = t
