(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t
type global_state = t
(** An abstraction over all the disk storage used by the node.

    It encapsulates access to:

    - the index of validation contexts; and
    - the persistent state of the node:
      - the blockchain and its alternate heads of a "network";
      - the pool of pending operations of a "network". *)

val read:
  ?patch_context:(Context.t -> Context.t Lwt.t) ->
  store_root:string ->
  context_root:string ->
  unit ->
  global_state tzresult Lwt.t
(** Read the internal state of the node and initialize
    the databases. *)

val close:
  global_state -> unit Lwt.t

(** {2 Errors} **************************************************************)

type error +=
  | Unknown_network of Net_id.t


(** {2 Network} ************************************************************)

(** Data specific to a given network. *)
module Net : sig

  type t
  type net_state = t

  type genesis = {
    time: Time.t ;
    block: Block_hash.t ;
    protocol: Protocol_hash.t ;
  }
  val genesis_encoding: genesis Data_encoding.t

  val create:
    global_state ->
    ?allow_forked_network:bool ->
    genesis -> net_state Lwt.t
  (** Initialize a network for a given [genesis]. By default,
      the network does accept forking test network. When
      [~allow_forked_network:true] is provided, test network are allowed. *)

  val get: global_state -> Net_id.t -> net_state tzresult Lwt.t
  (** Look up for a network by the hash of its genesis block. *)

  val all: global_state -> net_state list Lwt.t
  (** Returns all the known networks. *)

  val destroy: global_state -> net_state -> unit Lwt.t
  (** Destroy a network: this completly removes from the local storage all
      the data associated to the network (this includes blocks and
      operations). *)

  val id: net_state -> Net_id.t
  val genesis: net_state -> genesis
  val faked_genesis_hash: net_state -> Block_hash.t
  val expiration: net_state -> Time.t option
  val allow_forked_network: net_state -> bool
  (** Accessors. Respectively access to;
      - the network id (the hash of its genesis block)
      - its optional expiration time
      - the associated global state. *)

  val global_state: net_state -> global_state

end

(** {2 Block database} ********************************************************)

module Block : sig

  type t
  type block = t

  val known: Net.t -> Block_hash.t -> bool Lwt.t
  val known_valid: Net.t -> Block_hash.t -> bool Lwt.t
  val known_invalid: Net.t -> Block_hash.t -> bool Lwt.t

  val read: Net.t -> Block_hash.t -> block tzresult Lwt.t
  val read_opt: Net.t -> Block_hash.t -> block option Lwt.t
  val read_exn: Net.t -> Block_hash.t -> block Lwt.t

  val store:
    Net.t ->
    Block_header.t ->
    Operation.t list list ->
    Updater.validation_result ->
    block option tzresult Lwt.t

  val store_invalid:
    Net.t ->
    Block_header.t ->
    bool tzresult Lwt.t

  val compare: t -> t -> int
  val equal: t -> t -> bool

  val hash: t -> Block_hash.t
  val header: t -> Block_header.t
  val shell_header: t -> Block_header.shell_header
  val timestamp: t -> Time.t
  val fitness: t -> Fitness.t
  val validation_passes: t -> int
  val net_id: t -> Net_id.t
  val net_state: t -> Net.t
  val level: t -> Int32.t
  val message: t -> string
  val max_operations_ttl: t -> int

  val is_genesis: t -> bool
  val predecessor: t -> block option Lwt.t

  val context: t -> Context.t Lwt.t
  val protocol_hash: t -> Protocol_hash.t Lwt.t
  val test_network: t -> Context.test_network Lwt.t

  val operation_hashes:
    t -> int ->
    (Operation_hash.t list * Operation_list_list_hash.path) Lwt.t
  val all_operation_hashes: t -> Operation_hash.t list list Lwt.t

  val operations:
    t -> int -> (Operation.t list * Operation_list_list_hash.path) Lwt.t
  val all_operations: t -> Operation.t list list Lwt.t

  val watcher: Net.t -> block Lwt_stream.t * Watcher.stopper

end

val read_block:
  global_state -> Block_hash.t -> Block.t option Lwt.t

val read_block_exn:
  global_state -> Block_hash.t -> Block.t Lwt.t

val fork_testnet:
  Block.t -> Protocol_hash.t -> Time.t -> Net.t tzresult Lwt.t

type chain_data = {
  current_head: Block.t ;
  current_reversed_mempool: Operation_hash.t list ;
}

val read_chain_store:
  Net.t ->
  (Store.Chain.store -> chain_data -> 'a Lwt.t) ->
  'a Lwt.t

val update_chain_store:
  Net.t ->
  (Store.Chain.store -> chain_data -> (chain_data option * 'a) Lwt.t) ->
  'a Lwt.t

(** {2 Protocol database} ***************************************************)

module Protocol : sig

  (** Is a value stored in the local database ? *)
  val known: global_state -> Protocol_hash.t -> bool Lwt.t

  (** Read a value in the local database. *)
  val read: global_state -> Protocol_hash.t -> Protocol.t tzresult Lwt.t
  val read_opt: global_state -> Protocol_hash.t -> Protocol.t option Lwt.t
  val read_exn: global_state -> Protocol_hash.t -> Protocol.t Lwt.t

  (** Read a value in the local database (without parsing). *)
  val read_raw: global_state -> Protocol_hash.t -> MBytes.t tzresult Lwt.t
  val read_raw_opt: global_state -> Protocol_hash.t -> MBytes.t option Lwt.t
  val read_raw_exn: global_state -> Protocol_hash.t -> MBytes.t Lwt.t

  val store: global_state -> Protocol.t -> Protocol_hash.t option Lwt.t

  (** Remove a value from the local database. *)
  val remove: global_state -> Protocol_hash.t -> bool Lwt.t

  val list: global_state -> Protocol_hash.Set.t Lwt.t

end

module Registred_protocol : sig

  module type T = sig
    val hash: Protocol_hash.t
    include Updater.NODE_PROTOCOL
    val complete_b58prefix : Context.t -> string -> string list Lwt.t
  end

  type t = (module T)

  val mem: Protocol_hash.t -> bool

  val get: Protocol_hash.t -> t option
  val get_exn: Protocol_hash.t -> t

end

module Register_embedded_protocol
    (Env : Updater.Node_protocol_environment_sigs.V1)
    (Proto : Env.Updater.PROTOCOL)
    (Source : sig
       val hash: Protocol_hash.t option
       val sources: Tezos_data.Protocol.t
     end) : sig end
