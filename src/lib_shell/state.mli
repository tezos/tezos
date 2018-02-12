(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Shell - Abstraction over all the disk storage.

    It encapsulates access to:

    - the index of validation contexts; and
    - the persistent state of the node:
      - the blockchain and its alternate heads of a "network";
      - the pool of pending operations of a "network". *)

type t
type global_state = t

(** Read the internal state of the node and initialize
    the databases. *)
val read:
  ?patch_context:(Context.t -> Context.t Lwt.t) ->
  store_root:string ->
  context_root:string ->
  unit ->
  global_state tzresult Lwt.t

val close:
  global_state -> unit Lwt.t

(** {2 Errors} **************************************************************)

type error +=
  | Unknown_network of Net_id.t


(** {2 Network} ************************************************************)

(** Data specific to a given network (e.g the mainnet or the current
    test network).  *)
module Net : sig

  type t
  type net_state = t

  (** The chain starts from a genesis block associated to a seed protocol *)
  type genesis = {
    time: Time.t ;
    block: Block_hash.t ;
    protocol: Protocol_hash.t ;
  }
  val genesis_encoding: genesis Data_encoding.t

  (** Initialize a network for a given [genesis]. By default,
      the network does accept forking test network. When
      [~allow_forked_network:true] is provided, test network are allowed. *)
  val create:
    global_state ->
    ?allow_forked_network:bool ->
    genesis -> net_state Lwt.t

  (** Look up for a network by the hash of its genesis block. *)
  val get: global_state -> Net_id.t -> net_state tzresult Lwt.t

  (** Returns all the known networks. *)
  val all: global_state -> net_state list Lwt.t

  (** Destroy a network: this completly removes from the local storage all
      the data associated to the network (this includes blocks and
      operations). *)
  val destroy: global_state -> net_state -> unit Lwt.t

  (** Various accessors. *)
  val id: net_state -> Net_id.t
  val genesis: net_state -> genesis
  val global_state: net_state -> global_state

  (** Hash of the faked block header of the genesis block. *)
  val faked_genesis_hash: net_state -> Block_hash.t

  (** Return the expiration timestamp of a test netwowk. *)
  val expiration: net_state -> Time.t option
  val allow_forked_network: net_state -> bool

end

(** {2 Block database} *****************************************************)

module Block : sig

  type t
  type block = t

  val known: Net.t -> Block_hash.t -> bool Lwt.t
  val known_valid: Net.t -> Block_hash.t -> bool Lwt.t
  val known_invalid: Net.t -> Block_hash.t -> bool Lwt.t
  val read_invalid: Net.t -> Block_hash.t -> Store.Block.invalid_block option Lwt.t
  val list_invalid: Net.t -> (Block_hash.t * int32 * error list) list Lwt.t
  val unmark_invalid: Net.t -> Block_hash.t -> unit tzresult Lwt.t

  val read: Net.t -> Block_hash.t -> block tzresult Lwt.t
  val read_opt: Net.t -> Block_hash.t -> block option Lwt.t
  val read_exn: Net.t -> Block_hash.t -> block Lwt.t

  type error += Inconsistent_hash of Context_hash.t * Context_hash.t

  val store:
    ?dont_enforce_context_hash:bool ->
    Net.t ->
    Block_header.t ->
    Operation.t list list ->
    Updater.validation_result ->
    block option tzresult Lwt.t

  val store_invalid:
    Net.t ->
    Block_header.t ->
    error list ->
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
  val message: t -> string option
  val max_operations_ttl: t -> int
  val max_operation_data_length: t -> int

  val is_genesis: t -> bool
  val predecessor: t -> block option Lwt.t
  val predecessor_n: Net.t -> Block_hash.t -> int -> Block_hash.t option Lwt.t

  val context: t -> Context.t Lwt.t
  val protocol_hash: t -> Protocol_hash.t Lwt.t
  val test_network: t -> Test_network_status.t Lwt.t

  val operation_hashes:
    t -> int ->
    (Operation_hash.t list * Operation_list_list_hash.path) Lwt.t
  val all_operation_hashes: t -> Operation_hash.t list list Lwt.t

  val operations:
    t -> int -> (Operation.t list * Operation_list_list_hash.path) Lwt.t
  val all_operations: t -> Operation.t list list Lwt.t

  val watcher: Net.t -> block Lwt_stream.t * Lwt_watcher.stopper

end

val read_block:
  global_state -> Block_hash.t -> Block.t option Lwt.t

val read_block_exn:
  global_state -> Block_hash.t -> Block.t Lwt.t

val compute_locator: Net.t -> ?size:int -> Block.t -> Block_locator.t Lwt.t

val fork_testnet:
  Block.t -> Protocol_hash.t -> Time.t -> Net.t tzresult Lwt.t

type chain_data = {
  current_head: Block.t ;
  current_mempool: Mempool.t ;
  live_blocks: Block_hash.Set.t ;
  live_operations: Operation_hash.Set.t ;
  locator: Block_locator.t Lwt.t lazy_t ;
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

  include (module type of (struct include Protocol end))

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

module Current_mempool : sig

  val get: Net.t -> (Block_header.t * Mempool.t) Lwt.t
  (** The current mempool. *)

  val set: Net.t -> head:Block_hash.t -> Mempool.t -> unit Lwt.t
  (** Set the current mempool. It is ignored if the current head is
      not the provided one. *)

end

