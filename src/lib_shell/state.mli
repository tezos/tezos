(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Tezos Shell - Abstraction over all the disk storage.

    It encapsulates access to:

    - the index of validation contexts; and
    - the persistent state of the node:
      - the blockchain and its alternate heads ;
      - the pool of pending operations of a chain. *)

type t
type global_state = t

(** {2 Network} *)

(** Data specific to a given chain (e.g the main chain or the current
    test chain).  *)
module Chain : sig

  type t
  type chain_state = t

  (** The chain starts from a genesis block associated to a seed protocol *)
  type genesis = {
    time: Time.Protocol.t ;
    block: Block_hash.t ;
    protocol: Protocol_hash.t ;
  }
  val genesis_encoding: genesis Data_encoding.t

  (** Initialize a chain for a given [genesis]. By default,
      the chain does accept forking test chain. When
      [~allow_forked_chain:true] is provided, test chain are allowed. *)
  val create:
    global_state ->
    ?allow_forked_chain:bool ->
    genesis ->
    Chain_id.t ->
    chain_state Lwt.t

  (** Look up for a chain by the hash of its genesis block. *)
  val get: global_state -> Chain_id.t -> chain_state tzresult Lwt.t
  val get_exn: global_state -> Chain_id.t -> chain_state Lwt.t

  val main: global_state -> Chain_id.t
  val test: chain_state -> Chain_id.t option Lwt.t

  (** Returns all the known chains. *)
  val all: global_state -> chain_state list Lwt.t

  (** Destroy a chain: this completly removes from the local storage all
      the data associated to the chain (this includes blocks and
      operations). *)
  val destroy: global_state -> chain_state -> unit Lwt.t

  (** Various accessors. *)
  val id: chain_state -> Chain_id.t
  val genesis: chain_state -> genesis
  val global_state: chain_state -> global_state

  (** Hash of the faked block header of the genesis block. *)
  val faked_genesis_hash: chain_state -> Block_hash.t

  (** Return the expiration timestamp of a test chain. *)
  val expiration: chain_state -> Time.Protocol.t option
  val allow_forked_chain: chain_state -> bool

  val checkpoint: chain_state -> (Int32.t * Block_hash.t) Lwt.t

  (** Update the current checkpoint. The current head should be
      consistent (i.e. it should either have a lower level or pass
      through the checkpoint). In the process all the blocks from
      invalid alternate heads are removed from the disk, either
      completely (when `level <= checkpoint`) or still tagged as
      invalid (when `level > checkpoint`). *)
  val set_checkpoint:
    chain_state ->
    Int32.t * Block_hash.t ->
    unit Lwt.t

  (** Check that a block is compatible with the current checkpoint.
      This function assumes that the predecessor is known valid. *)
  val acceptable_block:
    chain_state ->
    Block_hash.t -> Block_header.t ->
    bool Lwt.t

end

(** {2 Block header manipulation} *)


(** {2 Block database} *)

module Block : sig

  type t
  type block = t

  type validation_store = {
    context_hash: Context_hash.t ;
    message: string option ;
    max_operations_ttl: int ;
    last_allowed_fork_level: Int32.t ;
  }

  val known: Chain.t -> Block_hash.t -> bool Lwt.t
  val known_valid: Chain.t -> Block_hash.t -> bool Lwt.t
  val known_invalid: Chain.t -> Block_hash.t -> bool Lwt.t
  val read_invalid: Chain.t -> Block_hash.t -> Store.Block.invalid_block option Lwt.t
  val list_invalid: Chain.t -> (Block_hash.t * int32 * error list) list Lwt.t
  val unmark_invalid: Chain.t -> Block_hash.t -> unit tzresult Lwt.t

  val read: Chain.t -> ?pred:int -> Block_hash.t -> block tzresult Lwt.t
  val read_opt: Chain.t -> ?pred:int -> Block_hash.t -> block option Lwt.t

  val store:
    ?dont_enforce_context_hash:bool ->
    Chain.t ->
    Block_header.t -> MBytes.t ->
    Operation.t list list -> MBytes.t list list ->
    validation_store ->
    forking_testchain: bool ->
    block option tzresult Lwt.t

  val store_invalid:
    Chain.t ->
    Block_header.t ->
    error list ->
    bool tzresult Lwt.t

  module Header : sig
    type t
    type block_header = t

    val known: Chain.t -> Block_hash.t -> bool Lwt.t

    val read: Chain.t -> ?pred:int -> Block_hash.t -> block_header tzresult Lwt.t
    val read_opt: Chain.t -> ?pred:int -> Block_hash.t -> block_header option Lwt.t
    val read_exn: Chain.t -> ?pred:int -> Block_hash.t -> block_header Lwt.t
    val of_block: block -> block_header
    val to_block: Chain.t -> block_header -> block option Lwt.t

    val compare: t -> t -> int
    val equal: t -> t -> bool

    val hash: t -> Block_hash.t
    val header: t -> Block_header.t
    val shell_header: t -> Block_header.shell_header
    val timestamp: t -> Time.Protocol.t
    val fitness: t -> Fitness.t
    val validation_passes: t -> int
    val level: t -> Int32.t

    val all_operation_hashes: Chain.t -> block_header -> Operation_hash.t list list Lwt.t

    val predecessor : Chain.t -> block_header -> block_header option Lwt.t
    val predecessor_n : Chain.t -> Block_hash.t -> int -> Block_hash.t option Lwt.t

  end


  val compare: t -> t -> int
  val equal: t -> t -> bool

  val hash: t -> Block_hash.t
  val header: t -> Block_header.t
  val shell_header: t -> Block_header.shell_header
  val timestamp: t -> Time.Protocol.t
  val fitness: t -> Fitness.t
  val validation_passes: t -> int
  val chain_id: t -> Chain_id.t
  val chain_state: t -> Chain.t
  val level: t -> Int32.t
  val message: t -> string option
  val max_operations_ttl: t -> int
  val metadata: t -> MBytes.t
  val last_allowed_fork_level: t -> Int32.t

  val is_genesis: t -> bool
  val predecessor: t -> block option Lwt.t
  val predecessor_n: t -> int -> Block_hash.t option Lwt.t

  val is_valid_for_checkpoint: t -> (Int32.t * Block_hash.t) -> bool Lwt.t

  val context: t -> Context.t Lwt.t
  val protocol_hash: t -> Protocol_hash.t Lwt.t
  val test_chain: t -> (Test_chain_status.t * t option) Lwt.t

  val operation_hashes:
    t -> int ->
    (Operation_hash.t list * Operation_list_list_hash.path) Lwt.t
  val all_operation_hashes: t -> Operation_hash.t list list Lwt.t

  val operations:
    t -> int -> (Operation.t list * Operation_list_list_hash.path) Lwt.t
  val all_operations: t -> Operation.t list list Lwt.t

  val operations_metadata:
    t -> int -> MBytes.t list Lwt.t
  val all_operations_metadata: t -> MBytes.t list list Lwt.t

  val watcher: Chain.t -> block Lwt_stream.t * Lwt_watcher.stopper

  val known_ancestor:
    Chain.t -> Block_locator.t -> (block * Block_locator.t) option Lwt.t
  (** [known_ancestor chain_state locator] computes the first block of
      [locator] that is known to be a valid block. It also computes the
      'prefix' of [locator] with end at the first valid block.  The
      function returns [None] when no block in the locator are known or
      if the first known block is invalid. *)

  val get_rpc_directory: block -> block RPC_directory.t option Lwt.t
  val set_rpc_directory: block -> block RPC_directory.t -> unit Lwt.t

end

val read_block:
  global_state -> ?pred:int -> Block_hash.t -> Block.t option Lwt.t

val read_block_exn:
  global_state -> ?pred:int -> Block_hash.t -> Block.t Lwt.t

val watcher: t -> Block.t Lwt_stream.t * Lwt_watcher.stopper

(** Computes the block with the best fitness amongst the known blocks
    which are compatible with the given checkpoint. *)
val best_known_head_for_checkpoint:
  Chain.t ->
  Int32.t * Block_hash.t ->
  Block.t Lwt.t

val compute_locator: Chain.t -> ?size:int -> Block.t -> Block_locator.seed -> Block_locator.t Lwt.t

val update_testchain:
  Block.t ->
  testchain_state: Chain.t ->
  unit Lwt.t

val fork_testchain:
  Block.t ->
  Chain_id.t -> Block_hash.t -> Block_header.t ->
  Protocol_hash.t -> Time.Protocol.t -> Chain.t tzresult Lwt.t

type chain_data = {
  current_head: Block.t ;
  current_mempool: Mempool.t ;
  live_blocks: Block_hash.Set.t ;
  live_operations: Operation_hash.Set.t ;
  test_chain: Chain_id.t option ;
}

val read_chain_data:
  Chain.t ->
  (Store.Chain_data.store -> chain_data -> 'a Lwt.t) ->
  'a Lwt.t

val update_chain_data:
  Chain.t ->
  (Store.Chain_data.store -> chain_data -> (chain_data option * 'a) Lwt.t) ->
  'a Lwt.t

(** {2 Protocol database} *)

module Protocol : sig

  include (module type of (struct include Protocol end))

  (** Is a value stored in the local database ? *)
  val known: global_state -> Protocol_hash.t -> bool Lwt.t

  (** Read a value in the local database. *)
  val read: global_state -> Protocol_hash.t -> Protocol.t tzresult Lwt.t
  val read_opt: global_state -> Protocol_hash.t -> Protocol.t option Lwt.t

  (** Read a value in the local database (without parsing). *)
  val read_raw: global_state -> Protocol_hash.t -> MBytes.t tzresult Lwt.t
  val read_raw_opt: global_state -> Protocol_hash.t -> MBytes.t option Lwt.t

  val store: global_state -> Protocol.t -> Protocol_hash.t option Lwt.t

  (** Remove a value from the local database. *)
  val remove: global_state -> Protocol_hash.t -> bool Lwt.t

  val list: global_state -> Protocol_hash.Set.t Lwt.t

  val watcher: global_state -> Protocol_hash.t Lwt_stream.t * Lwt_watcher.stopper

end

module Current_mempool : sig

  val get: Chain.t -> (Block_header.t * Mempool.t) Lwt.t
  (** The current mempool. *)

  val set: Chain.t -> head:Block_hash.t -> Mempool.t -> unit Lwt.t
  (** Set the current mempool. It is ignored if the current head is
      not the provided one. *)

end

(** Read the internal state of the node and initialize
    the databases. *)
val init:
  ?patch_context:(Context.t -> Context.t Lwt.t) ->
  ?store_mapsize:int64 ->
  ?context_mapsize:int64 ->
  store_root:string ->
  context_root:string ->
  Chain.genesis ->
  (global_state * Chain.t * Context.index) tzresult Lwt.t

val close:
  global_state -> unit Lwt.t
