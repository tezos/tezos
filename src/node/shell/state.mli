(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** An abstraction over all the disk storage used by the node.

    It encapsulates access to:

    - the (distributed) database of raw blocks and operations;
    - the index of validation contexts; and
    - the persistent state of the node:
      - active "networks";
      - the blockchain and its alternate heads of a "network";
      - the pool of pending operations of a "network".

 *)
type t
type state = t

(** A "network" identifier. Here, a "network" denotes an independant
    blockchain, or a "fork" of another blockchain. Such a "network"
    is identified by the hash of its genesis block. *)
type net_id = Store.net_id = Net of Block_hash.t

type error +=
  | Invalid_fitness of Fitness.fitness * Fitness.fitness
  | Unknown_protocol of Protocol_hash.t
  | Inactive_network of Store.net_id
  | Unknown_network of Store.net_id
  | Cannot_parse

(** Read the internal state of the node and initialize
    the blocks/operations/contexts databases. *)
val read:
  request_operations: (net_id -> Operation_hash.t list -> unit) ->
  request_blocks: (net_id -> Block_hash.t list -> unit) ->
  request_protocols: (Protocol_hash.t list -> unit) ->
  store_root:string ->
  context_root:string ->
  ttl:int ->
  ?patch_context:(Context.t -> Context.t Lwt.t) ->
  unit ->
  state Lwt.t

(** Store the internal state of the node on disk. *)
val store: state -> unit Lwt.t

(** Shutdown the various databases worker and store the
    internal state of the node on disk. *)
val shutdown: state -> unit Lwt.t


(** {2 Operation database} ****************************************************)

(** The local and distributed database of operations. *)
module Operation : sig

  type key = Operation_hash.t

  (** Raw operations in the database (partially parsed). *)
  type shell_header = Store.shell_operation = {
    net_id: net_id ;
    (** The genesis of the chain this operation belongs to. *)
  }
  type t = Store.operation = {
    shell: shell_header ;
    proto: MBytes.t ;
    (** The raw part of the operation, as understood only by the protocol. *)
  }
  type operation = t

  (** Is an operation stored in the local database ? *)
  val known: state -> key -> bool Lwt.t

  (** Read an operation in the local database. This returns [None]
      when the operation does not exist in the local database; this returns
      [Some (Error _)] when [mark_invalid] was used. This also returns
      the time when the operation was stored on the local database. *)
  val read:
    state -> key -> operation tzresult Time.timed_data option Lwt.t

  (** Read an operation in the local database. This throws [Not_found]
      when the operation does not exist in the local database or when
      [mark_invalid] was used. *)
  val read_exn:
    state -> key -> operation Time.timed_data Lwt.t
  exception Invalid of key * error list

  (** Read an operation in the local database (without parsing). *)
  val raw_read: state -> key -> MBytes.t option Lwt.t

  (** Read an operation from the distributed database. This may block
      while the block is fetched from the P2P network. *)
  val fetch:
    state -> Store.net_id -> key -> operation tzresult Time.timed_data Lwt.t

  (** Request operations on the P2P network without waiting for answers. *)
  val prefetch: state -> Store.net_id -> key list -> unit

  (** Add an operation to the local database. This returns [Ok None]
      if the operation was already stored in the database, or returns
      the parsed operation if not. It may also fails when the shell
      part of the operation cannot be parsed or when the operation
      does not belong to an active "network". For a given sequence of
      bytes, it is guaranted that at most one call to [store] returns
      [Some _]. *)
  val store:
    state -> MBytes.t -> (Operation_hash.t * operation) option tzresult Lwt.t

  (** Mark an operation as invalid in the local database. This returns
      [false] if then operation was previously stores in the local
      database. The operation is not removed from the local database,
      but its content is replaced by the an list of errors. *)
  val mark_invalid: state -> key -> error list -> bool Lwt.t

  (** Returns the list known-invalid operations. *)
  val invalid: state -> Operation_hash_set.t Lwt.t

  (** Create a stream of all the newly locally-stored operations.
      The returned function allows to terminate the stream. *)
  val create_watcher:
      state -> (key * operation) Lwt_stream.t * Watcher.stopper

end

(** {2 Block database} ********************************************************)

(** The local and distributed database of blocks. *)
module Block : sig

  type shell_header = Store.shell_block = {
    net_id: net_id ;
    (** The genesis of the chain this block belongs to. *)
    predecessor: Block_hash.t ;
    (** The preceding block in the chain. *)
    timestamp: Time.t ;
    (** The date at which this block has been forged. *)
    fitness: MBytes.t list ;
    (** The announced score of the block. As a sequence of sequences
        of unsigned bytes. Ordered by length and then by contents
        lexicographically. *)
    operations: Operation_hash.t list ;
    (** The raw part of the block header, as understood only by the protocol. *)
  }
  type t = Store.block = {
    shell: shell_header ;
    proto: MBytes.t ;
  }
  type block = t

  (** Is a block stored in the local database ? *)
  val known: state -> Block_hash.t -> bool Lwt.t

  (** Read a block in the local database. *)
  val read: state -> Block_hash.t -> block Time.timed_data option Lwt.t

  (** Read a block in the local database. This throws [Not_found]
      when the block does not exist in the local database. *)
  val read_exn: state -> Block_hash.t -> block Time.timed_data Lwt.t

  (** Read the predecessor of a block in the local database. *)
  val read_pred: state -> Block_hash.t -> Block_hash.t option Lwt.t

  (** Read a block in the local database (without parsing). *)
  val raw_read: state -> Block_hash.t -> MBytes.t option Lwt.t

  (** Read a block from the distributed database. This may block
      while the block is fetched from the P2P network. *)
  val fetch: state -> Store.net_id -> Block_hash.t -> block Time.timed_data Lwt.t

  (** Request blocks on the P2P network without waiting for answers. *)
  val prefetch: state -> Store.net_id -> Block_hash.t list -> unit

  (** Add a block to the local database. This returns [Ok None] if the
      block was already stored in the database, or returns the
      (partially) parsed block if not. It may also fails when the
      shell part of the block cannot be parsed or when the block does
      not belong to an active "network". For a given sequence of
      bytes, it is guaranted that at most one call to [store] returns
      [Some _]. *)
  val store:
    state -> MBytes.t -> (Block_hash.t * block) option tzresult Lwt.t

  (** Create a stream of all the newly locally-stored blocks.
      The returned function allows to terminate the stream. *)
  val create_watcher:
    state -> (Block_hash.t * block) Lwt_stream.t * Watcher.stopper

  (** If [h1] is an ancestor of [h2] in the current [state],
      then [path state h1 h2] returns the chain of block from
      [h1] (excluded) to [h2] (included). *)
  val path:
    state -> Block_hash.t -> Block_hash.t -> Block_hash.t list tzresult Lwt.t

  (** [common_ancestor state h1 h2] returns the first common ancestors
      in the history of blocks [h1] and [h2]. *)
  val common_ancestor:
    state -> Block_hash.t -> Block_hash.t -> Block_hash.t tzresult Lwt.t

  (** [block_locator state max_length h] compute the sparse block locator
      (/à la/ Bitcoin) for the block [h]. *)
  val block_locator:
    state -> int -> Block_hash.t -> Block_hash.t list tzresult Lwt.t

  (** [iter_predecessors state blocks f] iter [f] on [blocks] and
      their recursive (known) predecessors. Blocks are visited with a
      decreasing fitness (then decreasing timestamp). If the optional
      argument [max] is provided, the iteration is stopped after [max]
      visited block. If [min_fitness] id provided, blocks with a
      fitness lower than [min_fitness] are ignored. If [min_date],
      blocks with a fitness lower than [min_date] are ignored. *)
  val iter_predecessors:
    state ->
    ?max:int ->
    ?min_fitness:Fitness.fitness ->
    ?min_date:Time.t ->
    block list ->
    f:(block -> unit Lwt.t) ->
    unit tzresult Lwt.t

end

(** {2 Valid block} ***********************************************************)

(** The local database of known-valid blocks. *)
module Valid_block : sig

  (** A previously validated block. *)
  type t = private {
    net_id: net_id ;
    (** The genesis of the chain this block belongs to. *)
    hash: Block_hash.t ;
    (** The block hash. *)
    pred: Block_hash.t ;
    (** The preceding block in the chain. *)
    timestamp: Time.t ;
    (** The date at which this block has been forged. *)
    fitness: Protocol.fitness ;
    (** The (validated) score of the block. *)
    operations: Operation_hash.t list ;
    (** The sequence of operations. *)
    discovery_time: Time.t ;
    (** The data at which the block was discorevered on the P2P network. *)
    protocol_hash: Protocol_hash.t ;
    (** The protocol to be used for validating the following blocks. *)
    protocol: (module Updater.REGISTRED_PROTOCOL) option ;
    (** The actual implementation of the protocol to be used for
        validating the following blocks. *)
    test_protocol_hash: Protocol_hash.t ;
    (** The protocol to be used for the next test network. *)
    test_protocol: (module Updater.REGISTRED_PROTOCOL) option ;
    (** The actual implementatino of the protocol to be used for the
        next test network. *)
    test_network: (net_id * Time.t) option ;
    (** The current test network associated to the block, and the date
        of its expiration date. *)
    context: Context.t ;
    (** The validation context that was produced by the block validation. *)
    successors: Block_hash_set.t ;
    (** The set of valid successors (including forked networks). *)
    invalid_successors: Block_hash_set.t ;
    (** The set of invalid successors (including forked networks). *)
  }
  type valid_block = t

  (** Is the block known as a valid block in the database ? *)
  val valid: state -> Block_hash.t -> bool Lwt.t

  (** Is the block known in the database (valid or invalid) ? *)
  val known: state -> Block_hash.t -> bool Lwt.t

  (** Read a block in the database. This returns [None] when
      the block did not get trough the validation process yet. This
      returns [Error] if the block is known invalid or [Ok] otherwise. *)
  val read: state -> Block_hash.t -> valid_block tzresult option Lwt.t

  (** Read a block in the database. This throws [Not_found] when
      the block did not get trough the validation process yet. This
      throws [Invalid] if the block is known invalid. *)
  val read_exn: state -> Block_hash.t -> valid_block Lwt.t
  exception Invalid of Block_hash.t * error list

  (** Returns all the known (validated) heads of all the known block chain.
      (This includes the main blockchain and the non-expired test networks. *)
  val known_heads: state -> valid_block Block_hash_map.t Lwt.t

  (** Returns all the known blocks that not did get through the validator yet. *)
  val postponed: state -> Block_hash_set.t Lwt.t

  (** Returns all the known blocks whose validation failed. *)
  val invalid: state -> Block_hash_set.t Lwt.t

  (** Create a stream of all the newly validated blocks.
      The returned function allows to terminate the stream. *)
  val create_watcher: state -> (valid_block Lwt_stream.t * Watcher.stopper) Lwt.t

  (** If [h1] is an ancestor of [h2] in the current [state],
      then [path state h1 h2] returns the chain of block from
      [h1] (excluded) to [h2] (included). Returns [None] otherwise. *)
  val path:
    state -> valid_block -> valid_block -> valid_block list option Lwt.t

  (** [common_ancestor state h1 h2] returns the first common ancestors
      in the history of blocks [h1] and [h2]. *)
  val common_ancestor:
    state -> valid_block -> valid_block -> valid_block Lwt.t

  (** [block_locator state max_length h] compute the sparse block locator
      (/à la/ Bitcoin) for the block [h]. *)
  val block_locator: state -> int -> valid_block -> Block_hash.t list Lwt.t

  (** [iter_predecessors state blocks f] iter [f] on [blocks] and
      their recursive predecessors. Blocks are visited with a
      decreasing fitness (then decreasing timestamp). If the optional
      argument [max] is provided, the iteration is stopped after [max]
      visited block. If [min_fitness] id provided, blocks with a
      fitness lower than [min_fitness] are ignored. If [min_date],
      blocks with a fitness lower than [min_date] are ignored. *)
  val iter_predecessors:
    state ->
    ?max:int ->
    ?min_fitness:Fitness.fitness ->
    ?min_date:Time.t ->
    valid_block list ->
    f:(valid_block -> unit Lwt.t) ->
    unit tzresult Lwt.t

  (**/**)

  (* Store function to be used by the validator. *)
  module Store : Persist.TYPED_STORE with type key = Block_hash.t
                                      and type value = Context.t tzresult
  val get_store: state -> Store.t Persist.shared_ref

  (* Private interface for testing. *)
  val store: state -> Block_hash.t -> Context.t -> valid_block tzresult Lwt.t
  val store_invalid: state -> Block_hash.t -> error list -> bool Lwt.t

end

(** {2 Protocol database} ****************************************************)

(** The local and distributed database of protocols. *)
module Protocol : sig

  type key = Protocol_hash.t

  type component = Tezos_compiler.Protocol.component = {
    name : string ;
    interface : string option ;
    implementation : string ;
  }

  type t = Tezos_compiler.Protocol.t

  type protocol = t

  (** Is a protocol stored in the local database ? *)
  val known: state -> key -> bool Lwt.t

  (** Read a protocol in the local database. This returns [None]
      when the protocol does not exist in the local database; this returns
      [Some (Error _)] when [mark_invalid] was used. This also returns
      the time when the protocol was stored on the local database. *)
  val read:
    state -> key -> protocol tzresult Time.timed_data option Lwt.t

  (** Read a protocol in the local database. This throws [Not_found]
      when the protocol does not exist in the local database or when
      [mark_invalid] was used. *)
  val read_exn:
    state -> key -> protocol Time.timed_data Lwt.t
  exception Invalid of key * error list

  (** Read an operation in the local database (without parsing). *)
  val raw_read: state -> key -> MBytes.t option Lwt.t

  (** Read a protocol from the distributed database. This may block
      while the block is fetched from the P2P network. *)
  val fetch:
    state -> Store.net_id -> key -> protocol tzresult Time.timed_data Lwt.t

  (** Request protocols on the P2P network without waiting for answers. *)
  val prefetch: state -> Store.net_id -> key list -> unit

  (** Add a protocol to the local database. This returns [Ok None]
      if the protocol was already stored in the database, or returns
      the parsed operation if not. It may also fails when the shell
      part of the operation cannot be parsed or when the operation
      does not belong to an active "network". For a given sequence of
      bytes, it is guaranted that at most one call to [store] returns
      [Some _]. *)
  val store:
    state -> MBytes.t -> (Protocol_hash.t * protocol) option tzresult Lwt.t

  (** Mark a protocol as invalid in the local database. This returns
      [false] if the protocol was previously stored in the local
      database. The protocol is not removed from the local database,
      but its content is replaced by a list of errors. *)
  val mark_invalid: state -> key -> error list -> bool Lwt.t

  (** Returns the list known-invalid procols. *)
  val invalid: state -> Protocol_hash_set.t Lwt.t

  (** Create a stream of all the newly locally-stored protocols.
      The returned function allows to terminate the stream. *)
  val create_watcher:
    state -> (key * protocol) Lwt_stream.t * Watcher.stopper

  val keys: state -> key list Lwt.t
end

(** {2 Network} ****************************************************************)

(** Data specific to a given network. *)
module Net : sig

  type t
  type net = t

  (** Initialize a network for a given [genesis]. It may fails if the
      genesis block is a known invalid block. By default the network
      never expirate and the test_protocol is the genesis protocol.
      When the genesis block correspond to a valid block where
      the "test_network" is set to be this genesis block, the test protocol
      will be promoted as validation protocol(in this forked network only). *)
  val create:
    state -> ?expiration:Time.t -> ?test_protocol:Protocol_hash.t ->
    Store.genesis -> net tzresult Lwt.t

  (** Look up for a network by the hash of its genesis block. *)
  val get: state -> net_id -> net tzresult

  (** Returns all the known networks. *)
  val all: state -> net list

  (** Destroy a network: this completly removes from the local storage all
      the data associated to the network (this includes blocks and
      operations). *)
  val destroy: net -> unit Lwt.t

  (** Accessors. Respectively access to;
      - the network id (the hash of its genesis block)
      - its optional expiration time
      - the associated global state. *)
  val id: net -> net_id
  val expiration: net -> Time.t option
  val state: net -> state

  (** Mark a network as active or inactive. Newly discovered blocks and
      operations on inactive networks are ignored. *)
  val activate: net -> unit
  val deactivate: net -> unit

  (** Return the list of active network. *)
  val active: state -> net list

  (** Test whether a network is active or not. *)
  val is_active: state -> net_id -> bool

  (** {3 Blockchain} ************************************************************)

  module Blockchain : sig

    (** The genesis block of the network's blockchain. On a test network,
        the test protocol has been promoted as "main" protocol. *)
    val genesis: net -> Valid_block.t Lwt.t

    (** The current head of the network's blockchain. *)
    val head: net -> Valid_block.t Lwt.t

    (** The current protocol of the network's blockchain. *)
    val protocol: net -> (module Updater.REGISTRED_PROTOCOL) Lwt.t

    (** Record a block as the current head of the network's blockchain. *)
    val set_head: net -> Valid_block.t -> unit Lwt.t

    (** Atomically change the current head of the network's blockchain.
        This returns [true] whenever the change succeeded, or [false]
        when the current head os not equal to the [old] argument. *)
    val test_and_set_head:
        net -> old:Valid_block.t -> Valid_block.t -> bool Lwt.t

    (** Test whether a block belongs to the current branch of the network's
        blockchain. *)
    val mem: net -> Block_hash.t -> bool Lwt.t

    (** [find_new net locator max_length], where [locator] is a sparse block
        locator (/à la/ Bitcoin), returns the missing block when compared
        with the current branch of [net]. *)
    val find_new:
        net -> Block_hash.t list -> int -> Block_hash.t list tzresult Lwt.t

  end

  (** {3 Mempool} *************************************************************)

  (** The mempool contains the known not-trivially-invalid operations
      that are not yet included in the blockchain. *)
  module Mempool : sig

    (** Returns the current mempool of the network. *)
    val get: net -> Operation_hash_set.t Lwt.t

    (** Add an operation to the mempool. *)
    val add: net -> Operation_hash.t -> bool Lwt.t

    (** Remove an operation from the mempool. *)
    val remove: net -> Operation_hash.t -> bool Lwt.t

    (** Returns a sur-approximation to the mempool for an alternative
        head in the blockchain. *)
    val for_block: net -> Valid_block.t -> Operation_hash_set.t Lwt.t

  end

end
