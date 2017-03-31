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

    - the index of validation contexts; and
    - the persistent state of the node:
      - the blockchain and its alternate heads of a "network";
      - the pool of pending operations of a "network".

 *)
type t
type global_state = t

(** Read the internal state of the node and initialize
    the blocks/operations/contexts databases. *)

val read:
  ?patch_context:(Context.t -> Context.t Lwt.t) ->
  store_root:string ->
  context_root:string ->
  unit ->
  global_state tzresult Lwt.t


(** {2 Errors} **************************************************************)

type error +=
  | Invalid_fitness of { block: Block_hash.t ;
                         expected: Fitness.fitness ;
                         found: Fitness.fitness }
  | Invalid_operations of { block: Block_hash.t ;
                            expected: Operation_list_list_hash.t ;
                            found: Operation_hash.t list list }
  | Unknown_network of Net_id.t
  | Unknown_operation of Operation_hash.t
  | Unknown_block of Block_hash.t
  | Unknown_protocol of Protocol_hash.t
  | Cannot_parse


(** {2 Network} ************************************************************)

(** Data specific to a given network. *)
module Net : sig

  type t
  type net = t

  type genesis = {
    time: Time.t ;
    block: Block_hash.t ;
    protocol: Protocol_hash.t ;
  }
  val genesis_encoding: genesis Data_encoding.t

  (** Initialize a network for a given [genesis]. By default the network
      never expirate and the test_protocol is the genesis protocol. *)
  val create:
    global_state ->
    ?test_protocol: Protocol_hash.t ->
    ?forked_network_ttl: int ->
    genesis -> net Lwt.t

  (** Look up for a network by the hash of its genesis block. *)
  val get: global_state -> Net_id.t -> net tzresult Lwt.t

  (** Returns all the known networks. *)
  val all: global_state -> net list Lwt.t

  (** Destroy a network: this completly removes from the local storage all
      the data associated to the network (this includes blocks and
      operations). *)
  val destroy: global_state -> net -> unit Lwt.t

  (** Accessors. Respectively access to;
      - the network id (the hash of its genesis block)
      - its optional expiration time
      - the associated global state. *)
  val id: net -> Net_id.t
  val genesis: net -> genesis
  val expiration: net -> Time.t option
  val forked_network_ttl: net -> Int64.t option

end

(** Shared signature for the databases of block_headers,
    operations and protocols. *)
module type DATA_STORE = sig

  type store
  type key
  type value

  (** Is a value stored in the local database ? *)
  val known: store -> key -> bool Lwt.t

  (** Read a value in the local database. *)
  val read: store -> key -> value tzresult Lwt.t
  val read_opt: store -> key -> value option Lwt.t
  val read_exn: store -> key -> value Lwt.t

  (** Read a value in the local database (without parsing). *)
  val read_raw: store -> key -> MBytes.t tzresult Lwt.t
  val read_raw_opt: store -> key -> MBytes.t option Lwt.t
  val read_raw_exn: store -> key -> MBytes.t Lwt.t

  (** Read data discovery time (the time when `store` was called). *)
  val read_discovery_time: store -> key -> Time.t tzresult Lwt.t
  val read_discovery_time_opt: store -> key -> Time.t option Lwt.t
  val read_discovery_time_exn: store -> key -> Time.t Lwt.t

  (** Store a value in the local database (pre-parsed value).  It
      returns [false] when the value is already stored, or [true]
      otherwise. For a given value, only one call to `store` (or an
      equivalent call to `store_raw`) might return [true].  *)
  val store: store -> key -> value -> bool Lwt.t

  (** Store a value in the local database (unparsed data).  It returns
      [Ok None] when the data is already stored, or [Ok (Some (hash,
      value))] otherwise. For a given data, only one call to
      `store_raw` (or an equivalent call to `store`) might return [Ok
      (Some _)]. It may return [Error] when the shell part of the value
      cannot be parsed. *)
  val store_raw: store -> key -> MBytes.t -> value option tzresult Lwt.t

  (** Remove a value from the local database. *)
  val remove: store -> key -> bool Lwt.t

end


(** {2 Block_header database} *************************************************)

module Block_header : sig

  type shell_header = Store.Block_header.shell_header = {
    net_id: Net_id.t ;
    predecessor: Block_hash.t ;
    timestamp: Time.t ;
    operations: Operation_list_list_hash.t ;
    fitness: MBytes.t list ;
  }

  type t = Store.Block_header.t = {
    shell: shell_header ;
    proto: MBytes.t ;
  }
  type block_header = t

  include DATA_STORE with type store = Net.t
                      and type key = Block_hash.t
                      and type value = block_header

  val mark_invalid: Net.t -> Block_hash.t -> error list -> bool Lwt.t

  val invalid: Net.t -> Block_hash.t -> error list option Lwt.t
  val pending: Net.t -> Block_hash.t -> bool Lwt.t

  val list_pending: Net.t -> Block_hash.Set.t Lwt.t
  val list_invalid: Net.t -> Block_hash.Set.t Lwt.t

  module Helpers : sig

    (** If [h1] is an ancestor of [h2] in the current [state],
        then [path state h1 h2] returns the chain of block from
        [h1] (excluded) to [h2] (included). *)
    val path:
      Net.t -> Block_hash.t -> Block_hash.t ->
      (Block_hash.t * shell_header) list tzresult Lwt.t

    (** [common_ancestor state h1 h2] returns the first common ancestors
        in the history of blocks [h1] and [h2]. *)
    val common_ancestor:
      Net.t -> Block_hash.t -> Block_hash.t ->
      (Block_hash.t * shell_header) tzresult Lwt.t

    (** [block_locator state max_length h] compute the sparse block locator
        (/à la/ Bitcoin) for the block [h]. *)
    val block_locator:
      Net.t -> int -> Block_hash.t -> Block_hash.t list tzresult Lwt.t

    (** [iter_predecessors state blocks f] iter [f] on [blocks] and
        their recursive (known) predecessors. Blocks are visited with a
        decreasing fitness (then decreasing timestamp). If the optional
        argument [max] is provided, the iteration is stopped after [max]
        visited block. If [min_fitness] id provided, blocks with a
        fitness lower than [min_fitness] are ignored. If [min_date],
        blocks with a fitness lower than [min_date] are ignored. *)
    val iter_predecessors:
      Net.t ->
      ?max:int ->
      ?min_fitness:Fitness.fitness ->
      ?min_date:Time.t ->
      block_header list ->
      f:(block_header -> unit Lwt.t) ->
      unit tzresult Lwt.t

  end

end

module Operation_list : sig

  type store = Net.t
  type key = Block_hash.t * int
  type value = Operation_hash.t list * Operation_list_list_hash.path

  val known: store -> key -> bool Lwt.t
  val read: store -> key -> value tzresult Lwt.t
  val read_opt: store -> key -> value option Lwt.t
  val read_exn: store -> key -> value Lwt.t
  val store: store -> key -> value -> bool Lwt.t
  val remove: store -> key -> bool Lwt.t

  val read_count: store -> Block_hash.t -> int tzresult Lwt.t
  val read_count_opt: store -> Block_hash.t -> int option Lwt.t
  val read_count_exn: store -> Block_hash.t -> int Lwt.t
  val store_count: store -> Block_hash.t -> int -> unit Lwt.t

  val read_all:
    store -> Block_hash.t -> Operation_hash.t list list tzresult Lwt.t
  val store_all:
    store -> Block_hash.t -> Operation_hash.t list  list -> unit Lwt.t

end


(** {2 Valid block} ***********************************************************)

(** The local database of known-valid blocks. *)
module Valid_block : sig

  (** A validated block. *)
  type t = private {
    net_id: Net_id.t ;
    (** The genesis of the chain this block belongs to. *)
    hash: Block_hash.t ;
    (** The block hash. *)
    pred: Block_hash.t ;
    (** The preceding block in the chain. *)
    timestamp: Time.t ;
    (** The date at which this block has been forged. *)
    fitness: Protocol.fitness ;
    (** The (validated) score of the block. *)
    operations_hash: Operation_list_list_hash.t ;
    operations: Operation_hash.t list list ;
    (** The sequence of operations ans its (Merkle-)hash. *)
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
    test_network: (Net_id.t * Time.t) option ;
    (** The current test network associated to the block, and the date
        of its expiration date. *)
    context: Context.t ;
    (** The validation context that was produced by the block validation. *)
    successors: Block_hash.Set.t ;
    invalid_successors: Block_hash.Set.t ;
    (** The set of valid successors (including forked networks). *)
    proto_header: MBytes.t;
    (** The uninterpreted protocol dependent part of the header. *)
  }
  type valid_block = t

  val known: Net.t -> Block_hash.t -> bool Lwt.t
  val read: Net.t -> Block_hash.t -> valid_block tzresult Lwt.t
  val read_opt: Net.t -> Block_hash.t -> valid_block option Lwt.t
  val read_exn: Net.t -> Block_hash.t -> valid_block Lwt.t
  val store:
    Net.t -> Block_hash.t -> Context.t -> valid_block option tzresult Lwt.t

  val watcher: Net.t -> valid_block Lwt_stream.t * Watcher.stopper

  (** The known valid heads of the network's blockchain. *)
  val known_heads: Net.t -> valid_block list Lwt.t

  val fork_testnet:
    global_state -> Net.t -> valid_block -> Time.t -> Net.t tzresult Lwt.t

  module Current : sig

    (** The genesis block of the network's blockchain. On a test network,
        the test protocol has been promoted as "main" protocol. *)
    val genesis: Net.t -> valid_block Lwt.t

    (** The current head of the network's blockchain. *)
    val head: Net.t -> valid_block Lwt.t

    (** The current protocol of the network's blockchain. *)
    val protocol: Net.t -> (module Updater.REGISTRED_PROTOCOL) Lwt.t

    (** Record a block as the current head of the network's blockchain. *)
    val set_head: Net.t -> valid_block -> unit Lwt.t

    val mem: Net.t -> Block_hash.t -> bool Lwt.t

    (** Atomically change the current head of the network's blockchain.
        This returns [true] whenever the change succeeded, or [false]
        when the current head os not equal to the [old] argument. *)
    val test_and_set_head:
      Net.t -> old:valid_block -> valid_block -> bool Lwt.t

    (** [find_new net locator max_length], where [locator] is a sparse block
        locator (/à la/ Bitcoin), returns the missing block when compared
        with the current branch of [net]. *)
    val find_new:
      Net.t -> Block_hash.t list -> int -> Block_hash.t list tzresult Lwt.t

    val new_blocks:
      Net.t -> from_block:valid_block -> to_block:valid_block ->
      (Block_hash.t * (Block_hash.t * Block_header.shell_header) list) Lwt.t

  end

  module Helpers : sig

    (** If [h1] is an ancestor of [h2] in the current [state],
        then [path state h1 h2] returns the chain of block from
        [h1] (excluded) to [h2] (included). Returns [None] otherwise. *)
    val path:
      Net.t -> valid_block -> valid_block -> valid_block list option Lwt.t

    (** [common_ancestor state h1 h2] returns the first common ancestors
        in the history of blocks [h1] and [h2]. *)
    val common_ancestor:
      Net.t -> valid_block -> valid_block -> valid_block Lwt.t

    (** [block_locator state max_length h] compute the sparse block locator
        (/à la/ Bitcoin) for the block [h]. *)
    val block_locator: Net.t -> int -> valid_block -> Block_hash.t list Lwt.t

    (** [iter_predecessors state blocks f] iter [f] on [blocks] and
        their recursive predecessors. Blocks are visited with a
        decreasing fitness (then decreasing timestamp). If the optional
        argument [max] is provided, the iteration is stopped after [max]
        visited block. If [min_fitness] id provided, blocks with a
        fitness lower than [min_fitness] are ignored. If [min_date],
        blocks with a fitness lower than [min_date] are ignored. *)
    val iter_predecessors:
      Net.t ->
      ?max:int ->
      ?min_fitness:Fitness.fitness ->
      ?min_date:Time.t ->
      valid_block list ->
      f:(valid_block -> unit Lwt.t) ->
      unit tzresult Lwt.t

  end

end


(** {2 Operation database} ****************************************************)

module Operation : sig

  type shell_header = Store.Operation.shell_header = {
    net_id: Net_id.t ;
  }

  type t = Store.Operation.t = {
    shell: shell_header ;
    proto: MBytes.t ;
  }

  include DATA_STORE with type store = Net.t
                      and type key = Operation_hash.t
                      and type value = t

  val mark_invalid: Net.t -> Operation_hash.t -> error list -> bool Lwt.t

  val in_chain: Net.t -> Operation_hash.t -> bool Lwt.t
  val pending: Net.t -> Operation_hash.t -> bool Lwt.t
  val invalid: Net.t -> Operation_hash.t -> error list option Lwt.t

  val list_pending: Net.t -> Operation_hash.Set.t Lwt.t

  val list_invalid: Net.t -> Operation_hash.Set.t Lwt.t

end


(** {2 Protocol database} ***************************************************)

module Protocol : sig
  include DATA_STORE with type store = global_state
                      and type key = Protocol_hash.t
                      and type value = Tezos_compiler.Protocol.t

  val list: global_state -> Protocol_hash.Set.t Lwt.t

  (* val mark_invalid: Net.t -> Protocol_hash.t -> error list -> bool Lwt.t *)
  (* val list_invalid: Net.t -> Protocol_hash.Set.t Lwt.t *)

end

