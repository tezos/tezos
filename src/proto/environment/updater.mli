(** Tezos Protocol Environment - Protocol Implementation Updater *)

open Hash
open Tezos_data

type validation_result = {
  context: Context.t ;
  fitness: Fitness.t ;
  message: string option ;
  max_operations_ttl: int ;
}

type rpc_context = {
  block_hash: Block_hash.t ;
  block_header: Block_header.t ;
  operation_hashes: unit -> Operation_hash.t list list Lwt.t ;
  operations: unit -> Operation.t list list Lwt.t ;
  context: Context.t ;
}

(** This is the signature of a Tezos protocol implementation. It has
    access to the standard library and the Environment module. *)
module type PROTOCOL = sig

  type error = ..
  type 'a tzresult = ('a, error list) result

  (** The version specific type of operations. *)
  type operation

  (** The maximum size of operations in bytes *)
  val max_operation_data_length : int

  (** The maximum size of block headers in bytes *)
  val max_block_length : int

  (** The maximum *)
  val max_number_of_operations : int

  (** The parsing / preliminary validation function for
      operations. Similar to {!parse_block}. *)
  val parse_operation :
    Operation_hash.t -> Operation.t -> operation tzresult

  (** Basic ordering of operations. [compare_operations op1 op2] means
      that [op1] should appear before [op2] in a block. *)
  val compare_operations : operation -> operation -> int

  (** A functional state that is transmitted through the steps of a
      block validation sequence. It must retain the current state of
      the store (that can be extracted from the outside using
      {!current_context}, and whose final value is produced by
      {!finalize_block}). It can also contain the information that
      must be remembered during the validation, which must be
      immutable (as validator or baker implementations are allowed to
      pause, replay or backtrack during the validation process). *)
  type validation_state

  (** Access the context at a given validation step. *)
  val current_context : validation_state -> Context.t tzresult Lwt.t

  (** Checks that a block is well formed in a given context. This
      function should run quickly, as its main use is to reject bad
      blocks from the network as early as possible. The input context
      is the one resulting of an ancestor block of same protocol
      version, not necessarily the one of its predecessor. *)
  val precheck_block :
    ancestor_context: Context.t ->
    ancestor_timestamp: Time.t ->
    Block_header.t ->
    unit tzresult Lwt.t

  (** The first step in a block validation sequence. Initializes a
      validation context for validating a block. Takes as argument the
      {!Block_header.t} to initialize the context for this block, patching
      the context resulting of the application of the predecessor
      block passed as parameter. The function {!precheck_block} may
      not have been called before [begin_application], so all the
      check performed by the former must be repeated in the latter. *)
  val begin_application :
    predecessor_context: Context.t ->
    predecessor_timestamp: Time.t ->
    predecessor_fitness: Fitness.t ->
    Block_header.t ->
    validation_state tzresult Lwt.t

  (** Initializes a validation context for constructing a new block
      (as opposed to validating an existing block). Since there is no
      {!Block_header.t} header available, the parts that it provides are
      passed as arguments (predecessor block hash, context resulting
      of the application of the predecessor block, and timestamp). *)
  val begin_construction :
    predecessor_context: Context.t ->
    predecessor_timestamp: Time.t ->
    predecessor_level: Int32.t ->
    predecessor_fitness: Fitness.t ->
    predecessor: Block_hash.t ->
    timestamp: Time.t ->
    ?proto_header: MBytes.t ->
    unit -> validation_state tzresult Lwt.t

  (** Called after {!begin_application} (or {!begin_construction}) and
      before {!finalize_block}, with each operation in the block. *)
  val apply_operation :
    validation_state -> operation -> validation_state tzresult Lwt.t

  (** The last step in a block validation sequence. It produces the
      context that will be used as input for the validation of its
      successor block candidates. *)
  val finalize_block :
    validation_state -> validation_result tzresult Lwt.t

  (** The list of remote procedures exported by this implementation *)
  val rpc_services : rpc_context RPC.directory

  val configure_sandbox :
    Context.t -> Data_encoding.json option -> Context.t tzresult Lwt.t

end

(** Takes a version hash, a list of OCaml components in compilation
    order. The last element must be named [protocol] and respect the
    [protocol.ml] interface. Tries to compile it and returns true
    if the operation was successful. *)
val compile : Protocol_hash.t -> Protocol.t -> bool Lwt.t

(** Activates a given protocol version from a given context. This
    means that the context used for the next block will use this
    version (this is not an immediate change). The version must have
    been previously compiled successfully. *)
val activate : Context.t -> Protocol_hash.t -> Context.t Lwt.t

(** Fork a test network. The forkerd network will use the current block
    as genesis, and [protocol] as economic protocol. The network will
    be destroyed when a (successor) block will have a timestamp greater
    than [expiration]. The protocol must have been previously compiled
    successfully. *)
val fork_test_network:
  Context.t -> protocol:Protocol_hash.t -> expiration:Time.t -> Context.t Lwt.t
