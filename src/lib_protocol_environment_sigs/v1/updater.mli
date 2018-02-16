(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Protocol Environment - Protocol updater. *)

(** Validation result: the record returned by the protocol
    on the successfull validation of a block. *)
type validation_result = {

  context: Context.t ;
  (** The resulting context, it will be used for the next block. *)

  fitness: Fitness.t ;
  (** The effective fitness of the block (to be compared with
      the 'announced' one in the block header. *)

  message: string option ;
  (** An optional informative message to be used as in the 'git
      commit' of the block's context. *)

  max_operation_data_length: int ;
  (** The maximum size of operations in bytes. *)

  max_operations_ttl: int ;
  (** The "time-to-live" of operation for the next block: any
      operations whose 'branch' is older than 'ttl' blocks in the
      past cannot be included in the next block. *)

}

type quota = {
  max_size: int ;
  (** The maximum size (in bytes) of the serialized list of
      operations. *)
  max_op: int option ;
  (** The maximum number of operation.
      [None] means no limit. *)
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

  (** The maximum size of block headers in bytes. *)
  val max_block_length: int

  (** The number of validation passes (length of the list) and the
      operation's quota for each pass. *)
  val validation_passes: quota list

  (** The version specific type of operations. *)
  type operation

  (** The parsing / preliminary validation function for
      operations. Similar to {!parse_block}. *)
  val parse_operation:
    Operation_hash.t -> Operation.t -> operation tzresult

  (** The Validation passes in which an operation can appear.
      For instance [[0]] if it only belongs to the first pass.
      An answer of [[]] means that the operation is ill-formed
      and cannot be included at all. *)
  val acceptable_passes: operation -> int list

  (** Basic ordering of operations. [compare_operations op1 op2] means
      that [op1] should appear before [op2] in a block. *)
  val compare_operations: operation -> operation -> int

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
  val current_context: validation_state -> Context.t tzresult Lwt.t

  (** Checks that a block is well formed in a given context. This
      function should run quickly, as its main use is to reject bad
      blocks from the chain as early as possible. The input context
      is the one resulting of an ancestor block of same protocol
      version, not necessarily the one of its predecessor. *)
  val precheck_block:
    ancestor_context: Context.t ->
    ancestor_timestamp: Time.t ->
    Block_header.t ->
    unit tzresult Lwt.t

  (** The first step in a block validation sequence. Initializes a
      validation context for validating a block. Takes as argument the
      {!Block_header.t} to initialize the context for this block. The
      function {!precheck_block} may not have been called before
      [begin_application], so all the check performed by the former
      must be repeated in the latter. *)
  val begin_application:
    predecessor_context: Context.t ->
    predecessor_timestamp: Time.t ->
    predecessor_fitness: Fitness.t ->
    Block_header.t ->
    validation_state tzresult Lwt.t

  (** Initializes a validation context for constructing a new block
      (as opposed to validating an existing block). When the
      [protocol_data] argument is specified, it should contains a
      'prototype' of a the protocol specific part of a block header,
      and the function should produce the exact same effect on the
      context than would produce the validation of a block containing
      an "equivalent" (but complete) header. For instance, if the
      block header usually includes a signature, the header provided
      to {!begin_construction} should includes a faked signature. *)
  val begin_construction:
    predecessor_context: Context.t ->
    predecessor_timestamp: Time.t ->
    predecessor_level: Int32.t ->
    predecessor_fitness: Fitness.t ->
    predecessor: Block_hash.t ->
    timestamp: Time.t ->
    ?protocol_data: MBytes.t ->
    unit -> validation_state tzresult Lwt.t

  (** Called after {!begin_application} (or {!begin_construction}) and
      before {!finalize_block}, with each operation in the block. *)
  val apply_operation:
    validation_state -> operation -> validation_state tzresult Lwt.t

  (** The last step in a block validation sequence. It produces the
      context that will be used as input for the validation of its
      successor block candidates. *)
  val finalize_block:
    validation_state -> validation_result tzresult Lwt.t

  (** The list of remote procedures exported by this implementation *)
  val rpc_services: rpc_context Lwt.t RPC_directory.t

  (** An ad-hoc context patcher. It used only for debugging protocol
      while running in the "sandbox" mode. This function is never used
      in production. *)
  val configure_sandbox:
    Context.t -> Data_encoding.json option -> Context.t tzresult Lwt.t

end

(** Takes a version hash, a list of OCaml components in compilation
    order. The last element must be named [protocol] and respect the
    [protocol.ml] interface. Tries to compile it and returns true
    if the operation was successful. *)
val compile: Protocol_hash.t -> Protocol.t -> bool Lwt.t

(** Activates a given protocol version from a given context. This
    means that the context used for the next block will use this
    version (this is not an immediate change). The version must have
    been previously compiled successfully. *)
val activate: Context.t -> Protocol_hash.t -> Context.t Lwt.t

(** Fork a test chain. The forkerd chain will use the current block
    as genesis, and [protocol] as economic protocol. The chain will
    be destroyed when a (successor) block will have a timestamp greater
    than [expiration]. The protocol must have been previously compiled
    successfully. *)
val fork_test_chain:
  Context.t -> protocol:Protocol_hash.t -> expiration:Time.t -> Context.t Lwt.t
