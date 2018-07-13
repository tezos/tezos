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

  max_operations_ttl: int ;
  (** The "time-to-live" of operation for the next block: any
      operations whose 'branch' is older than 'ttl' blocks in the
      past cannot be included in the next block. *)

  last_allowed_fork_level: Int32.t ;
  (** The level of the last block for which the node might consider an
      alternate branch. The shell should consider as invalid any
      branch whose fork point is older than the given level *)

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
  block_header: Block_header.shell_header ;
  context: Context.t ;
}

(** This is the signature of a Tezos protocol implementation. It has
    access to the standard library and the Environment module. *)
module type PROTOCOL = sig

  (** The maximum size of a block header in bytes. *)
  val max_block_length: int

  (** The maximum size of an operation in bytes. *)
  val max_operation_data_length: int

  (** The number of validation passes (length of the list) and the
      operation's quota for each pass. *)
  val validation_passes: quota list

  (** The version specific type of blocks. *)
  type block_header_data

  (** Encoding for version specific part of block headers.  *)
  val block_header_data_encoding: block_header_data Data_encoding.t

  (** A fully parsed block header. *)
  type block_header = {
    shell: Block_header.shell_header ;
    protocol_data: block_header_data ;
  }

  (** Version-specific side information computed by the protocol
      during the validation of a block. Should not include information
      about the evaluation of operations which is handled separately by
      {!operation_metadata}. To be used as an execution trace by tools
      (client, indexer). Not necessary for validation. *)
  type block_header_metadata

  (** Encoding for version-specific block metadata. *)
  val block_header_metadata_encoding: block_header_metadata Data_encoding.t

  (** The version specific type of operations. *)
  type operation_data

  (** Version-specific side information computed by the protocol
      during the validation of each operation, to be used conjointly
      with {!block_header_metadata}. *)
  type operation_receipt

  (** A fully parsed operation. *)
  type operation = {
    shell: Operation.shell_header ;
    protocol_data: operation_data ;
  }

  (** Encoding for version-specific operation data. *)
  val operation_data_encoding: operation_data Data_encoding.t

  (** Encoding for version-specific operation receipts. *)
  val operation_receipt_encoding: operation_receipt Data_encoding.t

  (** Encoding that mixes an operation data and its receipt. *)
  val operation_data_and_receipt_encoding:
    (operation_data * operation_receipt) Data_encoding.t

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
      version. This ancestor of the current head is guaranteed to be
      more recent than `last_allowed_fork_level`.

      The resulting `validation_state` will be used for multi-pass
      validation. *)
  val begin_partial_application:
    chain_id: Chain_id.t ->
    ancestor_context: Context.t ->
    predecessor_timestamp: Time.t ->
    predecessor_fitness: Fitness.t ->
    block_header ->
    validation_state tzresult Lwt.t

  (** The first step in a block validation sequence. Initializes a
      validation context for validating a block. Takes as argument the
      {!Block_header.t} to initialize the context for this block. The
      function {!precheck_block} may not have been called before
      [begin_application], so all the check performed by the former
      must be repeated in the latter. *)
  val begin_application:
    chain_id: Chain_id.t ->
    predecessor_context: Context.t ->
    predecessor_timestamp: Time.t ->
    predecessor_fitness: Fitness.t ->
    block_header ->
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
    chain_id: Chain_id.t ->
    predecessor_context: Context.t ->
    predecessor_timestamp: Time.t ->
    predecessor_level: Int32.t ->
    predecessor_fitness: Fitness.t ->
    predecessor: Block_hash.t ->
    timestamp: Time.t ->
    ?protocol_data: block_header_data ->
    unit -> validation_state tzresult Lwt.t

  (** Called after {!begin_application} (or {!begin_construction}) and
      before {!finalize_block}, with each operation in the block. *)
  val apply_operation:
    validation_state ->
    operation ->
    (validation_state * operation_receipt) tzresult Lwt.t

  (** The last step in a block validation sequence. It produces the
      context that will be used as input for the validation of its
      successor block candidates. *)
  val finalize_block:
    validation_state ->
    (validation_result * block_header_metadata) tzresult Lwt.t

  (** The list of remote procedures exported by this implementation *)
  val rpc_services: rpc_context RPC_directory.t

  (** Initialize the context (or upgrade the context after a protocol
      amendment). This function receives the context resulting of the
      application of a block that triggered the amendment. It also
      receives the header of the block that triggered the amendment. *)
  val init:
    Context.t -> Block_header.shell_header -> validation_result tzresult Lwt.t

end

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
