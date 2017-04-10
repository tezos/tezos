(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Protocol Environment - Protocol Implementation Signature *)

(** The score of a block as a sequence of as unsigned bytes. Ordered
    by length and then by contents lexicographically. *)
type fitness = Fitness.fitness

(** The version agnostic toplevel structure of operations. *)
type shell_operation = Store.Operation.shell_header = {
  net_id: Net_id.t ;
}

type raw_operation = Store.Operation.t = {
  shell: shell_operation ;
  proto: MBytes.t ;
}

(** The version agnostic toplevel structure of blocks. *)
type shell_block = Store.Block_header.shell_header =
  { net_id: Net_id.t ;
    (** The genesis of the chain this block belongs to. *)
    predecessor: Block_hash.t ;
    (** The preceding block in the chain. *)
    timestamp: Time.t ;
    (** The date at which this block has been forged. *)
    operations: Operation_list_list_hash.t ;
    (** The sequence of operations. *)
    fitness: MBytes.t list ;
    (** The announced score of the block. As a sequence of sequences
        of unsigned bytes. Ordered by length and then by contents
        lexicographically. *)
  }

type raw_block = Store.Block_header.t = {
  shell: shell_block ;
  proto: MBytes.t ;
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
    Operation_hash.t -> raw_operation -> operation tzresult

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
    raw_block ->
    unit tzresult Lwt.t

  (** The first step in a block validation sequence. Initializes a
      validation context for validating a block. Takes as argument the
      {!raw_block} to initialize the context for this block, patching
      the context resulting of the application of the predecessor
      block passed as parameter. The function {!precheck_block} may
      not have been called before [begin_application], so all the
      check performed by the former must be repeated in the latter. *)
  val begin_application :
    predecessor_context: Context.t ->
    predecessor_timestamp: Time.t ->
    raw_block ->
    validation_state tzresult Lwt.t

  (** Initializes a validation context for constructing a new block
      (as opposed to validating an existing block). Since there is no
      {!raw_block} header available, the parts that it provides are
      passed as arguments (predecessor block hash, context resulting
      of the application of the predecessor block, and timestamp). *)
  val begin_construction :
    predecessor_context: Context.t ->
    predecessor_timestamp: Time.t ->
    predecessor: Block_hash.t ->
    timestamp: Time.t ->
    validation_state tzresult Lwt.t

  (** Called after {!begin_application} (or {!begin_construction}) and
      before {!finalize_block}, with each operation in the block. *)
  val apply_operation :
    validation_state -> operation -> validation_state tzresult Lwt.t

  (** The last step in a block validation sequence. It produces the
      context that will be used as input for the validation of its
      successor block candidates. *)
  val finalize_block :
    validation_state -> Context.t tzresult Lwt.t

  (** The list of remote procedures exported by this implementation *)
  val rpc_services : Context.t RPC.directory

  val configure_sandbox :
    Context.t -> Data_encoding.json option -> Context.t tzresult Lwt.t

end

module type PACKED_PROTOCOL = sig
  val hash : Protocol_hash.t
  include PROTOCOL
  val error_encoding : error Data_encoding.t
  val classify_errors : error list -> [ `Branch | `Temporary | `Permanent ]
  val pp : Format.formatter -> error -> unit
  val complete_b58prefix : Context.t -> string -> string list Lwt.t
end
