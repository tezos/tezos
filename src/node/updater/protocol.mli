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

(** Result of the {!PROTOCOL.preapply} function of the protocol for
    discriminating cacheable operations from droppable ones. *)
type 'error preapply_result =
  { applied: Operation_hash.t list;
    (** Operations that where successfully applied. *)
    refused: 'error list Operation_hash.Map.t;
    (** Operations which triggered a context independent, unavoidable
        error (e.g. invalid signature). *)
    branch_refused: 'error list Operation_hash.Map.t;
    (** Operations which triggered an error that might not arise in a
        different context (e.g. past account counter, insufficent
        balance). *)
    branch_delayed: 'error list Operation_hash.Map.t;
    (** Operations which triggered an error that might not arise in a
        future update of this context (e.g. futur account counter). *) }

(** This is the signature of a Tezos protocol implementation. It has
    access to the standard library and the Environment module. *)
module type PROTOCOL = sig

  type error = ..
  type 'a tzresult = ('a, error list) result

  (** The version specific type of operations. *)
  type operation

  (** The maximum size of operations in bytes *)
  val max_operation_data_length : int

  (** The version specific part of blocks. *)
  type block

  (** The maximum size of block headers in bytes *)
  val max_block_length : int

  (** The maximum *)
  val max_number_of_operations : int

  (** The parsing / preliminary validation function for blocks. Its
      role is to check that the raw header is well formed, and to
      produce a pre-decomposed value of the high level, protocol defined
      {!block} type. It does not have access to the storage
      context. It may store the hash and raw bytes for later signature
      verification by {!apply} or {!preapply}. The timestamp of the
      predecessor block is also provided for early delay checks. *)
  val parse_block : raw_block -> Time.t -> block tzresult

  (** The parsing / preliminary validation function for
      operations. Similar to {!parse_block}. *)
  val parse_operation :
    Operation_hash.t -> raw_operation -> operation tzresult

  (** The main protocol function that validates blocks. It receives the
      block header and the list of associated operations, as
      pre-decomposed by {!parse_block} and {!parse_operation}. *)
  val apply :
    Context.t -> block -> operation list ->
    Context.t tzresult Lwt.t

  (** The auxiliary protocol entry point that validates pending
      operations out of blocks. This function tries to apply the all
      operations in the given order, and returns which applications have
      suceeded and which ones have failed. The first two parameters
      are a context in which to apply the operations and the hash of the
      preceding block. This function is used by the shell for accepting or
      dropping operations, as well as the mining client to check that a
      sequence of operations forms a valid block. *)
  val preapply :
    Context.t -> Block_hash.t -> bool -> operation list ->
    (Context.t * error preapply_result) tzresult Lwt.t

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
