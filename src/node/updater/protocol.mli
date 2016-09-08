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

type net_id = Store.net_id = Net of Block_hash.t

(** The version agnostic toplevel structure of operations. *)
type shell_operation = Store.shell_operation = {
  net_id: net_id ;
}

type raw_operation = Store.operation = {
  shell: shell_operation ;
  proto: MBytes.t ;
}

(** The version agnostic toplevel structure of blocks. *)
type shell_block_header = Store.shell_block_header =
  { net_id: net_id ;
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
    (** The sequence of operations. *)
  }

type raw_block_header = Store.block_header = {
  shell: shell_block_header ;
  proto: MBytes.t ;
}

(** Result of the {!PROTOCOL.preapply} function of the protocol for
    discriminating cacheable operations from droppable ones. *)
type 'error preapply_result =
  { applied: Operation_hash.t list;
    (** Operations that where successfully applied. *)
    refused: 'error list Operation_hash_map.t;
    (** Operations which triggered a context independent, unavoidable
        error (e.g. invalid signature). *)
    branch_refused: 'error list Operation_hash_map.t;
    (** Operations which triggered an error that might not arise in a
        different context (e.g. past account counter, insufficent
        balance). *)
    branch_delayed: 'error list Operation_hash_map.t;
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
  type block_header

  (** The maximum size of block headers in bytes *)
  val max_block_header_length : int

  (** The maximum *)
  val max_number_of_operations : int

  (** The parsing / preliminary validation function for blocks. Its
      role is to check that the raw header is well formed, and to
      produce a pre-decomposed value of the high level, protocol defined
      {!block_header} type. It does not have access to the storage
      context. It may store the hash and raw bytes for later signature
      verification by {!apply} or {!preapply}. *)
  val parse_block_header : raw_block_header -> block_header tzresult

  (** The parsing / preliminary validation function for
      operations. Similar to {!parse_block_header}. *)
  val parse_operation :
    Operation_hash.t -> raw_operation -> operation tzresult

  (** The main protocol function that validates blocks. It receives the
      block header and the list of associated operations, as
      pre-decomposed by {!parse_block_header} and {!parse_operation}. *)
  val apply :
    Context.t -> block_header -> operation list ->
    Context.t tzresult Lwt.t

  (** The auxiliary protocol entry point that validates pending
      operations out of blocks. This function tries to apply the all
      operations in the given order, and returns which applications have
      suceeded and which ones have failed. The first three parameters
      are a context in which to apply the operations, the hash of the
      preceding block and the date at which the operations are
      executed. This function is used by the shell for accepting or
      dropping operations, as well as the mining client to check that a
      sequence of operations forms a valid block. *)
  val preapply :
    Context.t -> Block_hash.t -> Time.t -> bool -> operation list ->
    (Context.t * error preapply_result) tzresult Lwt.t

  (** The context rating function to determine the winning block chain. *)
  val fitness :
    Context.t -> fitness Lwt.t

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
end
