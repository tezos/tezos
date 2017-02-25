(** Tezos Protocol Environment - Protocol Implementation Updater *)

open Hash

module Net_id : sig
  type t
  val encoding : t Data_encoding.t
end

type shell_operation = {
  net_id: Net_id.t ;
}
val shell_operation_encoding: shell_operation Data_encoding.t

type raw_operation = {
  shell: shell_operation ;
  proto: MBytes.t ;
}


(** The version agnostic toplevel structure of blocks. *)
type shell_block = {
  net_id: Net_id.t ;
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
val shell_block_encoding: shell_block Data_encoding.t

type raw_block = {
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
    access to the Environment module. *)
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
      verification by {!apply} or {!preapply}. *)
  val parse_block : raw_block -> block tzresult

  (** The parsing / preliminary validation function for
      operations. Similar to {!parse_block}. *)
  val parse_operation :
    Operation_hash.t -> raw_operation -> operation tzresult

  (** The main protocol function that validates blocks. It receives the
      block header and the list of associated operations, as
      pre-decomposed by {!parse_block} and {!parse_operation}. *)
  val apply :
    Context.t -> block -> operation list -> Context.t tzresult Lwt.t

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

  (** The list of remote procedures exported by this implementation *)
  val rpc_services : Context.t RPC.directory

  val configure_sandbox :
    Context.t -> Data_encoding.json option -> Context.t tzresult Lwt.t

end

(** An OCaml source component of a protocol implementation. *)
type component = {
  (** The OCaml module name. *)
  name : string ;
  (** The OCaml interface source code *)
  interface : string option ;
  (** The OCaml source code *)
  implementation : string ;
}

(** Takes a version hash, a list of OCaml components in compilation
    order. The last element must be named [protocol] and respect the
    [protocol.ml] interface. Tries to compile it and returns true
    if the operation was successful. *)
val compile : Protocol_hash.t -> component list -> bool Lwt.t

(** Activates a given protocol version from a given context. This
    means that the context used for the next block will use this
    version (this is not an immediate change). The version must have
    been previously compiled successfully. *)
val activate : Context.t -> Protocol_hash.t -> Context.t Lwt.t

val set_test_protocol: Context.t -> Protocol_hash.t -> Context.t Lwt.t
val fork_test_network: Context.t -> Context.t Lwt.t
