(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Make(Context : Protocol_environment.CONTEXT) = struct

  type validation_result = {
    context: Context.t ;
    fitness: Fitness.t ;
    message: string option ;
    max_operation_data_length: int ;
    max_operations_ttl: int ;
  }

  type quota = {
    max_size: int ;
    max_op: int option ;
  }

  type rpc_context = {
    block_hash: Block_hash.t ;
    block_header: Block_header.t ;
    operation_hashes: unit -> Operation_hash.t list list Lwt.t ;
    operations: unit -> Operation.t list list Lwt.t ;
    context: Context.t ;
  }

  module type RAW_PROTOCOL = sig
    type error = ..
    type 'a tzresult = ('a, error list) result
    val max_block_length: int
    val validation_passes: quota list
    type operation
    val parse_operation:
      Operation_hash.t -> Operation.t -> operation tzresult
    val acceptable_passes: operation -> int list
    val compare_operations: operation -> operation -> int
    type validation_state
    val current_context: validation_state -> Context.t tzresult Lwt.t
    val precheck_block:
      ancestor_context: Context.t ->
      ancestor_timestamp: Time.t ->
      Block_header.t ->
      unit tzresult Lwt.t
    val begin_application:
      predecessor_context: Context.t ->
      predecessor_timestamp: Time.t ->
      predecessor_fitness: Fitness.t ->
      Block_header.t ->
      validation_state tzresult Lwt.t
    val begin_construction:
      predecessor_context: Context.t ->
      predecessor_timestamp: Time.t ->
      predecessor_level: Int32.t ->
      predecessor_fitness: Fitness.t ->
      predecessor: Block_hash.t ->
      timestamp: Time.t ->
      ?proto_header: MBytes.t ->
      unit -> validation_state tzresult Lwt.t
    val apply_operation:
      validation_state -> operation -> validation_state tzresult Lwt.t
    val finalize_block:
      validation_state -> validation_result tzresult Lwt.t
    val rpc_services: rpc_context RPC_directory.t
    val configure_sandbox:
      Context.t -> Data_encoding.json option -> Context.t tzresult Lwt.t
  end

  let compile _ _ = assert false
  let activate _ _ = assert false
  let fork_test_network _ ~protocol:_ ~expiration:_ = assert false

end
