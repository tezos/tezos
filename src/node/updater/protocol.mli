(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Protocol Environment - Protocol Implementation Signature *)

(* See `src/proto/updater.mli` for documentation. *)

type fitness = Fitness.fitness

type shell_operation = Store.Operation.shell_header = {
  net_id: Net_id.t ;
}

type raw_operation = Store.Operation.t = {
  shell: shell_operation ;
  proto: MBytes.t ;
}

type shell_block_header = Store.Block_header.shell_header =
  { net_id: Net_id.t ;
    level: Int32.t ;
    proto_level: int ; (* uint8 *)
    predecessor: Block_hash.t ;
    timestamp: Time.t ;
    operations_hash: Operation_list_list_hash.t ;
    fitness: MBytes.t list ;
  }

type raw_block_header = Store.Block_header.t = {
  shell: shell_block_header ;
  proto: MBytes.t ;
}

type validation_result = {
  context: Context.t ;
  fitness: Fitness.fitness ;
  message: string option ;
}

type rpc_context = {
  block_hash: Block_hash.t ;
  block_header: raw_block_header ;
  operation_hashes: unit -> Operation_hash.t list list Lwt.t ;
  operations: unit -> raw_operation list list Lwt.t ;
  context: Context.t ;
}

module type PROTOCOL = sig

  type error = ..
  type 'a tzresult = ('a, error list) result

  val max_operation_data_length : int
  val max_block_length : int
  val max_number_of_operations : int

  type operation

  val parse_operation :
    Operation_hash.t -> raw_operation -> operation tzresult
  val compare_operations : operation -> operation -> int

  type validation_state
  val current_context : validation_state -> Context.t tzresult Lwt.t
  val precheck_block :
    ancestor_context: Context.t ->
    ancestor_timestamp: Time.t ->
    raw_block_header ->
    unit tzresult Lwt.t
  val begin_application :
    predecessor_context: Context.t ->
    predecessor_timestamp: Time.t ->
    predecessor_fitness: Fitness.fitness ->
    raw_block_header ->
    validation_state tzresult Lwt.t
  val begin_construction :
    predecessor_context: Context.t ->
    predecessor_timestamp: Time.t ->
    predecessor_level: Int32.t ->
    predecessor_fitness: Fitness.fitness ->
    predecessor: Block_hash.t ->
    timestamp: Time.t ->
    validation_state tzresult Lwt.t
  val apply_operation :
    validation_state -> operation -> validation_state tzresult Lwt.t
  val finalize_block :
    validation_state -> validation_result tzresult Lwt.t

  val rpc_services : rpc_context RPC.directory

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
