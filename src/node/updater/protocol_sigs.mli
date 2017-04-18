(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Tezos Protocol Environment - Protocol Implementation Signature *)

open Tezos_data

(* See `src/proto/updater.mli` for documentation. *)

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

module type PROTOCOL = sig

  type error = ..
  type 'a tzresult = ('a, error list) result

  val max_operation_data_length : int
  val max_block_length : int
  val max_number_of_operations : int

  type operation

  val parse_operation :
    Operation_hash.t -> Operation.t -> operation tzresult
  val compare_operations : operation -> operation -> int

  type validation_state
  val current_context : validation_state -> Context.t tzresult Lwt.t
  val precheck_block :
    ancestor_context: Context.t ->
    ancestor_timestamp: Time.t ->
    Block_header.t ->
    unit tzresult Lwt.t
  val begin_application :
    predecessor_context: Context.t ->
    predecessor_timestamp: Time.t ->
    predecessor_fitness: Fitness.t ->
    Block_header.t ->
    validation_state tzresult Lwt.t
  val begin_construction :
    predecessor_context: Context.t ->
    predecessor_timestamp: Time.t ->
    predecessor_level: Int32.t ->
    predecessor_fitness: Fitness.t ->
    predecessor: Block_hash.t ->
    timestamp: Time.t ->
    ?proto_header: MBytes.t ->
    unit -> validation_state tzresult Lwt.t
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
