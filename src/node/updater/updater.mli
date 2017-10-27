(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* See `src/environment/v1//updater.mli` for documentation. *)

val compile: Protocol_hash.t -> Protocol.t -> bool Lwt.t
val activate: Context.t -> Protocol_hash.t -> Context.t Lwt.t
val fork_test_network:
  Context.t -> protocol:Protocol_hash.t -> expiration:Time.t -> Context.t Lwt.t

val extract: Lwt_io.file_name -> ?hash:Protocol_hash.t -> Protocol.t -> unit Lwt.t
val init: string -> unit

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

module type RAW_PROTOCOL = sig
  type error = ..
  type 'a tzresult = ('a, error list) result
  type operation
  val max_operation_data_length: int
  val max_block_length: int
  val max_number_of_operations: int
  val parse_operation:
    Operation_hash.t -> Operation.t -> operation tzresult
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
  val rpc_services: rpc_context RPC.directory
  val configure_sandbox:
    Context.t -> Data_encoding.json option -> Context.t tzresult Lwt.t
end

(**/**)

(* The end of this file is not exported to the protocol... *)

val compiler_name: string

module Node_protocol_environment_sigs : sig

  module type V1 = sig

    include Tezos_protocol_environment_sigs_v1.T
      with type Format.formatter = Format.formatter
       and type 'a Data_encoding.t = 'a Data_encoding.t
       and type 'a Lwt.t = 'a Lwt.t
       and type ('a, 'b) Pervasives.result = ('a, 'b) result
       and type Hash.Net_id.t = Hash.Net_id.t
       and type Hash.Block_hash.t = Hash.Block_hash.t
       and type Hash.Operation_hash.t = Hash.Operation_hash.t
       and type Hash.Operation_list_list_hash.t = Hash.Operation_list_list_hash.t
       and type Context.t = Context.t
       and type Time.t = Time.t
       and type MBytes.t = MBytes.t
       and type Tezos_data.Operation.shell_header = Tezos_data.Operation.shell_header
       and type Tezos_data.Operation.t = Tezos_data.Operation.t
       and type Tezos_data.Block_header.shell_header = Tezos_data.Block_header.shell_header
       and type Tezos_data.Block_header.t = Tezos_data.Block_header.t
       and type 'a RPC.directory = 'a RPC.directory
       and type Updater.validation_result = validation_result
       and type Updater.rpc_context = rpc_context

    type error += Ecoproto_error of Error_monad.error list
    val wrap_error : 'a Error_monad.tzresult -> 'a tzresult

  end

end

module type NODE_PROTOCOL =
  RAW_PROTOCOL with type error := error
                and type 'a tzresult := 'a tzresult

module LiftProtocol(Name : sig val name: string end)
    (Env : Node_protocol_environment_sigs.V1)
    (P : Env.Updater.PROTOCOL) :
  NODE_PROTOCOL with type operation := P.operation
                and type validation_state := P.validation_state
