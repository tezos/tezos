(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* See `src/environment/v1/updater.mli` for documentation. *)

val compile: Protocol_hash.t -> Protocol.t -> bool Lwt.t
val activate: Context.t -> Protocol_hash.t -> Context.t Lwt.t
val fork_test_network:
  Context.t -> protocol:Protocol_hash.t -> expiration:Time.t -> Context.t Lwt.t

val init: string -> unit

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

(* The end of this file is not exported to the protocol... *)

val compiler_name: string

module type NODE_PROTOCOL = Protocol_environment.T
  with type context := Context.t
   and type validation_result := validation_result
   and type quota := quota
   and type rpc_context := rpc_context
   and type 'a tzresult := 'a tzresult

module Node_protocol_environment_sigs : sig

  module type V1 = sig

    include Protocol_environment.V1
      with type Context.t = Context.t
       and type Updater.validation_result = validation_result
       and type Updater.quota = quota
       and type Updater.rpc_context = rpc_context

  end

end

module MakeV1(Name : sig val name: string end)() :
  Node_protocol_environment_sigs.V1
