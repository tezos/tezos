(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* See `src/proto/updater.mli` for documentation. *)

type validation_result = Protocol_sigs.validation_result = {
  context: Context.t ;
  fitness: Fitness.t ;
  message: string option ;
  max_operations_ttl: int ;
}

type rpc_context = Protocol_sigs.rpc_context = {
  block_hash: Block_hash.t ;
  block_header: Block_header.t ;
  operation_hashes: unit -> Operation_hash.t list list Lwt.t ;
  operations: unit -> Operation.t list list Lwt.t ;
  context: Context.t ;
}

module type PROTOCOL = Protocol_sigs.PROTOCOL
module type PACKED_PROTOCOL = Protocol_sigs.PACKED_PROTOCOL
module type REGISTRED_PROTOCOL = sig
  val hash: Protocol_hash.t
  (* exception Ecoproto_error of error list *)
  include PROTOCOL with type error := error
                             and type 'a tzresult := 'a tzresult
  val complete_b58prefix : Context.t -> string -> string list Lwt.t
end

val extract: Lwt_io.file_name -> Protocol_hash.t -> Protocol.t -> unit Lwt.t
val compile: Protocol_hash.t -> Protocol.t -> bool Lwt.t

val activate: Context.t -> Protocol_hash.t -> Context.t Lwt.t
val fork_test_network:
  Context.t -> protocol:Protocol_hash.t -> expiration:Time.t -> Context.t Lwt.t

val register: Protocol_hash.t -> (module REGISTRED_PROTOCOL) -> unit

val get: Protocol_hash.t -> (module REGISTRED_PROTOCOL) option
val get_exn: Protocol_hash.t -> (module REGISTRED_PROTOCOL)

val init: string -> unit
