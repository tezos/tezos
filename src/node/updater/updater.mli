(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type shell_operation = Store.Operation.shell_header = {
  net_id: Net_id.t ;
}
val shell_operation_encoding: shell_operation Data_encoding.t

type raw_operation = Store.Operation.t = {
  shell: shell_operation ;
  proto: MBytes.t ;
}
val raw_operation_encoding: raw_operation Data_encoding.t

type shell_block_header = Store.Block_header.shell_header = {
  net_id: Net_id.t ;
  level: Int32.t ;
  proto_level: int ; (* uint8 *)
  predecessor: Block_hash.t ;
  timestamp: Time.t ;
  operations_hash: Operation_list_list_hash.t ;
  fitness: MBytes.t list ;
}
val shell_block_header_encoding: shell_block_header Data_encoding.t

type raw_block_header = Store.Block_header.t = {
  shell: shell_block_header ;
  proto: MBytes.t ;
}
val raw_block_header_encoding: raw_block_header Data_encoding.t

type validation_result = Protocol.validation_result = {
  context: Context.t ;
  fitness: Fitness.fitness ;
  message: string option ;
}

type rpc_context = Protocol.rpc_context = {
  block_hash: Block_hash.t ;
  block_header: raw_block_header ;
  operation_hashes: unit -> Operation_hash.t list list Lwt.t ;
  operations: unit -> raw_operation list list Lwt.t ;
  context: Context.t ;
}

module type PROTOCOL = Protocol.PROTOCOL
module type REGISTRED_PROTOCOL = sig
  val hash: Protocol_hash.t
  (* exception Ecoproto_error of error list *)
  include Protocol.PROTOCOL with type error := error
                             and type 'a tzresult := 'a tzresult
  val complete_b58prefix : Context.t -> string -> string list Lwt.t
end

type component = Tezos_compiler.Protocol.component = {
  name : string ;
  interface : string option ;
  implementation : string ;
}

val extract: Lwt_io.file_name -> Protocol_hash.t -> component list -> unit Lwt.t
val compile: Protocol_hash.t -> component list -> bool Lwt.t

val activate: Context.t -> Protocol_hash.t -> Context.t Lwt.t
val fork_test_network:
  Context.t -> protocol:Protocol_hash.t -> expiration:Time.t -> Context.t Lwt.t

val register: Protocol_hash.t -> (module REGISTRED_PROTOCOL) -> unit

val get: Protocol_hash.t -> (module REGISTRED_PROTOCOL) option
val get_exn: Protocol_hash.t -> (module REGISTRED_PROTOCOL)

val init: string -> unit
