(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type net = State.net_id = Net of Block_hash.t

val errors: unit -> Json_schema.schema Lwt.t
val forge_block:
  ?net:Updater.net_id ->
  ?predecessor:Block_hash.t ->
  ?timestamp:Time.t ->
  Fitness.fitness ->
  Operation_hash.t list ->
  MBytes.t ->
  MBytes.t Lwt.t

val validate_block: net -> Block_hash.t -> unit tzresult Lwt.t
val inject_block:
  ?wait:bool -> ?force:bool -> MBytes.t ->
  Block_hash.t tzresult Lwt.t
val inject_operation:
  ?wait:bool -> ?force:bool -> MBytes.t -> Operation_hash.t tzresult Lwt.t

module Blocks : sig

  type block = [
    | `Genesis
    | `Head of int | `Prevalidation
    | `Test_head of int | `Test_prevalidation
    | `Hash of Block_hash.t
    ]

  val net: block -> net Lwt.t
  val predecessor: block -> Block_hash.t Lwt.t
  val hash: block -> Block_hash.t Lwt.t
  val timestamp: block -> Time.t Lwt.t
  val fitness: block -> MBytes.t list Lwt.t
  val operations: block -> Operation_hash.t list Lwt.t
  val protocol: block -> Protocol_hash.t Lwt.t
  val test_protocol: block -> Protocol_hash.t option Lwt.t
  val test_network: block -> (net * Time.t) option Lwt.t

  val pending_operations:
    block -> (error Updater.preapply_result * Operation_hash_set.t) Lwt.t

  type block_info = {
    hash: Block_hash.t ;
    predecessor: Block_hash.t ;
    fitness: MBytes.t list ;
    timestamp: Time.t ;
    protocol: Protocol_hash.t option ;
    operations: Operation_hash.t list option ;
    net: net ;
    test_protocol: Protocol_hash.t option ;
    test_network: (net * Time.t) option ;
  }

  val info:
    ?operations:bool -> block -> block_info Lwt.t

  val list:
    ?operations:bool -> ?length:int -> ?heads:Block_hash.t list -> ?delay:int ->
    unit -> block_info list list Lwt.t

  val monitor:
    ?operations:bool -> ?length:int -> ?heads:Block_hash.t list -> ?delay:int ->
    unit -> block_info list list Lwt_stream.t Lwt.t

  type preapply_result = {
    operations: error Updater.preapply_result ;
    fitness: MBytes.t list ;
    timestamp: Time.t ;
  }

  val preapply:
    block ->
    ?timestamp:Time.t ->
    ?sort:bool ->
    Hash.Operation_hash.t list -> preapply_result tzresult Lwt.t

end

module Operations : sig
  val monitor:
    ?contents:bool -> unit ->
    (Operation_hash.t * Store.operation option) list Lwt_stream.t Lwt.t
end

val describe: ?recurse:bool -> string list -> RPC.Description.directory_descr Lwt.t

(** Low-level *)

val get_json: string list -> Data_encoding.json -> Data_encoding.json Lwt.t

val call_service0:
  (unit, unit, 'i, 'o) RPC.service -> 'i -> 'o Lwt.t
val call_service1:
  (unit, unit * 'a, 'i, 'o) RPC.service -> 'a -> 'i -> 'o Lwt.t
val call_service2:
  (unit, (unit * 'a) * 'b, 'i, 'o) RPC.service -> 'a -> 'b -> 'i -> 'o Lwt.t
