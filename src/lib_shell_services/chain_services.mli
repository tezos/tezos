(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type chain = [
  | `Main
  | `Test
  | `Hash of Chain_id.t
]

val parse_chain: string -> (chain, string) result
val to_string: chain -> string

val chain_arg: chain RPC_arg.t

type invalid_block = {
  hash: Block_hash.t ;
  level: Int32.t ;
  errors: error list ;
}

type prefix = unit * chain
val path: (unit, prefix) RPC_path.path

open RPC_context

val chain_id:
  #simple ->
  ?chain:chain ->
  unit -> Chain_id.t tzresult Lwt.t

module Mempool : sig

  val pending_operations:
    #simple ->
    ?chain:chain ->
    unit ->
    (error Preapply_result.t * Operation.t Operation_hash.Map.t) tzresult Lwt.t

end

module Blocks : sig

  val list:
    #simple ->
    ?chain:chain ->
    ?heads:Block_hash.t list ->
    ?length:int ->
    ?min_date:Time.t ->
    unit -> Block_hash.t list list tzresult Lwt.t

end

module Invalid_blocks : sig

  val list:
    #simple ->
    ?chain:chain ->
    unit -> invalid_block list tzresult Lwt.t

  val get:
    #simple ->
    ?chain:chain ->
    Block_hash.t -> invalid_block tzresult Lwt.t

  val delete:
    #simple ->
    ?chain:chain ->
    Block_hash.t -> unit tzresult Lwt.t

end

module S : sig

  val chain_id:
    ([ `GET ], prefix,
     prefix, unit, unit,
     Chain_id.t) RPC_service.t

  module Mempool : sig

    val pending_operations:
      ([ `GET ], prefix,
       prefix , unit, unit,
       error Preapply_result.t * Operation.t Operation_hash.Map.t) RPC_service.t

  end

  module Blocks : sig

    val path: (prefix, prefix) RPC_path.t

    val list:
      ([ `GET ], prefix,
       prefix, < heads : Block_hash.t list;
                 length : int option;
                 min_date : Time.t option >, unit,
       Block_hash.t list list) RPC_service.t

  end

  module Invalid_blocks : sig

    val list:
      ([ `GET ], prefix,
       prefix, unit, unit,
       invalid_block list) RPC_service.t

    val get:
      ([ `GET ], prefix,
       prefix * Block_hash.t, unit, unit,
       invalid_block) RPC_service.t

    val delete:
      ([ `DELETE ], prefix,
       prefix * Block_hash.t, unit, unit,
       unit) RPC_service.t

  end

end
