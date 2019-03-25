(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

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

module Mempool = Block_services.Empty.Mempool

module Blocks : sig

  val list:
    #simple ->
    ?chain:chain ->
    ?heads:Block_hash.t list ->
    ?length:int ->
    ?min_date:Time.Protocol.t ->
    unit -> Block_hash.t list list tzresult Lwt.t

  include (module type of Block_services.Empty)

  type protocols = {
    current_protocol: Protocol_hash.t ;
    next_protocol: Protocol_hash.t ;
  }

  val protocols:
    #RPC_context.simple -> ?chain:chain -> ?block:Block_services.block ->
    unit -> protocols tzresult Lwt.t

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

  module Blocks : sig

    val path: (prefix, prefix) RPC_path.t

    val list:
      ([ `GET ], prefix,
       prefix, < heads : Block_hash.t list;
                 length : int option;
                 min_date : Time.Protocol.t option >, unit,
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
