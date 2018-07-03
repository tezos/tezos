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

open RPC_context

val block:
  #simple ->
  ?async:bool -> ?force:bool -> ?chain:Chain_services.chain ->
  MBytes.t -> Operation.t list list ->
  Block_hash.t tzresult Lwt.t
(** [block cctxt ?async ?force raw_block] tries to inject
    [raw_block] inside the node. If [?async] is [true], [raw_block]
    will be validated before the result is returned. If [?force] is
    true, the block will be injected even on non strictly increasing
    fitness. *)

val operation:
  #simple ->
  ?async:bool -> ?chain:Chain_services.chain ->
  MBytes.t ->
  Operation_hash.t tzresult Lwt.t

val protocol:
  #simple ->
  ?async:bool -> ?force:bool ->
  Protocol.t ->
  Protocol_hash.t tzresult Lwt.t

module S : sig

  val block:
    ([ `POST ], unit,
     unit, < async: bool ;
             force: bool ;
             chain: Chain_services.chain option >, MBytes.t * Operation.t list list,
     Block_hash.t) RPC_service.t

  val operation:
    ([ `POST ], unit,
     unit, < async : bool;
             chain : Chain_services.chain option >, MBytes.t,
     Operation_hash.t) RPC_service.t

  val protocol:
    ([ `POST ], unit,
     unit, < async : bool;
             force : bool >, Protocol.t,
     Protocol_hash.t) RPC_service.t

end
