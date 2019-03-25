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

open Proto_alpha
open Alpha_context

type block_info = {
  hash: Block_hash.t ;
  chain_id: Chain_id.t ;
  predecessor: Block_hash.t ;
  fitness: MBytes.t list ;
  timestamp: Time.Protocol.t ;
  protocol: Protocol_hash.t ;
  next_protocol: Protocol_hash.t ;
  proto_level: int ;
  level: Raw_level.t ;
  context : Context_hash.t ;
}

val info:
  #Proto_alpha.rpc_context ->
  ?chain:Chain_services.chain ->
  Block_services.block ->
  block_info tzresult Lwt.t

val monitor_valid_blocks:
  #Proto_alpha.rpc_context ->
  ?chains:Chain_services.chain list ->
  ?protocols:Protocol_hash.t list ->
  next_protocols:Protocol_hash.t list option ->
  unit -> block_info tzresult Lwt_stream.t tzresult Lwt.t

val monitor_heads:
  #Proto_alpha.rpc_context ->
  next_protocols:Protocol_hash.t list option ->
  Chain_services.chain ->
  block_info tzresult Lwt_stream.t tzresult Lwt.t

val blocks_from_current_cycle:
  #Proto_alpha.rpc_context ->
  ?chain:Chain_services.chain ->
  Block_services.block ->
  ?offset:int32 ->
  unit ->
  Block_hash.t list tzresult Lwt.t
