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

[@@@ocaml.warning "-30"]

type t

type config = {
  genesis: State.Chain.genesis ;
  store_root: string ;
  context_root: string ;
  patch_context: (Context.t -> Context.t Lwt.t) option ;
  p2p: (P2p.config * P2p.limits) option ;
  test_chain_max_tll: int option ;
  checkpoint: (Int32.t * Block_hash.t) option ;
}

and peer_validator_limits = {
  new_head_request_timeout: Time.System.Span.t ;
  block_header_timeout: Time.System.Span.t ;
  block_operations_timeout: Time.System.Span.t ;
  protocol_timeout: Time.System.Span.t ;
  worker_limits: Worker_types.limits
}
and prevalidator_limits = {
  max_refused_operations: int ;
  operation_timeout: Time.System.Span.t ;
  worker_limits : Worker_types.limits ;
}
and block_validator_limits = {
  protocol_timeout: Time.System.Span.t ;
  worker_limits : Worker_types.limits ;
}
and chain_validator_limits = {
  bootstrap_threshold: int ;
  worker_limits : Worker_types.limits ;
}

val default_peer_validator_limits: peer_validator_limits
val default_prevalidator_limits: prevalidator_limits
val default_block_validator_limits: block_validator_limits
val default_chain_validator_limits: chain_validator_limits

val create:
  ?sandboxed:bool ->
  config ->
  peer_validator_limits ->
  block_validator_limits ->
  prevalidator_limits ->
  chain_validator_limits ->
  t tzresult Lwt.t

val shutdown: t -> unit Lwt.t

val build_rpc_directory: t -> unit RPC_directory.t
