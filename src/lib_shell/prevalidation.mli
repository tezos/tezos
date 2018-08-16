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

type prevalidation_state

val start_prevalidation :
  ?protocol_data: MBytes.t ->
  predecessor: State.Block.t ->
  timestamp: Time.t ->
  unit -> prevalidation_state tzresult Lwt.t

val prevalidate :
  prevalidation_state -> sort:bool ->
  (Operation_hash.t * Operation.t) list ->
  (prevalidation_state * error Preapply_result.t) Lwt.t

val end_prevalidation :
  prevalidation_state ->
  Tezos_protocol_environment_shell.validation_result tzresult Lwt.t

val preapply :
  predecessor:State.Block.t ->
  timestamp:Time.t ->
  protocol_data:MBytes.t ->
  sort_operations:bool ->
  Operation.t list list ->
  (Block_header.shell_header * error Preapply_result.t list) tzresult Lwt.t

val notify_operation :
  prevalidation_state ->
  error Preapply_result.t ->
  unit

val shutdown_operation_input :
  prevalidation_state ->
  unit

val build_rpc_directory :
  Protocol_hash.t ->
  (prevalidation_state * error Preapply_result.t) RPC_directory.t tzresult Lwt.t
