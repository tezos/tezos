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

(** [wait_for_operation_inclusion chain ~predecessors ~confirmations
    oph] waits for `oph` to appears in the main chain with at least
    `confirmations`. It returns the hash of the block that contains
    the operation and the operation position in the block.

    This functions also looks for the operations in the `predecessors`
    of the intial chain head. *)
val wait_for_operation_inclusion:
  #Client_context.full ->
  chain:Chain_services.chain ->
  ?predecessors:int ->
  ?confirmations:int ->
  ?branch:Block_hash.t ->
  Operation_hash.t ->
  (Block_hash.t * int * int) tzresult Lwt.t

val wait_for_bootstrapped:
  #Client_context.full -> unit tzresult Lwt.t

(** lookup an operation in [predecessors] previous blocks, starting 
    from head *)
val lookup_operation_in_previous_blocks:
  #Client_context.full ->
  chain:Block_services.chain ->
  predecessors:int ->
  Operation_list_hash.elt ->
  (Block_hash.t * int * int) option tzresult Lwt.t
