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

(***************** Prevalidation errors ***********************************)

type error += Parse_error
type error += Too_many_operations
type error += Oversized_operation of { size: int ; max: int }
type error += Future_block_header of { block: Block_hash.t ;
                                       block_time : Time.Protocol.t ;
                                       time : Time.System.t }

(************************* State errors ***********************************)

type error +=  Unknown_chain of Chain_id.t
type error += Bad_data_dir
type error += Block_not_invalid of Block_hash.t

(* Block database error *)

type error += Inconsistent_hash of Context_hash.t * Context_hash.t

(******************* Bootstrap pipeline errors ****************************)

type error += Invalid_locator of P2p_peer.Id.t * Block_locator.t

(******************* Protocol validator errors ****************************)

type protocol_error =
  | Compilation_failed
  | Dynlinking_failed

type error += Invalid_protocol of { hash: Protocol_hash.t ; error: protocol_error }

(********************* Peer validator errors ******************************)

type error +=
  | Unknown_ancestor
  | Known_invalid

(************************ Validator errors ********************************)

type error +=  Inactive_chain of Chain_id.t
type error += Checkpoint_error of Block_hash.t * P2p_peer.Id.t option
