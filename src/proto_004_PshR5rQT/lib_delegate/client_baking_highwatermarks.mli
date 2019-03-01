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

type error += Invalid_chain_id of Chain_id.t
type error += Level_previously_endorsed of Raw_level.t
type error += Level_previously_baked of Raw_level.t

type t

val encoding: t Data_encoding.t

val should_upgrade_blocks_file: #Client_context.full -> bool tzresult Lwt.t
val should_upgrade_endorsements_file: #Client_context.full -> bool tzresult Lwt.t
val upgrade_files: #Client_context.full -> unit tzresult Lwt.t

val may_inject_block:
  #Client_context.full ->
  chain: Chain_services.chain ->
  delegate: Signature.public_key_hash ->
  Raw_level.t ->
  bool tzresult Lwt.t

val may_inject_endorsement:
  #Client_context.full ->
  chain: Chain_services.chain ->
  delegate: Signature.public_key_hash ->
  Raw_level.t ->
  bool tzresult Lwt.t

val record_block:
  #Client_context.full ->
  chain:Chain_services.chain ->
  delegate:Signature.public_key_hash ->
  Raw_level.t ->
  unit tzresult Lwt.t

val record_endorsement:
  #Client_context.full ->
  chain:Chain_services.chain ->
  delegate:Signature.public_key_hash ->
  Raw_level.t ->
  unit tzresult Lwt.t
