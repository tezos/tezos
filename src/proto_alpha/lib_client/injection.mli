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
open Apply_results

type 'kind preapply_result =
  Operation_hash.t * 'kind operation * 'kind operation_metadata

type fee_parameter = {
  minimal_fees: Tez.t ;
  minimal_nanotez_per_byte: Z.t ;
  minimal_nanotez_per_gas_unit: Z.t ;
  force_low_fee: bool ;
  fee_cap: Tez.t ;
  burn_cap: Tez.t ;
}

val dummy_fee_parameter: fee_parameter

val preapply:
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?verbose_signing:bool ->
  ?fee_parameter:fee_parameter ->
  ?branch:int ->
  ?src_sk:Client_keys.sk_uri ->
  'kind contents_list ->
  'kind preapply_result tzresult Lwt.t

type 'kind result_list =
  Operation_hash.t * 'kind contents_list * 'kind contents_result_list

val inject_operation:
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?branch:int ->
  ?src_sk:Client_keys.sk_uri ->
  ?verbose_signing:bool ->
  fee_parameter:fee_parameter ->
  ?compute_fee:bool ->
  'kind contents_list ->
  'kind result_list tzresult Lwt.t

type 'kind result =
  Operation_hash.t * 'kind contents * 'kind contents_result

val inject_manager_operation:
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?branch:int ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?verbose_signing:bool ->
  source:Contract.t ->
  src_pk:Signature.public_key ->
  src_sk:Client_keys.sk_uri ->
  ?fee:Tez.t ->
  ?gas_limit:Z.t ->
  ?storage_limit:Z.t ->
  ?counter:Z.t ->
  fee_parameter:fee_parameter ->
  'kind manager_operation ->
  'kind Kind.manager result tzresult Lwt.t

val originated_contracts:
  'kind contents_result_list -> Contract.t list tzresult
