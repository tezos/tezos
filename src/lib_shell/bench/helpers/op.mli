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

val endorsement:
  ?delegate:public_key_hash ->
  ?level:Raw_level.t ->
  Context.t -> ?signing_context:Context.t ->
  int list -> Kind.endorsement Operation.t tzresult Lwt.t

val miss_signed_endorsement:
  ?level:Raw_level.t ->
  Context.t -> int -> Kind.endorsement Operation.t tzresult Lwt.t

val transaction:
  ?fee:Tez.tez ->
  ?gas_limit:Z.t ->
  ?storage_limit:Z.t ->
  ?parameters:Script.lazy_expr ->
  Context.t ->
  Contract.t ->
  Contract.t ->
  Tez.t ->
  Operation.packed tzresult Lwt.t

val delegation:
  ?fee:Tez.tez -> Context.t ->
  Contract.t -> public_key_hash option ->
  Operation.packed tzresult Lwt.t

val revelation:
  Context.t -> public_key -> Operation.packed tzresult Lwt.t

val origination:
  ?delegate:public_key_hash ->
  ?script:Script.t ->
  ?spendable:bool ->
  ?delegatable:bool ->
  ?preorigination: Contract.contract option ->
  ?public_key:public_key ->
  ?manager:public_key_hash ->
  ?credit:Tez.tez ->
  ?fee:Tez.tez ->
  ?gas_limit:Z.t ->
  ?storage_limit:Z.t ->
  Context.t ->
  Contract.contract ->
  (Operation.packed * Contract.contract) tzresult Lwt.t

val originated_contract:
  Operation.packed -> Contract.contract

val double_endorsement:
  Context.t ->
  Kind.endorsement Operation.t ->
  Kind.endorsement Operation.t ->
  Operation.packed tzresult Lwt.t

val double_baking:
  Context.t ->
  Block_header.block_header ->
  Block_header.block_header ->
  Operation.packed tzresult Lwt.t

val activation:
  Context.t ->
  Signature.Public_key_hash.t -> Blinded_public_key_hash.activation_code ->
  Operation.packed tzresult Lwt.t
