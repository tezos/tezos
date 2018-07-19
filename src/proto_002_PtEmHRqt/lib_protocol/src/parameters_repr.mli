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

type bootstrap_account = {
  public_key_hash : Signature.Public_key_hash.t ;
  public_key : Signature.Public_key.t option ;
  amount : Tez_repr.t ;
}

type bootstrap_contract = {
  delegate : Signature.Public_key_hash.t ;
  amount : Tez_repr.t ;
  script : Script_repr.t ;
}

type t = {
  bootstrap_accounts : bootstrap_account list ;
  bootstrap_contracts : bootstrap_contract list ;
  commitments : Commitment_repr.t list ;
  constants : Constants_repr.parametric ;
  security_deposit_ramp_up_cycles : int option ;
  no_reward_cycles : int option ;
}

val encoding: t Data_encoding.t
val constants_encoding: Constants_repr.parametric Data_encoding.t
