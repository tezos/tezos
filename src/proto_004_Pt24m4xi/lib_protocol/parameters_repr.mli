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


(* Only for stitching form proto_003 *)
module Proto_003 : sig
  type parametric = {
    preserved_cycles: int ;
    blocks_per_cycle: int32 ;
    blocks_per_commitment: int32 ;
    blocks_per_roll_snapshot: int32 ;
    blocks_per_voting_period: int32 ;
    time_between_blocks: Period_repr.t list ;
    endorsers_per_block: int ;
    hard_gas_limit_per_operation: Z.t ;
    hard_gas_limit_per_block: Z.t ;
    proof_of_work_threshold: int64 ;
    tokens_per_roll: Tez_repr.t ;
    michelson_maximum_type_size: int;
    seed_nonce_revelation_tip: Tez_repr.t ;
    origination_size: int ;
    block_security_deposit: Tez_repr.t ;
    endorsement_security_deposit: Tez_repr.t ;
    block_reward: Tez_repr.t ;
    endorsement_reward: Tez_repr.t ;
    cost_per_byte: Tez_repr.t ;
    hard_storage_limit_per_operation: Z.t ;
  }

  val constants_encoding : parametric Data_encoding.encoding
end
