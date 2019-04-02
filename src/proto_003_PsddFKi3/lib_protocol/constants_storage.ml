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

let preserved_cycles c =
  let constants = Raw_context.constants c in
  constants.preserved_cycles
let blocks_per_cycle c =
  let constants = Raw_context.constants c in
  constants.blocks_per_cycle
let blocks_per_commitment c =
  let constants = Raw_context.constants c in
  constants.blocks_per_commitment
let blocks_per_roll_snapshot c =
  let constants = Raw_context.constants c in
  constants.blocks_per_roll_snapshot
let blocks_per_voting_period c =
  let constants = Raw_context.constants c in
  constants.blocks_per_voting_period
let time_between_blocks c =
  let constants = Raw_context.constants c in
  constants.time_between_blocks
let endorsers_per_block c =
  let constants = Raw_context.constants c in
  constants.endorsers_per_block
let hard_gas_limit_per_operation c =
  let constants = Raw_context.constants c in
  constants.hard_gas_limit_per_operation
let hard_gas_limit_per_block c =
  let constants = Raw_context.constants c in
  constants.hard_gas_limit_per_block
let cost_per_byte c =
  let constants = Raw_context.constants c in
  constants.cost_per_byte
let hard_storage_limit_per_operation c =
  let constants = Raw_context.constants c in
  constants.hard_storage_limit_per_operation
let proof_of_work_threshold c =
  let constants = Raw_context.constants c in
  constants.proof_of_work_threshold
let tokens_per_roll c =
  let constants = Raw_context.constants c in
  constants.tokens_per_roll
let michelson_maximum_type_size c =
  let constants = Raw_context.constants c in
  constants.michelson_maximum_type_size
let seed_nonce_revelation_tip c =
  let constants = Raw_context.constants c in
  constants.seed_nonce_revelation_tip
let origination_size c =
  let constants = Raw_context.constants c in
  constants.origination_size
let block_security_deposit c =
  let constants = Raw_context.constants c in
  constants.block_security_deposit
let endorsement_security_deposit c =
  let constants = Raw_context.constants c in
  constants.endorsement_security_deposit
let block_reward c =
  let constants = Raw_context.constants c in
  constants.block_reward
let endorsement_reward c =
  let constants = Raw_context.constants c in
  constants.endorsement_reward
let parametric c =
  Raw_context.constants c
