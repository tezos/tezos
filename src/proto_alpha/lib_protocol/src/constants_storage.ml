(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
let hard_storage_limit_per_block c =
  let constants = Raw_context.constants c in
  constants.hard_storage_limit_per_block
let proof_of_work_threshold c =
  let constants = Raw_context.constants c in
  constants.proof_of_work_threshold
let dictator_pubkey c =
  let constants = Raw_context.constants c in
  constants.dictator_pubkey
let tokens_per_roll c =
  let constants = Raw_context.constants c in
  constants.tokens_per_roll
let michelson_maximum_type_size c =
  let constants = Raw_context.constants c in
  constants.michelson_maximum_type_size
let seed_nonce_revelation_tip c =
  let constants = Raw_context.constants c in
  constants.seed_nonce_revelation_tip
let origination_burn c =
  let constants = Raw_context.constants c in
  constants.origination_burn
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
