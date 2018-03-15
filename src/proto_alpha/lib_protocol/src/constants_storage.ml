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
let cycle_length c =
  let constants = Raw_context.constants c in
  constants.cycle_length
let blocks_per_commitment c =
  let constants = Raw_context.constants c in
  constants.blocks_per_commitment
let block_per_roll_snapshot c =
  let constants = Raw_context.constants c in
  constants.block_per_roll_snapshot
let voting_period_length c =
  let constants = Raw_context.constants c in
  constants.voting_period_length
let slot_durations c =
  let constants = Raw_context.constants c in
  constants.slot_durations
let first_free_baking_slot c =
  let constants = Raw_context.constants c in
  constants.first_free_baking_slot
let max_signing_slot c =
  let constants = Raw_context.constants c in
  constants.max_signing_slot
let max_gas c =
  let constants = Raw_context.constants c in
  constants.max_gas
let proof_of_work_threshold c =
  let constants = Raw_context.constants c in
  constants.proof_of_work_threshold
let dictator_pubkey c =
  let constants = Raw_context.constants c in
  constants.dictator_pubkey
let max_number_of_operations c =
  let constants = Raw_context.constants c in
  constants.max_number_of_operations
let max_operation_data_length c =
  let constants = Raw_context.constants c in
  constants.max_operation_data_length
let token_per_rolls c =
  let constants = Raw_context.constants c in
  constants.token_per_rolls
let michelson_maximum_type_size c =
  let constants = Raw_context.constants c in
  constants.michelson_maximum_type_size
