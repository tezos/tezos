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
let first_free_baking_slot c =
  let constants = Raw_context.constants c in
  constants.first_free_baking_slot
let endorsers_per_block c =
  let constants = Raw_context.constants c in
  constants.endorsers_per_block
let max_gas c =
  let constants = Raw_context.constants c in
  constants.max_gas
let proof_of_work_threshold c =
  let constants = Raw_context.constants c in
  constants.proof_of_work_threshold
let dictator_pubkey c =
  let constants = Raw_context.constants c in
  constants.dictator_pubkey
let max_operation_data_length c =
  let constants = Raw_context.constants c in
  constants.max_operation_data_length
let tokens_per_roll c =
  let constants = Raw_context.constants c in
  constants.tokens_per_roll
let michelson_maximum_type_size c =
  let constants = Raw_context.constants c in
  constants.michelson_maximum_type_size
let parametric c =
  Raw_context.constants c
