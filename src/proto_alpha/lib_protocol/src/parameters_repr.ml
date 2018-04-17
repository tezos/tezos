(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type bootstrap_account = {
  public_key : Signature.Public_key.t ;
  amount : Tez_repr.t ;
}

type t = {
  bootstrap_accounts : bootstrap_account list ;
  commitments : (Unclaimed_public_key_hash.t * Commitment_repr.t) list ;
  constants : Constants_repr.parametric ;
  security_deposit_ramp_up_cycles : int option ;
}

let bootstrap_account_encoding =
  let open Data_encoding in
  conv
    (fun { public_key ; amount } -> (public_key, amount))
    (fun (public_key, amount) -> { public_key ; amount })
    (tup2 Signature.Public_key.encoding Tez_repr.encoding)

(* This encoding is used to read configuration files (e.g. sandbox.json)
   where some fields can be missing, in that case they are replaced by
   the default. *)
let constants_encoding =
  let open Data_encoding in
  conv
    (fun (c : Constants_repr.parametric) ->
       let module Compare_time_between_blocks = Compare.List (Period_repr) in
       let module Compare_keys = Compare.List (Ed25519.Public_key) in
       let opt (=) def v = if def = v then None else Some v in
       let default = Constants_repr.default in
       let preserved_cycles =
         opt Compare.Int.(=)
           default.preserved_cycles c.preserved_cycles
       and blocks_per_cycle =
         opt Compare.Int32.(=)
           default.blocks_per_cycle c.blocks_per_cycle
       and blocks_per_commitment =
         opt Compare.Int32.(=)
           default.blocks_per_commitment c.blocks_per_commitment
       and blocks_per_roll_snapshot =
         opt Compare.Int32.(=)
           default.blocks_per_roll_snapshot c.blocks_per_roll_snapshot
       and blocks_per_voting_period =
         opt Compare.Int32.(=)
           default.blocks_per_voting_period c.blocks_per_voting_period
       and time_between_blocks =
         opt Compare_time_between_blocks.(=)
           default.time_between_blocks c.time_between_blocks
       and first_free_baking_slot =
         opt Compare.Int.(=)
           default.first_free_baking_slot c.first_free_baking_slot
       and endorsers_per_block =
         opt Compare.Int.(=)
           default.endorsers_per_block c.endorsers_per_block
       and max_gas =
         opt Compare.Int.(=)
           default.max_gas c.max_gas
       and proof_of_work_threshold =
         opt Compare.Int64.(=)
           default.proof_of_work_threshold c.proof_of_work_threshold
       and dictator_pubkey =
         opt Signature.Public_key.(=)
           default.dictator_pubkey c.dictator_pubkey
       and max_operation_data_length =
         opt Compare.Int.(=)
           default.max_operation_data_length c.max_operation_data_length
       and tokens_per_roll =
         opt Tez_repr.(=)
           default.tokens_per_roll c.tokens_per_roll
       and michelson_maximum_type_size =
         opt Compare.Int.(=)
           default.michelson_maximum_type_size c.michelson_maximum_type_size
       and seed_nonce_revelation_tip =
         opt Tez_repr.(=)
           default.seed_nonce_revelation_tip c.seed_nonce_revelation_tip
       and origination_burn =
         opt Tez_repr.(=)
           default.origination_burn c.origination_burn
       and block_security_deposit =
         opt Tez_repr.(=)
           default.block_security_deposit c.block_security_deposit
       and endorsement_security_deposit =
         opt Tez_repr.(=)
           default.endorsement_security_deposit c.endorsement_security_deposit
       and block_reward =
         opt Tez_repr.(=)
           default.block_reward c.block_reward
       and endorsement_reward =
         opt Tez_repr.(=)
           default.endorsement_reward c.endorsement_reward
       in
       (( preserved_cycles,
          blocks_per_cycle,
          blocks_per_commitment,
          blocks_per_roll_snapshot,
          blocks_per_voting_period,
          time_between_blocks,
          first_free_baking_slot,
          endorsers_per_block,
          max_gas,
          proof_of_work_threshold),
        ( dictator_pubkey,
          max_operation_data_length,
          tokens_per_roll,
          michelson_maximum_type_size,
          seed_nonce_revelation_tip,
          origination_burn,
          block_security_deposit,
          endorsement_security_deposit,
          block_reward,
          endorsement_reward)))
    (fun (( preserved_cycles,
            blocks_per_cycle,
            blocks_per_commitment,
            blocks_per_roll_snapshot,
            blocks_per_voting_period,
            time_between_blocks,
            first_free_baking_slot,
            endorsers_per_block,
            max_gas,
            proof_of_work_threshold),
          ( dictator_pubkey,
            max_operation_data_length,
            tokens_per_roll,
            michelson_maximum_type_size,
            seed_nonce_revelation_tip,
            origination_burn,
            block_security_deposit,
            endorsement_security_deposit,
            block_reward,
            endorsement_reward)) ->
      let unopt def = function None -> def | Some v -> v in
      let default = Constants_repr.default in
      { Constants_repr.preserved_cycles =
          unopt default.preserved_cycles preserved_cycles ;
        blocks_per_cycle =
          unopt default.blocks_per_cycle blocks_per_cycle ;
        blocks_per_commitment =
          unopt default.blocks_per_commitment blocks_per_commitment ;
        blocks_per_roll_snapshot =
          unopt default.blocks_per_roll_snapshot blocks_per_roll_snapshot ;
        blocks_per_voting_period =
          unopt default.blocks_per_voting_period blocks_per_voting_period ;
        time_between_blocks =
          unopt default.time_between_blocks @@
          time_between_blocks ;
        first_free_baking_slot =
          unopt default.first_free_baking_slot first_free_baking_slot ;
        endorsers_per_block =
          unopt default.endorsers_per_block endorsers_per_block ;
        max_gas =
          unopt default.max_gas max_gas ;
        proof_of_work_threshold =
          unopt default.proof_of_work_threshold proof_of_work_threshold ;
        dictator_pubkey =
          unopt default.dictator_pubkey dictator_pubkey ;
        max_operation_data_length =
          unopt default.max_operation_data_length max_operation_data_length ;
        tokens_per_roll =
          unopt default.tokens_per_roll tokens_per_roll ;
        michelson_maximum_type_size =
          unopt default.michelson_maximum_type_size michelson_maximum_type_size ;
        seed_nonce_revelation_tip =
          unopt default.seed_nonce_revelation_tip seed_nonce_revelation_tip ;
        origination_burn =
          unopt default.origination_burn origination_burn ;
        block_security_deposit =
          unopt default.block_security_deposit block_security_deposit ;
        endorsement_security_deposit =
          unopt default.endorsement_security_deposit endorsement_security_deposit ;
        block_reward =
          unopt default.block_reward block_reward ;
        endorsement_reward =
          unopt default.endorsement_reward endorsement_reward ;
      } )
    (merge_objs
       (obj10
          (opt "preserved_cycles" uint8)
          (opt "blocks_per_cycle" int32)
          (opt "blocks_per_commitment" int32)
          (opt "blocks_per_roll_snapshot" int32)
          (opt "blocks_per_voting_period" int32)
          (opt "time_between_blocks" (list Period_repr.encoding))
          (opt "first_free_baking_slot" uint16)
          (opt "endorsers_per_block" uint16)
          (opt "instructions_per_transaction" int31)
          (opt "proof_of_work_threshold" int64))
       (obj10
          (opt "dictator_pubkey" Signature.Public_key.encoding)
          (opt "max_operation_data_length" int31)
          (opt "tokens_per_roll" Tez_repr.encoding)
          (opt "michelson_maximum_type_size" uint16)
          (opt "seed_nonce_revelation_tip" Tez_repr.encoding)
          (opt "origination_burn" Tez_repr.encoding)
          (opt "block_security_deposit" Tez_repr.encoding)
          (opt "endorsement_security_deposit" Tez_repr.encoding)
          (opt "block_reward" Tez_repr.encoding)
          (opt "endorsement_reward" Tez_repr.encoding)))

let encoding =
  let open Data_encoding in
  conv
    (fun { bootstrap_accounts ; commitments ; constants ;
           security_deposit_ramp_up_cycles } ->
      ((bootstrap_accounts, commitments, security_deposit_ramp_up_cycles),
       constants))
    (fun ( (bootstrap_accounts, commitments, security_deposit_ramp_up_cycles),
           constants) ->
      { bootstrap_accounts ; commitments ; constants ;
        security_deposit_ramp_up_cycles})
    (merge_objs
       (obj3
          (req "bootstrap_accounts" (list bootstrap_account_encoding))
          (dft "commitments"
             (list (merge_tups
                      (tup1 Unclaimed_public_key_hash.encoding)
                      Commitment_repr.encoding)) [])
          (opt "security_deposit_ramp_up_cycles" int31))
       constants_encoding)
