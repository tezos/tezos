(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let version_number = "\000"
let proof_of_work_nonce_size = 8
let nonce_length = 32
let max_revelations_per_block = 32

type fixed = {
  proof_of_work_nonce_size : int ;
  nonce_length : int ;
  max_revelations_per_block : int ;
}

let fixed_encoding =
  let open Data_encoding in
  conv
    (fun c ->
       ( c.proof_of_work_nonce_size,
         c.nonce_length,
         c.max_revelations_per_block ))
    (fun ( proof_of_work_nonce_size,
           nonce_length,
           max_revelations_per_block ) ->
      { proof_of_work_nonce_size ;
        nonce_length ;
        max_revelations_per_block ;
      } )
    (obj3
       (req "proof_of_work_nonce_size" uint8)
       (req "nonce_length" uint8)
       (req "max_revelations_per_block" uint8))

let fixed = {
  proof_of_work_nonce_size ;
  nonce_length ;
  max_revelations_per_block ;
}

type parametric = {
  preserved_cycles: int ;
  blocks_per_cycle: int32 ;
  blocks_per_commitment: int32 ;
  blocks_per_roll_snapshot: int32 ;
  blocks_per_voting_period: int32 ;
  time_between_blocks: Period_repr.t list ;
  first_free_baking_slot: int ;
  endorsers_per_block: int ;
  max_gas: int ;
  proof_of_work_threshold: int64 ;
  dictator_pubkey: Ed25519.Public_key.t ;
  max_operation_data_length: int ;
  tokens_per_roll: Tez_repr.t ;
  michelson_maximum_type_size: int;
  seed_nonce_revelation_tip: Tez_repr.t ;
  origination_burn: Tez_repr.t ;
  block_security_deposit: Tez_repr.t ;
  endorsement_security_deposit: Tez_repr.t ;
  block_reward: Tez_repr.t ;
  endorsement_reward: Tez_repr.t ;
}

let default = {
  preserved_cycles = 5 ;
  blocks_per_cycle = 4096l ;
  blocks_per_commitment = 32l ;
  blocks_per_roll_snapshot = 256l ;
  blocks_per_voting_period = 32768l ;
  time_between_blocks =
    List.map Period_repr.of_seconds_exn [ 60L ] ;
  first_free_baking_slot = 16 ;
  endorsers_per_block = 32 ;
  max_gas = 40_000 ;
  proof_of_work_threshold =
    Int64.(sub (shift_left 1L 56) 1L) ;
  dictator_pubkey =
    Ed25519.Public_key.of_b58check_exn
      "edpkugeDwmwuwyyD3Q5enapgEYDxZLtEUFFSrvVwXASQMVEqsvTqWu" ;
  max_operation_data_length =
    16 * 1024 ; (* 16kB *)
  tokens_per_roll =
    Tez_repr.(mul_exn one 10_000) ;
  michelson_maximum_type_size = 1000 ;
  seed_nonce_revelation_tip = begin
    match Tez_repr.(one /? 8L) with
    | Ok c -> c
    | Error _ -> assert false
  end ;
  origination_burn = Tez_repr.one ;
  block_security_deposit = Tez_repr.(mul_exn one 512) ;
  endorsement_security_deposit = Tez_repr.(mul_exn one 64) ;
  block_reward = Tez_repr.(mul_exn one 16) ;
  endorsement_reward = Tez_repr.(mul_exn one 2) ;
}

module CompareListInt = Compare.List (Compare.Int)

let parametric_encoding =
  let open Data_encoding in
  conv
    (fun c ->
       (( c.preserved_cycles,
          c.blocks_per_cycle,
          c.blocks_per_commitment,
          c.blocks_per_roll_snapshot,
          c.blocks_per_voting_period,
          c.time_between_blocks,
          c.first_free_baking_slot,
          c.endorsers_per_block,
          c.max_gas,
          c.proof_of_work_threshold ),
        ( c.dictator_pubkey,
          c.max_operation_data_length,
          c.tokens_per_roll,
          c.michelson_maximum_type_size,
          c.seed_nonce_revelation_tip,
          c.origination_burn,
          c.block_security_deposit,
          c.endorsement_security_deposit,
          c.block_reward,
          c.endorsement_reward)) )
    (fun (( preserved_cycles,
            blocks_per_cycle,
            blocks_per_commitment,
            blocks_per_roll_snapshot,
            blocks_per_voting_period,
            time_between_blocks,
            first_free_baking_slot,
            endorsers_per_block,
            max_gas,
            proof_of_work_threshold ),
          ( dictator_pubkey,
            max_operation_data_length,
            tokens_per_roll,
            michelson_maximum_type_size,
            seed_nonce_revelation_tip,
            origination_burn,
            block_security_deposit,
            endorsement_security_deposit,
            block_reward,
            endorsement_reward )) ->
      { preserved_cycles ;
        blocks_per_cycle ;
        blocks_per_commitment ;
        blocks_per_roll_snapshot ;
        blocks_per_voting_period ;
        time_between_blocks ;
        first_free_baking_slot ;
        endorsers_per_block ;
        max_gas ;
        proof_of_work_threshold ;
        dictator_pubkey ;
        max_operation_data_length ;
        tokens_per_roll ;
        michelson_maximum_type_size ;
        seed_nonce_revelation_tip ;
        origination_burn ;
        block_security_deposit ;
        endorsement_security_deposit ;
        block_reward ;
        endorsement_reward ;
      } )
    (merge_objs
       (obj10
          (req "preserved_cycles" uint8)
          (req "blocks_per_cycle" int32)
          (req "blocks_per_commitment" int32)
          (req "blocks_per_roll_snapshot" int32)
          (req "blocks_per_voting_period" int32)
          (req "time_between_blocks" (list Period_repr.encoding))
          (req "first_free_baking_slot" uint16)
          (req "endorsers_per_block" uint16)
          (req "instructions_per_transaction" int31)
          (req "proof_of_work_threshold" int64))
       (obj10
          (req "dictator_pubkey" Ed25519.Public_key.encoding)
          (req "max_operation_data_length" int31)
          (req "tokens_per_roll" Tez_repr.encoding)
          (req "michelson_maximum_type_size" uint16)
          (req "seed_nonce_revelation_tip" Tez_repr.encoding)
          (req "origination_burn" Tez_repr.encoding)
          (req "block_security_deposit" Tez_repr.encoding)
          (req "endorsement_security_deposit" Tez_repr.encoding)
          (req "block_reward" Tez_repr.encoding)
          (req "endorsement_reward" Tez_repr.encoding)))

type t = {
  fixed : fixed ;
  parametric : parametric ;
}

let encoding =
  let open Data_encoding in
  conv
    (fun { fixed ; parametric } -> (fixed, parametric))
    (fun (fixed , parametric) -> { fixed ; parametric })
    (merge_objs fixed_encoding parametric_encoding)

type error += Constant_read of exn
