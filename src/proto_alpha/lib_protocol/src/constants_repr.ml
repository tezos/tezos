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

(* 1/8 tez *)
let seed_nonce_revelation_tip =
  match Tez_repr.(one /? 8L) with
  | Ok c -> c
  | Error _ -> assert false

(* 1 tez *)
let origination_burn =
  Tez_repr.one

(* 512 tez *)
let block_security_deposit =
  Tez_repr.(mul_exn one 512)

(* 64 tez *)
let endorsement_security_deposit =
  Tez_repr.(mul_exn one 64)

(* 16 tez *)
let block_reward =
  Tez_repr.(mul_exn one 16)

(* 2 tez *)
let endorsement_reward =
  Tez_repr.(mul_exn one 2)

type fixed = {
  proof_of_work_nonce_size : int ;
  nonce_length : int ;
  max_revelations_per_block : int ;
  seed_nonce_revelation_tip : Tez_repr.t ;
  origination_burn : Tez_repr.t ;
  block_security_deposit : Tez_repr.t ;
  endorsement_security_deposit : Tez_repr.t ;
  block_reward : Tez_repr.t ;
  endorsement_reward : Tez_repr.t ;
}

let fixed_encoding =
  let open Data_encoding in
  conv
    (fun c ->
       ( c.proof_of_work_nonce_size,
         c.nonce_length,
         c.max_revelations_per_block,
         c.seed_nonce_revelation_tip,
         c.origination_burn,
         c.block_security_deposit,
         c.endorsement_security_deposit,
         c.block_reward,
         c.endorsement_reward ))
    (fun ( proof_of_work_nonce_size,
           nonce_length,
           max_revelations_per_block,
           seed_nonce_revelation_tip,
           origination_burn,
           block_security_deposit,
           endorsement_security_deposit,
           block_reward,
           endorsement_reward) ->
      { proof_of_work_nonce_size ;
        nonce_length ;
        max_revelations_per_block ;
        seed_nonce_revelation_tip ;
        origination_burn ;
        block_security_deposit ;
        endorsement_security_deposit ;
        block_reward ;
        endorsement_reward ;
      } )
    (obj9
       (req "proof_of_work_nonce_size" uint8)
       (req "nonce_length" uint8)
       (req "max_revelations_per_block" uint8)
       (req "seed_nonce_revelation_tip" Tez_repr.encoding)
       (req "origination_burn" Tez_repr.encoding)
       (req "block_security_deposit" Tez_repr.encoding)
       (req "endorsement_security_deposit" Tez_repr.encoding)
       (req "block_reward" Tez_repr.encoding)
       (req "endorsement_reward" Tez_repr.encoding))

let fixed = {
  proof_of_work_nonce_size ;
  nonce_length ;
  max_revelations_per_block ;
  seed_nonce_revelation_tip ;
  origination_burn ;
  block_security_deposit ;
  endorsement_security_deposit ;
  block_reward ;
  endorsement_reward ;
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
}

let map_option f = function
  | None -> None
  | Some x -> Some (f x)

module CompareListInt = Compare.List (Compare.Int)

(* This encoding is used to read configuration files (e.g. sandbox.json)
   where some fields can be missing, in that case they are replaced by
   the default. *)
let sandbox_encoding =
  let open Data_encoding in
  conv
    (fun c ->
       let module Compare_time_between_blocks = Compare.List (Period_repr) in
       let module Compare_keys = Compare.List (Ed25519.Public_key) in
       let opt (=) def v = if def = v then None else Some v in
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
         opt Ed25519.Public_key.(=)
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
       in
       ((( preserved_cycles,
           blocks_per_cycle,
           blocks_per_commitment,
           blocks_per_roll_snapshot,
           blocks_per_voting_period,
           time_between_blocks,
           first_free_baking_slot,
           endorsers_per_block,
           max_gas),
         ( proof_of_work_threshold,
           dictator_pubkey,
           max_operation_data_length,
           tokens_per_roll,
           michelson_maximum_type_size)), ()) )
    (fun ((( preserved_cycles,
             blocks_per_cycle,
             blocks_per_commitment,
             blocks_per_roll_snapshot,
             blocks_per_voting_period,
             time_between_blocks,
             first_free_baking_slot,
             endorsers_per_block,
             max_gas),
           ( proof_of_work_threshold,
             dictator_pubkey,
             max_operation_data_length,
             tokens_per_roll,
             michelson_maximum_type_size)), ()) ->
      let unopt def = function None -> def | Some v -> v in
      { preserved_cycles =
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
      } )
    (merge_objs
       (merge_objs
          (obj9
             (opt "preserved_cycles" uint8)
             (opt "blocks_per_cycle" int32)
             (opt "blocks_per_commitment" int32)
             (opt "blocks_per_roll_snapshot" int32)
             (opt "blocks_per_voting_period" int32)
             (opt "time_between_blocks" (list Period_repr.encoding))
             (opt "first_free_baking_slot" uint16)
             (opt "endorsers_per_block" uint16)
             (opt "instructions_per_transaction" int31))
          (obj5
             (opt "proof_of_work_threshold" int64)
             (opt "dictator_pubkey" Ed25519.Public_key.encoding)
             (opt "max_operation_data_length" int31)
             (opt "tokens_per_roll" Tez_repr.encoding)
             (opt "michelson_maximum_type_size" uint16)
          ))
       unit)

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
          c.max_gas),
        ( c.proof_of_work_threshold,
          c.dictator_pubkey,
          c.max_operation_data_length,
          c.tokens_per_roll,
          c.michelson_maximum_type_size)) )
    (fun (( preserved_cycles,
            blocks_per_cycle,
            blocks_per_commitment,
            blocks_per_roll_snapshot,
            blocks_per_voting_period,
            time_between_blocks,
            first_free_baking_slot,
            endorsers_per_block,
            max_gas),
          ( proof_of_work_threshold,
            dictator_pubkey,
            max_operation_data_length,
            tokens_per_roll,
            michelson_maximum_type_size)) ->
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
      } )
    (merge_objs
       (obj9
          (req "preserved_cycles" uint8)
          (req "blocks_per_cycle" int32)
          (req "blocks_per_commitment" int32)
          (req "blocks_per_roll_snapshot" int32)
          (req "blocks_per_voting_period" int32)
          (req "time_between_blocks" (list Period_repr.encoding))
          (req "first_free_baking_slot" uint16)
          (req "endorsers_per_block" uint16)
          (req "instructions_per_transaction" int31))
       (obj5
          (req "proof_of_work_threshold" int64)
          (req "dictator_pubkey" Ed25519.Public_key.encoding)
          (req "max_operation_data_length" int31)
          (req "tokens_per_roll" Tez_repr.encoding)
          (req "michelson_maximum_type_size" uint16)))

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

let read_sandbox = function
  | None ->
      return default
  | Some json ->
      match Data_encoding.Json.(destruct sandbox_encoding json) with
      | exception exn -> fail (Constant_read exn)
      | c ->
          if Compare.Int32.(c.blocks_per_roll_snapshot > c.blocks_per_cycle) then
            failwith "Invalid sandbox: 'blocks_per_roll_snapshot > blocks_per_cycle'"
          else
            return c
