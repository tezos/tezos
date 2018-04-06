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

(* 4,000,000 tez *)
let bootstrap_wealth =
  Tez_repr.(mul_exn one 4_000_000)

let max_revelations_per_block = 32

type constants = {
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
  bootstrap_keys: Ed25519.Public_key.t list ;
  dictator_pubkey: Ed25519.Public_key.t ;
  max_operation_data_length: int ;
  tokens_per_roll: Tez_repr.t ;
  michelson_maximum_type_size: int;
}

let read_public_key = Ed25519.Public_key.of_b58check_exn

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
  bootstrap_keys =
    List.map read_public_key [
      "edpkumCM1MAkah9ESaoQJnf1pXKrEYZMtFnEz46rrpq9SWkF1phM5Q" ;
      "edpktsJoNN7G67B4rBwXD44ymRwEMMXVJDV2nURasB3gd6jqibZqWh" ;
      "edpkth7ZUHB1X26ruiQXg4WaJPKPFfBqX5wvgja17Bf6hybMcRBCkn" ;
      "edpkvTnasCe4gtEPRsgyBhMjvDkyf5YA7QTyEksJj836gXVCSxeQZk" ;
      "edpkvXCagqGcEfJ7rUmzwRcPMViM6Yk2XGwwkrLre4d9yMFEqsrwuf" ;
    ] ;
  dictator_pubkey =
    read_public_key
      "edpkugeDwmwuwyyD3Q5enapgEYDxZLtEUFFSrvVwXASQMVEqsvTqWu" ;
  max_operation_data_length =
    16 * 1024 ; (* 16kB *)
  tokens_per_roll =
    Tez_repr.(mul_exn one 10_000) ;
  michelson_maximum_type_size = 1000 ;
}

let opt (=) def v = if def = v then None else Some v
let unopt def = function None -> def | Some v -> v

let map_option f = function
  | None -> None
  | Some x -> Some (f x)

module CompareListInt = Compare.List (Compare.Int)

let constants_encoding =
  (* let open Data_encoding in *)
  Data_encoding.conv
    (fun c ->
       let module Compare_time_between_blocks = Compare.List (Period_repr) in
       let module Compare_keys = Compare.List (Ed25519.Public_key) in
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
       and bootstrap_keys =
         opt Compare_keys.(=)
           default.bootstrap_keys c.bootstrap_keys
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
           bootstrap_keys,
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
             bootstrap_keys,
             dictator_pubkey,
             max_operation_data_length,
             tokens_per_roll,
             michelson_maximum_type_size)), ()) ->
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
        bootstrap_keys =
          unopt default.bootstrap_keys bootstrap_keys ;
        dictator_pubkey =
          unopt default.dictator_pubkey dictator_pubkey ;
        max_operation_data_length =
          unopt default.max_operation_data_length max_operation_data_length ;
        tokens_per_roll =
          unopt default.tokens_per_roll tokens_per_roll ;
        michelson_maximum_type_size =
          unopt default.michelson_maximum_type_size michelson_maximum_type_size ;
      } )
    Data_encoding.(
      merge_objs
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
           (obj6
              (opt "proof_of_work_threshold" int64)
              (opt "bootstrap_keys" (list Ed25519.Public_key.encoding))
              (opt "dictator_pubkey" Ed25519.Public_key.encoding)
              (opt "max_operation_data_length" int31)
              (opt "tokens_per_roll" Tez_repr.encoding)
              (opt "michelson_maximum_type_size" uint16)
           ))
        unit)

type error += Constant_read of string

let read = function
  | None ->
      return default
  | Some json ->
      match Data_encoding.Json.(destruct constants_encoding json) with
      | exception (Data_encoding.Json.Cannot_destruct _ as exn) ->
          Format.kasprintf
            failwith "Invalid sandbox: %a %a"
            (fun ppf -> Data_encoding.Json.print_error ppf) exn
            Data_encoding.Json.pp json
      | c ->
          if Compare.Int32.(c.blocks_per_roll_snapshot > c.blocks_per_cycle) then
            failwith "Invalid sandbox: 'blocks_per_roll_snapshot > blocks_per_cycle'"
          else
            return c
