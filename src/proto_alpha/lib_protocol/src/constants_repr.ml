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

(* 10 tez *)
let seed_nonce_revelation_tip =
  Tez_repr.(mul_exn one 10)

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

(* 100,000 tez *)
let faucet_credit =
  Tez_repr.(mul_exn one 100_000)

(* 4,000,000 tez *)
let bootstrap_wealth =
  Tez_repr.(mul_exn one 4_000_000)

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
  max_number_of_operations: int list ;
  max_operation_data_length: int ;
  token_per_rolls: Tez_repr.t ;
  michelson_maximum_type_size: int;
}

let read_public_key s = Ed25519.Public_key.of_hex_exn (`Hex s)

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
      "dd5d3536916765fd00a8cd402bddd34e87b49ae5159c43b8feecfd9f06b267d2" ;
      "2dc874e66659ef2df0b7c6f29af7c913d32a01acecb36c4ad1a4ed74af7de33a" ;
      "9c328bddf6249bbe550121076194d99bbe60e5b1e144da4f426561b5d3bbc6ab" ;
      "a3db517734e07ace089ad0a2388e7276fb9b114bd79259dd5c93b0c33d57d6a2" ;
      "30cdca1f0713916c9f1f2d3efc9fb688deb3e2f87b19ccd77f4c06676dc9baa9" ;
    ] ;
  dictator_pubkey =
    read_public_key
      "4d5373455738070434f214826d301a1c206780d7f789fcbf94c2149b2e0718cc" ;
  max_number_of_operations =
    [ 300 ] ;
  max_operation_data_length =
    16 * 1024 ; (* 16kB *)
  token_per_rolls =
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
       and max_number_of_operations =
         opt CompareListInt.(=)
           default.max_number_of_operations c.max_number_of_operations
       and max_operation_data_length =
         opt Compare.Int.(=)
           default.max_operation_data_length c.max_operation_data_length
       and token_per_rolls =
         opt Tez_repr.(=)
           default.token_per_rolls c.token_per_rolls
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
           max_number_of_operations,
           max_operation_data_length,
           token_per_rolls,
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
             max_number_of_operations,
             max_operation_data_length,
             token_per_rolls,
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
        max_number_of_operations =
          unopt default.max_number_of_operations max_number_of_operations ;
        max_operation_data_length =
          unopt default.max_operation_data_length max_operation_data_length ;
        token_per_rolls =
          unopt default.token_per_rolls token_per_rolls ;
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
           (obj7
              (opt "proof_of_work_threshold" int64)
              (opt "bootstrap_keys" (list Ed25519.Public_key.encoding))
              (opt "dictator_pubkey" Ed25519.Public_key.encoding)
              (opt "max_number_of_operations" (list uint16))
              (opt "max_number_of_operations" int31)
              (opt "token_per_rolls" Tez_repr.encoding)
              (opt "michelson_maximum_type_size" uint16)
           ))
        unit)

type error += Constant_read of exn

let read = function
  | None ->
      return default
  | Some json ->
      match Data_encoding.Json.(destruct constants_encoding json) with
      | exception exn -> fail (Constant_read exn)
      | c ->
          if Compare.Int32.(c.blocks_per_roll_snapshot > c.blocks_per_cycle) then
            failwith "Invalid sandbox: 'blocks_per_roll_snapshot > blocks_per_cycle'"
          else
            return c
