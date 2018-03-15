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

(* 1000 tez *)
let baking_bond_cost =
  Tez_repr.(mul_exn one 1000)

(* 1000 tez *)
let endorsement_bond_cost =
  Tez_repr.(mul_exn one 1000)

(* 150 tez *)
let baking_reward =
  Tez_repr.(mul_exn one 150)

(* 150 tez *)
let endorsement_reward =
  Tez_repr.(mul_exn one 150)

(* 100,000 tez *)
let faucet_credit =
  Tez_repr.(mul_exn one 100_000)

(* 4,000,000 tez *)
let bootstrap_wealth =
  Tez_repr.(mul_exn one 4_000_000)

type constants = {
  preserved_cycles: int ;
  cycle_length: int32 ;
  blocks_per_commitment: int32 ;
  voting_period_length: int32 ;
  time_before_reward: Period_repr.t ;
  slot_durations: Period_repr.t list ;
  first_free_baking_slot: int ;
  max_signing_slot: int ;
  max_gas: int ;
  proof_of_work_threshold: int64 ;
  bootstrap_keys: Ed25519.Public_key.t list ;
  dictator_pubkey: Ed25519.Public_key.t ;
  max_number_of_operations: int list ;
  max_operation_data_length: int ;
  initial_roll_value: Tez_repr.t ;
  michelson_maximum_type_size: int;
}

let read_public_key s = Ed25519.Public_key.of_hex_exn (`Hex s)

let default = {
  preserved_cycles = 5 ;
  cycle_length = 2048l ;
  blocks_per_commitment = 32l ;
  voting_period_length = 32768l ;
  time_before_reward =
    Period_repr.of_seconds_exn
      (* One year in seconds *)
      Int64.(mul 365L (mul 24L 3600L)) ;
  slot_durations =
    List.map Period_repr.of_seconds_exn [ 60L ] ;
  first_free_baking_slot = 16 ;
  max_signing_slot = 15 ;
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
  initial_roll_value =
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
       let module Compare_slot_durations = Compare.List (Period_repr) in
       let module Compare_keys = Compare.List (Ed25519.Public_key) in
       let preserved_cycles =
         opt Compare.Int.(=)
           default.preserved_cycles c.preserved_cycles
       and cycle_length =
         opt Compare.Int32.(=)
           default.cycle_length c.cycle_length
       and blocks_per_commitment =
         opt Compare.Int32.(=)
           default.blocks_per_commitment c.blocks_per_commitment
       and voting_period_length =
         opt Compare.Int32.(=)
           default.voting_period_length c.voting_period_length
       and time_before_reward =
         map_option Period_repr.to_seconds @@
         opt Period_repr.(=)
           default.time_before_reward c.time_before_reward
       and slot_durations =
         opt Compare_slot_durations.(=)
           default.slot_durations c.slot_durations
       and first_free_baking_slot =
         opt Compare.Int.(=)
           default.first_free_baking_slot c.first_free_baking_slot
       and max_signing_slot =
         opt Compare.Int.(=)
           default.max_signing_slot c.max_signing_slot
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
       and initial_roll_value =
         opt Tez_repr.(=)
           default.initial_roll_value c.initial_roll_value
       and michelson_maximum_type_size =
         opt Compare.Int.(=)
           default.michelson_maximum_type_size c.michelson_maximum_type_size
       in
       ((( preserved_cycles,
           cycle_length,
           blocks_per_commitment,
           voting_period_length,
           time_before_reward,
           slot_durations,
           first_free_baking_slot,
           max_signing_slot,
           max_gas,
           proof_of_work_threshold),
         ( bootstrap_keys,
           dictator_pubkey,
           max_number_of_operations,
           max_operation_data_length,
           initial_roll_value,
           michelson_maximum_type_size)), ()) )
    (fun ((( preserved_cycles,
             cycle_length,
             blocks_per_commitment,
             voting_period_length,
             time_before_reward,
             slot_durations,
             first_free_baking_slot,
             max_signing_slot,
             max_gas,
             proof_of_work_threshold),
           ( bootstrap_keys,
             dictator_pubkey,
             max_number_of_operations,
             max_operation_data_length,
             initial_roll_value,
             michelson_maximum_type_size)), ()) ->
      { preserved_cycles =
          unopt default.preserved_cycles preserved_cycles ;
        cycle_length =
          unopt default.cycle_length cycle_length ;
        blocks_per_commitment =
          unopt default.blocks_per_commitment blocks_per_commitment ;
        voting_period_length =
          unopt default.voting_period_length voting_period_length ;
        time_before_reward =
          unopt default.time_before_reward @@
          map_option Period_repr.of_seconds_exn time_before_reward ;
        slot_durations =
          unopt default.slot_durations @@
          slot_durations ;
        first_free_baking_slot =
          unopt default.first_free_baking_slot first_free_baking_slot ;
        max_signing_slot =
          unopt default.max_signing_slot max_signing_slot ;
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
        initial_roll_value =
          unopt default.initial_roll_value initial_roll_value ;
        michelson_maximum_type_size =
          unopt default.michelson_maximum_type_size michelson_maximum_type_size ;
      } )
    Data_encoding.(
      merge_objs
        (merge_objs
           (obj10
              (opt "preserved_cycles" uint8)
              (opt "cycle_length" int32)
              (opt "blocks_per_commitment" int32)
              (opt "voting_period_length" int32)
              (opt "time_before_reward" int64)
              (opt "slot_durations" (list Period_repr.encoding))
              (opt "first_free_baking_slot" uint16)
              (opt "max_signing_slot" uint16)
              (opt "instructions_per_transaction" int31)
              (opt "proof_of_work_threshold" int64))
           (obj6
              (opt "bootstrap_keys" (list Ed25519.Public_key.encoding))
              (opt "dictator_pubkey" Ed25519.Public_key.encoding)
              (opt "max_number_of_operations" (list uint16))
              (opt "max_number_of_operations" int31)
              (opt "initial_roll_value" Tez_repr.encoding)
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
      | c -> return c
