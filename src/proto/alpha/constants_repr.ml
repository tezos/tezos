(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let version_number = "\000"

let max_operation_data_length = 16 * 1024
let max_number_of_operations = 200
let proof_of_work_nonce_size = 8
let nonce_length = 32

let roll_value =
  Tez_repr.of_cents_exn 10000_00L
let seed_nonce_revelation_tip =
  Tez_repr.of_cents_exn 10_00L
let origination_burn =
  Tez_repr.of_cents_exn 1_00L
let minimal_contract_balance =
  Tez_repr.of_cents_exn 1_00L
let mining_bond_cost =
  Tez_repr.of_cents_exn 1000_00L
let endorsement_bond_cost =
  Tez_repr.of_cents_exn 1000_00L
let mining_reward =
  Tez_repr.of_cents_exn 150_00L
let endorsement_reward =
  Tez_repr.of_cents_exn 150_00L
let faucet_credit =
  Tez_repr.of_cents_exn 100_000_00L

type constants = {
  cycle_length: int32 ;
  voting_period_length: int32 ;
  time_before_reward: Period_repr.t ;
  slot_durations: Period_repr.t list ;
  first_free_mining_slot: int ;
  max_signing_slot: int ;
  instructions_per_transaction: int ;
  proof_of_work_threshold: int64 ;
  bootstrap_keys: Ed25519.Public_key.t list ;
  dictator_pubkey: Ed25519.Public_key.t ;
}

let read_public_key s =
  Ed25519.Public_key.of_bytes (Bytes.of_string (Hex_encode.hex_decode s))

let default = {
  cycle_length = 2048l ;
  voting_period_length = 32768l ;
  time_before_reward =
    Period_repr.of_seconds_exn
      (* One year in seconds *)
      Int64.(mul 365L (mul 24L 3600L)) ;
  slot_durations =
    List.map Period_repr.of_seconds_exn [ 60L ] ;
  first_free_mining_slot = 16 ;
  max_signing_slot = 15 ;
  instructions_per_transaction = 16 * 1024 ;
  proof_of_work_threshold =
    Int64.(lognot (sub (shift_left 1L 56) 1L)) ;
  bootstrap_keys =
    List.map read_public_key [
      "dd5d3536916765fd00a8cd402bddd34e87b49ae5159c43b8feecfd9f06b267d2" ;
      "ce09f1c6b91d48cdd9f2aa98daf780f07353c759866c7dfbe50eb023bde51629" ;
      "9c328bddf6249bbe550121076194d99bbe60e5b1e144da4f426561b5d3bbc6ab" ;
      "a3db517734e07ace089ad0a2388e7276fb9b114bd79259dd5c93b0c33d57d6a2" ;
      "6d2d52e62f1d48f3cf9badbc90cfe5f3aa600194bf21eda44b8e64698a82d341" ;
    ] ;
  dictator_pubkey =
    read_public_key
      "4d5373455738070434f214826d301a1c206780d7f789fcbf94c2149b2e0718cc";
}

let opt (=) def v = if def = v then None else Some v
let unopt def = function None -> def | Some v -> v

let map_option f = function
  | None -> None
  | Some x -> Some (f x)

let constants_encoding =
  (* let open Data_encoding in *)
  Data_encoding.conv
    (fun c ->
       let module Compare_slot_durations = Compare.List (Period_repr) in
       let module Compare_keys = Compare.List (Ed25519.Public_key) in
       let cycle_length =
         opt Compare.Int32.(=)
           default.cycle_length c.cycle_length
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
       and first_free_mining_slot =
         opt Compare.Int.(=)
           default.first_free_mining_slot c.first_free_mining_slot
       and max_signing_slot =
         opt Compare.Int.(=)
           default.max_signing_slot c.max_signing_slot
       and instructions_per_transaction =
         opt Compare.Int.(=)
           default.instructions_per_transaction c.instructions_per_transaction
       and proof_of_work_threshold =
         opt Compare.Int64.(=)
           default.proof_of_work_threshold c.proof_of_work_threshold
       and bootstrap_keys =
         opt Compare_keys.(=)
           default.bootstrap_keys c.bootstrap_keys
       and dictator_pubkey =
         opt Ed25519.Public_key.(=)
           default.dictator_pubkey c.dictator_pubkey
       in
       (( cycle_length,
          voting_period_length,
          time_before_reward,
          slot_durations,
          first_free_mining_slot,
          max_signing_slot,
          instructions_per_transaction,
          proof_of_work_threshold,
          bootstrap_keys,
          dictator_pubkey), ()) )
    (fun (( cycle_length,
            voting_period_length,
            time_before_reward,
            slot_durations,
            first_free_mining_slot,
            max_signing_slot,
            instructions_per_transaction,
            proof_of_work_threshold,
            bootstrap_keys,
            dictator_pubkey), ()) ->
      { cycle_length =
          unopt default.cycle_length cycle_length ;
        voting_period_length =
          unopt default.voting_period_length voting_period_length ;
        time_before_reward =
          unopt default.time_before_reward @@
          map_option Period_repr.of_seconds_exn time_before_reward ;
        slot_durations =
          unopt default.slot_durations @@
          slot_durations ;
        first_free_mining_slot =
          unopt default.first_free_mining_slot first_free_mining_slot ;
        max_signing_slot =
          unopt default.max_signing_slot max_signing_slot ;
        instructions_per_transaction =
          unopt default.instructions_per_transaction instructions_per_transaction ;
        proof_of_work_threshold =
          unopt default.proof_of_work_threshold proof_of_work_threshold ;
        bootstrap_keys =
          unopt default.bootstrap_keys bootstrap_keys ;
        dictator_pubkey =
          unopt default.dictator_pubkey dictator_pubkey ;
      } )
     Data_encoding.(
       merge_objs
         (obj10
            (opt "cycle_length" int32)
            (opt "voting_period_length" int32)
            (opt "time_before_reward" int64)
            (opt "slot_durations" (list Period_repr.encoding))
            (opt "first_free_mining_slot" uint16)
            (opt "max_signing_slot" uint16)
            (opt "instructions_per_transaction" int31)
            (opt "proof_of_work_threshold" int64)
            (opt "bootstrap_keys" (list Ed25519.Public_key.encoding))
            (opt "dictator_pubkey" Ed25519.Public_key.encoding))
         unit)

type error += Constant_read of exn

let read = function
  | None ->
      return default
  | Some json ->
      match Data_encoding.Json.(destruct constants_encoding json) with
      | exception exn -> fail (Constant_read exn)
      | c -> return c
