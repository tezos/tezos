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

type constants = {
  cycle_length: int32 ;
  voting_period_length: int32 ;
  time_before_reward: Period_repr.t ;
  time_between_slots: Period_repr.t ;
  first_free_mining_slot: int32 ;
  max_signing_slot: int ;
  instructions_per_transaction: int ;
  proof_of_work_threshold: int64 ;
}

let default = {
  cycle_length = 2048l ;
  voting_period_length = 32768l ;
  time_before_reward =
    Period_repr.of_seconds_exn
    (* One year in seconds *)
    Int64.(mul 365L (mul 24L 3600L)) ;
  time_between_slots =
    Period_repr.of_seconds_exn
      (* One minute in seconds *)
      10L ;
  first_free_mining_slot = 16l ;
  max_signing_slot = 15 ;
  instructions_per_transaction = 16 * 1024 ;
  proof_of_work_threshold =
    Int64.(lognot (sub (shift_left 1L 56) 1L)) ;
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
       let open Compare in
       let cycle_length =
         opt Int32.(=)
           default.cycle_length c.cycle_length
       and voting_period_length =
         opt Int32.(=)
           default.voting_period_length c.voting_period_length
       and time_before_reward =
         map_option Period_repr.to_seconds @@
         opt Period_repr.(=)
           default.time_before_reward c.time_before_reward
       and time_between_slots =
         map_option Period_repr.to_seconds @@
         opt Period_repr.(=)
           default.time_between_slots c.time_between_slots
       and first_free_mining_slot =
         opt Int32.(=)
           default.first_free_mining_slot c.first_free_mining_slot
       and max_signing_slot =
         opt Int.(=)
           default.max_signing_slot c.max_signing_slot
       and instructions_per_transaction =
         opt Int.(=)
           default.instructions_per_transaction c.instructions_per_transaction
       and proof_of_work_threshold =
         opt Int64.(=)
           default.proof_of_work_threshold c.proof_of_work_threshold
       in
       ( cycle_length,
         voting_period_length,
         time_before_reward,
         time_between_slots,
         first_free_mining_slot,
         max_signing_slot,
         instructions_per_transaction,
         proof_of_work_threshold
       ) )
    (fun ( cycle_length,
           voting_period_length,
           time_before_reward,
           time_between_slots,
           first_free_mining_slot,
           max_signing_slot,
           instructions_per_transaction,
           proof_of_work_threshold
         ) ->
      { cycle_length =
          unopt default.cycle_length cycle_length ;
        voting_period_length =
          unopt default.voting_period_length voting_period_length ;
        time_before_reward =
          unopt default.time_before_reward @@
          map_option Period_repr.of_seconds_exn time_before_reward ;
        time_between_slots =
          unopt default.time_between_slots @@
          map_option Period_repr.of_seconds_exn time_between_slots ;
        first_free_mining_slot =
          unopt default.first_free_mining_slot first_free_mining_slot ;
        max_signing_slot =
          unopt default.max_signing_slot max_signing_slot ;
        instructions_per_transaction =
          unopt default.instructions_per_transaction instructions_per_transaction ;
        proof_of_work_threshold =
          unopt default.proof_of_work_threshold proof_of_work_threshold ;
      } )
    Data_encoding.(
      obj8
        (opt "cycle_length" int32)
        (opt "voting_period_length" int32)
        (opt "time_before_reward" int64)
        (opt "time_between_slots" int64)
        (opt "first_free_mining_slot" int32)
        (opt "max_signing_slot" int31)
        (opt "instructions_per_transaction" int31)
        (opt "proof_of_work_threshold" int64)
    )

type error += Constant_read of exn

let read = function
  | None ->
      return default
  | Some json ->
      match Data_encoding.Json.(destruct constants_encoding json) with
      | exception exn -> fail (Constant_read exn)
      | c -> return c
