(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type bootstrap_account = {
  public_key_hash : Signature.Public_key_hash.t ;
  public_key : Signature.Public_key.t option ;
  amount : Tez_repr.t ;
}

type bootstrap_contract = {
  delegate : Signature.Public_key_hash.t ;
  amount : Tez_repr.t ;
  script : Script_repr.t ;
}

type t = {
  bootstrap_accounts : bootstrap_account list ;
  bootstrap_contracts : bootstrap_contract list ;
  commitments : Commitment_repr.t list ;
  constants : Constants_repr.parametric ;
  security_deposit_ramp_up_cycles : int option ;
  no_reward_cycles : int option ;
}

let bootstrap_account_encoding =
  let open Data_encoding in
  union
    [ case (Tag 0) ~title:"Public_key_known"
        (tup2
           Signature.Public_key.encoding
           Tez_repr.encoding)
        (function
          | { public_key_hash ; public_key = Some public_key ; amount } ->
              assert (Signature.Public_key_hash.equal
                        (Signature.Public_key.hash public_key)
                        public_key_hash) ;
              Some (public_key, amount)
          | { public_key = None }  -> None)
        (fun (public_key, amount) ->
           { public_key = Some public_key ;
             public_key_hash = Signature.Public_key.hash public_key ;
             amount }) ;
      case (Tag 1) ~title:"Public_key_unknown"
        (tup2
           Signature.Public_key_hash.encoding
           Tez_repr.encoding)
        (function
          | { public_key_hash ; public_key = None ; amount } ->
              Some (public_key_hash, amount)
          | { public_key = Some _ }  -> None)
        (fun (public_key_hash, amount) ->
           { public_key = None ;
             public_key_hash ;
             amount }) ]

let bootstrap_contract_encoding =
  let open Data_encoding in
  conv
    (fun { delegate ; amount ; script } -> (delegate, amount, script))
    (fun (delegate, amount, script) -> { delegate ; amount ; script })
    (obj3
       (req "delegate" Signature.Public_key_hash.encoding)
       (req "amount" Tez_repr.encoding)
       (req "script" Script_repr.encoding))

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
       and endorsers_per_block =
         opt Compare.Int.(=)
           default.endorsers_per_block c.endorsers_per_block
       and hard_gas_limit_per_operation =
         opt Compare.Z.(=)
           default.hard_gas_limit_per_operation c.hard_gas_limit_per_operation
       and hard_gas_limit_per_block =
         opt Compare.Z.(=)
           default.hard_gas_limit_per_block c.hard_gas_limit_per_block
       and proof_of_work_threshold =
         opt Compare.Int64.(=)
           default.proof_of_work_threshold c.proof_of_work_threshold
       and tokens_per_roll =
         opt Tez_repr.(=)
           default.tokens_per_roll c.tokens_per_roll
       and michelson_maximum_type_size =
         opt Compare.Int.(=)
           default.michelson_maximum_type_size c.michelson_maximum_type_size
       and seed_nonce_revelation_tip =
         opt Tez_repr.(=)
           default.seed_nonce_revelation_tip c.seed_nonce_revelation_tip
       and origination_size =
         opt Compare.Int.(=)
           default.origination_size c.origination_size
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
       and cost_per_byte =
         opt Tez_repr.(=)
           default.cost_per_byte c.cost_per_byte
       and hard_storage_limit_per_operation =
         opt Compare.Z.(=)
           default.hard_storage_limit_per_operation c.hard_storage_limit_per_operation
       in
       (( preserved_cycles,
          blocks_per_cycle,
          blocks_per_commitment,
          blocks_per_roll_snapshot,
          blocks_per_voting_period,
          time_between_blocks,
          endorsers_per_block,
          hard_gas_limit_per_operation,
          hard_gas_limit_per_block),
        ((proof_of_work_threshold,
          tokens_per_roll,
          michelson_maximum_type_size,
          seed_nonce_revelation_tip,
          origination_size,
          block_security_deposit,
          endorsement_security_deposit,
          block_reward),
         (endorsement_reward,
          cost_per_byte,
          hard_storage_limit_per_operation))))
    (fun (( preserved_cycles,
            blocks_per_cycle,
            blocks_per_commitment,
            blocks_per_roll_snapshot,
            blocks_per_voting_period,
            time_between_blocks,
            endorsers_per_block,
            hard_gas_limit_per_operation,
            hard_gas_limit_per_block),
          ((proof_of_work_threshold,
            tokens_per_roll,
            michelson_maximum_type_size,
            seed_nonce_revelation_tip,
            origination_size,
            block_security_deposit,
            endorsement_security_deposit,
            block_reward),
           (endorsement_reward,
            cost_per_byte,
            hard_storage_limit_per_operation))) ->
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
        endorsers_per_block =
          unopt default.endorsers_per_block endorsers_per_block ;
        hard_gas_limit_per_operation =
          unopt default.hard_gas_limit_per_operation hard_gas_limit_per_operation ;
        hard_gas_limit_per_block =
          unopt default.hard_gas_limit_per_block hard_gas_limit_per_block ;
        proof_of_work_threshold =
          unopt default.proof_of_work_threshold proof_of_work_threshold ;
        tokens_per_roll =
          unopt default.tokens_per_roll tokens_per_roll ;
        michelson_maximum_type_size =
          unopt default.michelson_maximum_type_size michelson_maximum_type_size ;
        seed_nonce_revelation_tip =
          unopt default.seed_nonce_revelation_tip seed_nonce_revelation_tip ;
        origination_size =
          unopt default.origination_size origination_size ;
        block_security_deposit =
          unopt default.block_security_deposit block_security_deposit ;
        endorsement_security_deposit =
          unopt default.endorsement_security_deposit endorsement_security_deposit ;
        block_reward =
          unopt default.block_reward block_reward ;
        endorsement_reward =
          unopt default.endorsement_reward endorsement_reward ;
        cost_per_byte =
          unopt default.cost_per_byte cost_per_byte ;
        hard_storage_limit_per_operation =
          unopt default.hard_storage_limit_per_operation hard_storage_limit_per_operation ;
      } )
    (merge_objs
       (obj9
          (opt "preserved_cycles" uint8)
          (opt "blocks_per_cycle" int32)
          (opt "blocks_per_commitment" int32)
          (opt "blocks_per_roll_snapshot" int32)
          (opt "blocks_per_voting_period" int32)
          (opt "time_between_blocks" (list Period_repr.encoding))
          (opt "endorsers_per_block" uint16)
          (opt "hard_gas_limit_per_operation" z)
          (opt "hard_gas_limit_per_block" z))
       (merge_objs
          (obj8
             (opt "proof_of_work_threshold" int64)
             (opt "tokens_per_roll" Tez_repr.encoding)
             (opt "michelson_maximum_type_size" uint16)
             (opt "seed_nonce_revelation_tip" Tez_repr.encoding)
             (opt "origination_size" int31)
             (opt "block_security_deposit" Tez_repr.encoding)
             (opt "endorsement_security_deposit" Tez_repr.encoding)
             (opt "block_reward" Tez_repr.encoding))
          (obj3
             (opt "endorsement_reward" Tez_repr.encoding)
             (opt "cost_per_byte" Tez_repr.encoding)
             (opt "hard_storage_limit_per_operation" z))))

let encoding =
  let open Data_encoding in
  conv
    (fun { bootstrap_accounts ; bootstrap_contracts ; commitments ; constants ;
           security_deposit_ramp_up_cycles ; no_reward_cycles } ->
      ((bootstrap_accounts, bootstrap_contracts, commitments,
        security_deposit_ramp_up_cycles, no_reward_cycles),
       constants))
    (fun ( (bootstrap_accounts, bootstrap_contracts, commitments,
            security_deposit_ramp_up_cycles, no_reward_cycles),
           constants) ->
      { bootstrap_accounts ; bootstrap_contracts ; commitments ; constants ;
        security_deposit_ramp_up_cycles ; no_reward_cycles })
    (merge_objs
       (obj5
          (req "bootstrap_accounts" (list bootstrap_account_encoding))
          (dft "bootstrap_contracts" (list bootstrap_contract_encoding) [])
          (dft "commitments" (list Commitment_repr.encoding) [])
          (opt "security_deposit_ramp_up_cycles" int31)
          (opt "no_reward_cycles" int31))
       constants_encoding)
