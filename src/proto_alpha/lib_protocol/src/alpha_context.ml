(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = Raw_context.t
type context = t

module type BASIC_DATA = sig
  type t
  include Compare.S with type t := t
  val encoding: t Data_encoding.t
  val pp: Format.formatter -> t -> unit
end

module Tez = Tez_repr
module Period = Period_repr

module Timestamp = struct
  include Time_repr
  let current = Raw_context.current_timestamp
end

include Operation_repr
module Operation = struct
  type 'kind t = 'kind operation = {
    shell: Operation.shell_header ;
    protocol_data: 'kind protocol_data ;
  }
  type packed = packed_operation
  let unsigned_encoding = unsigned_operation_encoding
  include Operation_repr
end
module Block_header = Block_header_repr
module Vote = struct
  include Vote_repr
  include Vote_storage
end
module Raw_level = Raw_level_repr
module Cycle = Cycle_repr
module Script_int = Script_int_repr
module Script_timestamp = struct
  include Script_timestamp_repr
  let now ctxt =
    Raw_context.current_timestamp ctxt
    |> Timestamp.to_seconds
    |> of_int64
end
module Script = struct
  include Michelson_v1_primitives
  include Script_repr
end
module Fees = Fees_storage

type public_key = Signature.Public_key.t
type public_key_hash = Signature.Public_key_hash.t
type signature  = Signature.t

module Constants = struct
  include Constants_repr
  include Constants_storage
end

module Voting_period = Voting_period_repr

module Gas = struct
  include Gas_limit_repr
  type error += Gas_limit_too_high = Raw_context.Gas_limit_too_high
  type error += Not_enough_gas_minimal_deserialize_parameters =
    Script_repr.Not_enough_gas_minimal_deserialize_parameters
  type error += Not_enough_gas_minimal_deserialize_storage =
    Script_repr.Not_enough_gas_minimal_deserialize_storage
  type error += Not_enough_gas_minimal_serialize_storage =
    Script_repr.Not_enough_gas_minimal_deserialize_storage
  let check_limit = Raw_context.check_gas_limit
  let set_limit = Raw_context.set_gas_limit
  let set_unlimited = Raw_context.set_gas_unlimited
  let consume = Raw_context.consume_gas
  let level = Raw_context.gas_level
  let consumed = Raw_context.gas_consumed
  let block_level = Raw_context.block_gas_level
end
module Level = struct
  include Level_repr
  include Level_storage
end
module Contract = struct
  include Contract_repr
  include Contract_storage

  let originate c contract ~balance ~manager ?script ~delegate
      ~spendable ~delegatable =
    originate c contract ~balance ~manager ?script ~delegate
      ~spendable ~delegatable
  let init_origination_nonce = Raw_context.init_origination_nonce
  let unset_origination_nonce = Raw_context.unset_origination_nonce
end
module Delegate = Delegate_storage
module Roll = struct
  include Roll_repr
  include Roll_storage
end
module Nonce = Nonce_storage
module Seed = struct
  include Seed_repr
  include Seed_storage
end

module Fitness = struct

  include Fitness_repr
  include Fitness
  type fitness = t
  include Fitness_storage

end

module Bootstrap = Bootstrap_storage

module Commitment = struct
  include Commitment_repr
  include Commitment_storage
end

module Global = struct
  let get_last_block_priority = Storage.Last_block_priority.get
  let set_last_block_priority = Storage.Last_block_priority.set
end

let prepare_first_block = Init_storage.prepare_first_block
let prepare = Init_storage.prepare

let finalize ?commit_message:message c =
  let fitness = Fitness.from_int64 (Fitness.current c) in
  let context = Raw_context.recover c in
  { Updater.context ; fitness ; message ; max_operations_ttl = 60 ;
    last_allowed_fork_level =
      Raw_level.to_int32 @@ Level.last_allowed_fork_level c;
  }

let activate = Raw_context.activate
let fork_test_chain = Raw_context.fork_test_chain

let record_endorsement = Raw_context.record_endorsement
let allowed_endorsements = Raw_context.allowed_endorsements
let init_endorsements = Raw_context.init_endorsements

let reset_internal_nonce = Raw_context.reset_internal_nonce
let fresh_internal_nonce = Raw_context.fresh_internal_nonce
let record_internal_nonce = Raw_context.record_internal_nonce
let internal_nonce_already_recorded = Raw_context.internal_nonce_already_recorded

let add_deposit = Raw_context.add_deposit
let add_fees = Raw_context.add_fees
let add_rewards = Raw_context.add_rewards

let get_deposits = Raw_context.get_deposits
let get_fees = Raw_context.get_fees
let get_rewards = Raw_context.get_rewards

let description = Raw_context.description
