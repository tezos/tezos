(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module type BASIC_DATA = sig
  type t
  include Compare.S with type t := t
  val encoding: t Data_encoding.t
  val pp: Format.formatter -> t -> unit
end

type t
type context = t

module Contract_hash = Tezos_hash.Contract_hash

module Nonce_hash = Tezos_hash.Nonce_hash

type public_key = Ed25519.public_key
type public_key_hash = Ed25519.Public_key_hash.t
type secret_key = Ed25519.secret_key
type signature = Ed25519.signature

module Tez : sig

  include BASIC_DATA
  type tez = t

  val zero: tez
  val ( - ) : tez -> tez -> tez option
  val ( -? ) : tez -> tez -> tez tzresult
  val ( +? ) : tez -> tez -> tez tzresult
  val ( *? ) : tez -> int64 -> tez tzresult
  val ( / ) : tez -> int64 -> tez

  val of_string: string -> tez option
  val to_string: tez -> string

  val of_cents: int64 -> tez option
  val to_cents: tez -> int64

end

module Period : sig

  include BASIC_DATA
  type period = t

  val of_seconds: int64 -> period tzresult
  val mult: int32 -> period -> period tzresult

end

module Timestamp : sig

  include BASIC_DATA with type t = Time.t
  type time = t
  val (+?) : time -> Period.t -> time tzresult

  val of_notation: string -> time option
  val to_notation: time -> string

  val of_seconds: string -> time option
  val to_seconds: time -> string

  val set_current: context -> Time.t -> context tzresult Lwt.t
  val get_current: context -> Time.t tzresult Lwt.t

end

module Raw_level : sig

  include BASIC_DATA
  type raw_level = t
  val arg: raw_level RPC.Arg.arg

  val root: raw_level
  val succ: raw_level -> raw_level
  val pred: raw_level -> raw_level option

end

module Cycle : sig

  include BASIC_DATA
  type cycle = t
  val arg: cycle RPC.Arg.arg

  val root: cycle
  val succ: cycle -> cycle
  val pred: cycle -> cycle option

end

module Script_int : module type of Script_int_repr

module Script : sig

  type location = int

  type expr =
    | Int of location * string
    | String of location * string
    | Prim of location * string * expr list
    | Seq of location * expr list

  type code = {
    code: expr ;
    arg_type: expr ;
    ret_type: expr ;
    storage_type: expr ;
  }

  type storage = {
    storage: expr ;
    storage_type: expr ;
  }

  type t =
    | No_script
    | Script of {
        code: code ;
        storage: storage ;
      }

  val location_encoding: location Data_encoding.t
  val expr_encoding: expr Data_encoding.t
  val storage_encoding: storage Data_encoding.t
  val code_encoding: code Data_encoding.t
  val encoding: t Data_encoding.t

  val storage_cost: storage -> Tez.t
  val code_cost: code -> Tez.t

  val hash_expr: expr -> string

end

module Asset : sig
  type t
  type asset = t
  val encoding: asset Data_encoding.t
  module Map : sig
    type t
    val encoding: t Data_encoding.t
  end
end

module Bootstrap : sig
  type account = {
    public_key_hash: public_key_hash ;
    public_key: public_key ;
    secret_key: secret_key ;
  }
  val accounts: account list
  val account_encoding: account Data_encoding.t
  val refill: context -> context tzresult Lwt.t
end

module Constants : sig

  val proof_of_work_nonce_size: int
  val mining_reward: Tez.t
  val endorsement_reward: Tez.t
  val max_number_of_operations: int
  val nonce_length: int
  val seed_nonce_revelation_tip: Tez.t
  val origination_burn: Tez.t
  val mining_bond_cost: Tez.t
  val endorsement_bond_cost: Tez.t

  val cycle_length: context -> int32
  val voting_period_length: context -> int32
  val time_before_reward: context -> Period.t
  val slot_durations: context -> Period.t list
  val first_free_mining_slot: context -> int32
  val max_signing_slot: context -> int
  val instructions_per_transaction: context -> int
  val proof_of_work_threshold: context -> int64

end

module Public_key : sig

  val get:
    context -> public_key_hash -> public_key tzresult Lwt.t
  val get_option:
    context -> public_key_hash -> public_key option tzresult Lwt.t
  val set:
    context -> public_key_hash -> public_key -> context tzresult Lwt.t
  val remove:
    context -> public_key_hash -> context Lwt.t

  val list:
    context -> (public_key_hash * public_key) list tzresult Lwt.t

end

module Voting_period : sig

  include BASIC_DATA
  type voting_period = t
  val arg: voting_period RPC.Arg.arg

  val root: voting_period
  val succ: voting_period -> voting_period

  type kind =
    | Proposal
    | Testing_vote
    | Testing
    | Promotion_vote
  val kind_encoding: kind Data_encoding.encoding

end

module Level : sig

  type t = private {
    level: Raw_level.t ;
    cycle: Cycle.t ;
    cycle_position: int32 ;
    voting_period: Voting_period.t ;
    voting_period_position: int32 ;
  }
  include BASIC_DATA with type t := t
  type level = t

  val root: level

  val succ: context -> level -> level
  val pred: context -> level -> level option

  val from_raw: context -> ?offset:int32 -> Raw_level.t -> level

  val diff: level -> level -> int32

  val current: context -> level tzresult Lwt.t
  val increment_current: context -> context tzresult Lwt.t

  val last_level_in_cycle: context -> Cycle.t -> level
  val levels_in_cycle: context -> Cycle.t -> level list

end

module Fitness : sig

  include (module type of Fitness)
  type t = fitness

  val increase: context -> context tzresult Lwt.t

  val get: context -> int64 tzresult Lwt.t
  val to_int64: fitness -> int64 tzresult Lwt.t

end

module Nonce : sig

  type t
  type nonce = t
  val encoding: nonce Data_encoding.t

  val record_hash:
    context -> public_key_hash -> Tez.t -> Nonce_hash.t ->
    context tzresult Lwt.t

  val reveal:
    context -> Level.t -> nonce ->
    (context * public_key_hash * Tez.t) tzresult Lwt.t

  type status =
    | Unrevealed of {
        nonce_hash: Nonce_hash.t ;
        delegate_to_reward: public_key_hash ;
        reward_amount: Tez.t ;
      }
    | Revealed of nonce

  val get: context -> Level.t -> status tzresult Lwt.t

  val of_bytes: MBytes.t -> nonce tzresult
  val hash: nonce -> Nonce_hash.t
  val check_hash: nonce -> Nonce_hash.t -> bool

end

module Seed : sig

  val compute_for_cycle: context -> Cycle.t -> context tzresult Lwt.t
  val clear_cycle: context -> Cycle.t -> context tzresult Lwt.t

end

module Contract : sig

  include BASIC_DATA
  type contract = t
  val arg: contract RPC.Arg.arg

  val to_b58check: contract -> string
  val of_b58check: string -> contract tzresult

  val default_contract: public_key_hash -> contract
  val is_default: contract -> public_key_hash option

  val exists: context -> contract -> bool tzresult Lwt.t
  val list: context -> contract list tzresult Lwt.t

  type origination_nonce

  val origination_nonce_encoding : origination_nonce Data_encoding.t
  val originated_contract : origination_nonce -> contract
  val originated_contracts : origination_nonce -> contract list

  val initial_origination_nonce : Operation_hash.t -> origination_nonce

  val get_manager:
    context -> contract -> public_key_hash tzresult Lwt.t
  val get_delegate:
    context -> contract -> public_key_hash tzresult Lwt.t
  val get_delegate_opt:
    context -> contract -> public_key_hash option tzresult Lwt.t
  val is_delegatable:
    context -> contract -> bool tzresult Lwt.t
  val is_spendable:
    context -> contract -> bool tzresult Lwt.t
  val get_script:
    context -> contract -> Script.t tzresult Lwt.t

  val get_counter: context -> contract -> int32 tzresult Lwt.t
  val get_balance:
    context -> contract -> Tez.t tzresult Lwt.t
  val get_assets:
    context -> contract -> Asset.Map.t tzresult Lwt.t

  val set_delegate:
    context -> contract -> public_key_hash option -> context tzresult Lwt.t

  type error +=
    | Initial_amount_too_low

  val originate:
    context ->
    origination_nonce ->
    balance: Tez.t ->
    manager: public_key_hash ->
    script: Script.t ->
    delegate: public_key_hash option ->
    spendable: bool ->
    delegatable: bool -> (context * contract * origination_nonce) tzresult Lwt.t

  type error += Too_low_balance

  val spend:
    context -> contract -> Tez.t -> context tzresult Lwt.t
  val unconditional_spend:
    context -> contract -> Tez.t -> context tzresult Lwt.t

  val credit:
    context -> contract -> Tez.t -> context tzresult Lwt.t
  val issue:
    context -> contract ->
    Asset.t -> public_key_hash -> Tez.t -> context tzresult Lwt.t

  val update_script_storage:
    context -> contract -> Script.expr -> context tzresult Lwt.t

  val increment_counter:
    context -> contract -> context tzresult Lwt.t

  val check_counter_increment:
    context -> contract -> int32 -> unit tzresult Lwt.t

end

module Vote : sig

  type proposal = Protocol_hash.t

  val record_proposal:
    context -> Protocol_hash.t -> public_key_hash ->
    context tzresult Lwt.t
  val get_proposals:
    context -> int32 Protocol_hash.Map.t tzresult Lwt.t
  val clear_proposals: context -> context tzresult Lwt.t

  val freeze_listings: context -> context tzresult Lwt.t
  val clear_listings: context -> context tzresult Lwt.t
  val listing_size: context -> int32 tzresult Lwt.t
  val in_listings: context -> public_key_hash -> bool Lwt.t

  type ballot = Yay | Nay | Pass

  type ballots = {
    yay: int32 ;
    nay: int32 ;
    pass: int32 ;
  }

  val record_ballot:
    context -> public_key_hash -> ballot -> context tzresult Lwt.t
  val get_ballots: context -> ballots tzresult Lwt.t
  val clear_ballots: context -> context Lwt.t

  val get_current_period_kind:
    context -> Voting_period.kind tzresult Lwt.t
  val set_current_period_kind:
    context -> Voting_period.kind -> context tzresult Lwt.t

  val get_current_quorum: context -> int32 tzresult Lwt.t
  val set_current_quorum: context -> int32 -> context tzresult Lwt.t

  val get_current_proposal:
    context -> proposal tzresult Lwt.t
  val set_current_proposal:
    context -> proposal -> context tzresult Lwt.t
  val clear_current_proposal:
    context -> context tzresult Lwt.t

end

type operation = {
  hash: Operation_hash.t ;
  shell: Updater.shell_operation ;
  contents: proto_operation ;
  signature: signature option ;
}

and proto_operation =
  | Anonymous_operations of anonymous_operation list
  | Sourced_operations of sourced_operations

and anonymous_operation =
  | Seed_nonce_revelation of {
      level: Raw_level.t ;
      nonce: Nonce.t ;
    }

and sourced_operations =
  | Manager_operations of {
      source: Contract.t ;
      public_key: public_key option ;
      fee: Tez.t ;
      counter: counter ;
      operations: manager_operation list ;
    }
  | Delegate_operations of {
      source: public_key ;
      operations: delegate_operation list ;
    }

and manager_operation =
  | Transaction of {
      amount: Tez.t ;
      parameters: Script.expr option ;
      destination: Contract.contract ;
    }
  | Origination of {
      manager: public_key_hash ;
      delegate: public_key_hash option ;
      script: Script.t ;
      spendable: bool ;
      delegatable: bool ;
      credit: Tez.t ;
    }
  | Issuance of {
      asset: Asset.t * public_key_hash ;
      amount: Tez.t ;
    }
  | Delegation of public_key_hash option

and delegate_operation =
  | Endorsement of {
      block: Block_hash.t ;
      slot: int ;
    }
  | Proposals of {
      period: Voting_period.t ;
      proposals: Protocol_hash.t list ;
    }
  | Ballot of {
      period: Voting_period.t ;
      proposal: Protocol_hash.t ;
      ballot: Vote.ballot ;
    }

and counter = Int32.t

module Operation : sig

  type error += Cannot_parse_operation
  val parse:
    Operation_hash.t -> Updater.raw_operation -> operation tzresult

  val parse_proto:
    MBytes.t -> (proto_operation * signature option) tzresult Lwt.t

  type error += Invalid_signature
  val check_signature: public_key -> operation -> unit tzresult Lwt.t

  val forge: Updater.shell_operation -> proto_operation -> MBytes.t

  val proto_operation_encoding: proto_operation Data_encoding.t

  val unsigned_operation_encoding:
    (Updater.shell_operation * proto_operation) Data_encoding.t

  val max_operation_data_length: int

end

module Block : sig

  type header = {
    shell: Updater.shell_block ;
    proto: proto_header ;
    signature: Ed25519.signature ;
  }

  and proto_header = {
    mining_slot: mining_slot ;
    seed_nonce_hash: Nonce_hash.t ;
    proof_of_work_nonce: MBytes.t ;
  }

  and mining_slot = Raw_level.t * Int32.t

  val mining_slot_encoding: mining_slot Data_encoding.encoding

  val max_header_length: int

  val parse_header: Updater.raw_block -> header tzresult

  val unsigned_header_encoding:
    (Updater.shell_block * proto_header) Data_encoding.encoding

  val forge_header:
    Updater.shell_block -> proto_header -> MBytes.t

end

module Roll : sig

  val freeze_rolls_for_cycle: context -> Cycle.t -> context tzresult Lwt.t
  val clear_cycle: context -> Cycle.t -> context tzresult Lwt.t

  val mining_rights_owner:
    context -> Level.t -> priority:int32 -> public_key_hash tzresult Lwt.t

  val endorsement_rights_owner:
    context -> Level.t -> slot:int -> public_key_hash tzresult Lwt.t

end

module Reward : sig

  val record:
    context -> public_key_hash -> Cycle.t -> Tez.t -> context tzresult Lwt.t

  val discard:
    context -> public_key_hash -> Cycle.t -> Tez.t -> context tzresult Lwt.t

  val set_reward_time_for_cycle:
    context -> Cycle.t -> Time.t -> context tzresult Lwt.t

  val pay_due_rewards: context -> context tzresult Lwt.t

end

val init: Context.t -> context tzresult Lwt.t
val finalize: context -> Context.t tzresult Lwt.t

val configure_sandbox:
  Context.t -> Data_encoding.json option -> Context.t tzresult Lwt.t

val get_prevalidation: context -> bool Lwt.t
val set_prevalidation: context -> context Lwt.t

val activate: context -> Protocol_hash.t -> context Lwt.t
val set_test_protocol: context -> Protocol_hash.t -> context Lwt.t
val fork_test_network: context -> context Lwt.t
