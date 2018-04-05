(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
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

type public_key = Signature.Public_key.t
type public_key_hash = Signature.Public_key_hash.t
type signature = Signature.t

module Tez : sig

  include BASIC_DATA
  type tez = t

  val zero: tez
  val one_mutez: tez
  val one_cent: tez
  val fifty_cents: tez
  val one: tez

  val ( -? ) : tez -> tez -> tez tzresult
  val ( +? ) : tez -> tez -> tez tzresult
  val ( *? ) : tez -> int64 -> tez tzresult
  val ( /? ) : tez -> int64 -> tez tzresult

  val of_string: string -> tez option
  val to_string: tez -> string

  val of_mutez: int64 -> tez option
  val to_mutez: tez -> int64

end

module Period : sig

  include BASIC_DATA
  type period = t

  val of_seconds: int64 -> period tzresult
  val mult: int32 -> period -> period tzresult

  val one_second: period
  val one_minute: period
  val one_hour: period

end

module Timestamp : sig

  include BASIC_DATA with type t = Time.t
  type time = t
  val (+?) : time -> Period.t -> time tzresult

  val of_notation: string -> time option
  val to_notation: time -> string

  val of_seconds: string -> time option
  val to_seconds_string: time -> string

  val current: context -> time

end

module Raw_level : sig

  include BASIC_DATA
  type raw_level = t
  val arg: raw_level RPC_arg.arg

  val diff: raw_level -> raw_level -> int32

  val root: raw_level
  val succ: raw_level -> raw_level
  val pred: raw_level -> raw_level option
  val to_int32: raw_level -> int32
  val of_int32: int32 -> raw_level tzresult

end

module Cycle : sig

  include BASIC_DATA
  type cycle = t
  val arg: cycle RPC_arg.arg

  val root: cycle
  val succ: cycle -> cycle
  val pred: cycle -> cycle option
  val add: cycle -> int -> cycle
  val sub: cycle -> int -> cycle option
  val to_int32: cycle -> int32

end

module Gas : sig
  type t = private
    | Unaccounted
    | Limited of { remaining : Z.t }

  val encoding : t Data_encoding.encoding
  val pp : Format.formatter -> t -> unit

  type cost

  val cost_encoding : cost Data_encoding.encoding
  val pp_cost : Format.formatter -> cost -> unit

  type error += Block_quota_exceeded (* `Temporary *)
  type error += Operation_quota_exceeded (* `Temporary *)

  val free : cost
  val step_cost : int -> cost
  val alloc_cost : int -> cost
  val alloc_bytes_cost : int -> cost
  val alloc_bits_cost : int -> cost

  val ( *@ ) : int -> cost -> cost
  val ( +@ ) : cost -> cost -> cost

  val set_limit: context -> Z.t -> context tzresult
  val set_unlimited: context -> context
  val consume: context -> cost -> context tzresult
  val level: context -> t
  val block_level: context -> Z.t
end

module Script_int : module type of Script_int_repr

module Script_timestamp : sig
  open Script_int
  type t
  val compare: t -> t -> int
  val to_string: t -> string
  val to_notation: t -> string option
  val to_num_str: t -> string
  val of_string: string -> t option
  val diff: t -> t -> z num
  val add_delta: t -> z num -> t
  val sub_delta: t -> z num -> t
  val now: context -> t
  val to_zint: t -> Z.t
  val of_zint: Z.t -> t
end

module Script : sig

  type prim = Michelson_v1_primitives.prim =
    | K_parameter
    | K_storage
    | K_code
    | D_False
    | D_Elt
    | D_Left
    | D_None
    | D_Pair
    | D_Right
    | D_Some
    | D_True
    | D_Unit
    | I_H
    | I_ABS
    | I_ADD
    | I_AMOUNT
    | I_AND
    | I_BALANCE
    | I_CAR
    | I_CDR
    | I_CHECK_SIGNATURE
    | I_COMPARE
    | I_CONCAT
    | I_CONS
    | I_CREATE_ACCOUNT
    | I_CREATE_CONTRACT
    | I_IMPLICIT_ACCOUNT
    | I_DIP
    | I_DROP
    | I_DUP
    | I_EDIV
    | I_EMPTY_MAP
    | I_EMPTY_SET
    | I_EQ
    | I_EXEC
    | I_FAIL
    | I_GE
    | I_GET
    | I_GT
    | I_HASH_KEY
    | I_IF
    | I_IF_CONS
    | I_IF_LEFT
    | I_IF_NONE
    | I_INT
    | I_LAMBDA
    | I_LE
    | I_LEFT
    | I_LOOP
    | I_LSL
    | I_LSR
    | I_LT
    | I_MANAGER
    | I_MAP
    | I_MEM
    | I_MUL
    | I_NEG
    | I_NEQ
    | I_NIL
    | I_NONE
    | I_NOT
    | I_NOW
    | I_OR
    | I_PAIR
    | I_PUSH
    | I_REDUCE
    | I_RIGHT
    | I_SIZE
    | I_SOME
    | I_SOURCE
    | I_SELF
    | I_STEPS_TO_QUOTA
    | I_SUB
    | I_SWAP
    | I_TRANSFER_TOKENS
    | I_UNIT
    | I_UPDATE
    | I_XOR
    | I_ITER
    | I_LOOP_LEFT
    | T_bool
    | T_contract
    | T_int
    | T_key
    | T_key_hash
    | T_lambda
    | T_list
    | T_map
    | T_big_map
    | T_nat
    | T_option
    | T_or
    | T_pair
    | T_set
    | T_signature
    | T_string
    | T_tez
    | T_timestamp
    | T_unit
    | T_operation

  type location = Micheline.canonical_location

  type expr = prim Micheline.canonical

  type node = (location, prim) Micheline.node

  type t =
    { code: expr ;
      storage: expr }

  val location_encoding: location Data_encoding.t
  val expr_encoding: expr Data_encoding.t
  val prim_encoding: prim Data_encoding.t
  val encoding: t Data_encoding.t
end

module Constants : sig

  (** Fixed constants *)
  type fixed = {
    proof_of_work_nonce_size : int ;
    nonce_length : int ;
    max_revelations_per_block : int ;
  }
  val fixed_encoding: fixed Data_encoding.t
  val fixed: fixed

  val proof_of_work_nonce_size: int
  val nonce_length: int
  val max_revelations_per_block: int


  (** Constants parameterized by context *)
  type parametric = {
    preserved_cycles: int ;
    blocks_per_cycle: int32 ;
    blocks_per_commitment: int32 ;
    blocks_per_roll_snapshot: int32 ;
    blocks_per_voting_period: int32 ;
    time_between_blocks: Period.t list ;
    first_free_baking_slot: int ;
    endorsers_per_block: int ;
    hard_gas_limit_per_operation: Z.t ;
    hard_gas_limit_per_block: Z.t ;
    proof_of_work_threshold: int64 ;
    dictator_pubkey: Signature.Public_key.t ;
    max_operation_data_length: int ;
    tokens_per_roll: Tez.t ;
    michelson_maximum_type_size: int;
    seed_nonce_revelation_tip: Tez.t ;
    origination_burn: Tez.t ;
    block_security_deposit: Tez.t ;
    endorsement_security_deposit: Tez.t ;
    block_reward: Tez.t ;
    endorsement_reward: Tez.t ;
    cost_per_byte: Tez.t ;
  }
  val parametric_encoding: parametric Data_encoding.t
  val parametric: context -> parametric

  val preserved_cycles: context -> int
  val blocks_per_cycle: context -> int32
  val blocks_per_commitment: context -> int32
  val blocks_per_roll_snapshot: context -> int32
  val blocks_per_voting_period: context -> int32
  val time_between_blocks: context -> Period.t list
  val first_free_baking_slot: context -> int
  val endorsers_per_block: context -> int
  val hard_gas_limit_per_operation: context -> Z.t
  val hard_gas_limit_per_block: context -> Z.t
  val proof_of_work_threshold: context -> int64
  val dictator_pubkey: context -> Signature.Public_key.t
  val max_operation_data_length: context -> int
  val tokens_per_roll: context -> Tez.t
  val michelson_maximum_type_size: context -> int
  val block_reward: context -> Tez.t
  val endorsement_reward: context -> Tez.t
  val seed_nonce_revelation_tip: context -> Tez.t
  val origination_burn: context -> Tez.t
  val block_security_deposit: context -> Tez.t
  val endorsement_security_deposit: context -> Tez.t

  type t = {
    fixed : fixed ;
    parametric : parametric ;
  }
  val encoding: t Data_encoding.t

end

module Voting_period : sig

  include BASIC_DATA
  type voting_period = t
  val arg: voting_period RPC_arg.arg

  val root: voting_period
  val succ: voting_period -> voting_period

  type kind =
    | Proposal
    | Testing_vote
    | Testing
    | Promotion_vote
  val kind_encoding: kind Data_encoding.encoding
  val to_int32: voting_period -> int32

end

module Level : sig

  type t = private {
    level: Raw_level.t ;
    level_position: int32 ;
    cycle: Cycle.t ;
    cycle_position: int32 ;
    voting_period: Voting_period.t ;
    voting_period_position: int32 ;
    expected_commitment: bool ;
  }
  include BASIC_DATA with type t := t
  val pp_full: Format.formatter -> t -> unit
  type level = t

  val root: context -> level

  val succ: context -> level -> level
  val pred: context -> level -> level option

  val from_raw: context -> ?offset:int32 -> Raw_level.t -> level

  val diff: level -> level -> int32

  val current: context -> level

  val last_level_in_cycle: context -> Cycle.t -> level
  val levels_in_cycle: context -> Cycle.t -> level list

  val last_allowed_fork_level: context -> Raw_level.t

end

module Fitness : sig

  include (module type of Fitness)
  type fitness = t

  val increase: ?gap:int -> context -> context

  val current: context -> int64

  val to_int64: fitness -> int64 tzresult

end

module Nonce : sig

  type t
  type nonce = t
  val encoding: nonce Data_encoding.t

  type unrevealed = {
    nonce_hash: Nonce_hash.t ;
    delegate: public_key_hash ;
    rewards: Tez.t ;
    fees: Tez.t ;
  }

  val record_hash:
    context -> unrevealed -> context tzresult Lwt.t

  val reveal:
    context -> Level.t -> nonce ->
    context tzresult Lwt.t

  type status =
    | Unrevealed of unrevealed
    | Revealed of nonce

  val get: context -> Level.t -> status tzresult Lwt.t

  val of_bytes: MBytes.t -> nonce tzresult
  val hash: nonce -> Nonce_hash.t
  val check_hash: nonce -> Nonce_hash.t -> bool

end

module Seed : sig

  type error +=
    | Unknown of { oldest : Cycle.t ;
                   cycle : Cycle.t ;
                   latest : Cycle.t }

  val cycle_end:
    context -> Cycle.t -> (context * Nonce.unrevealed list) tzresult Lwt.t

end

module Contract : sig

  include BASIC_DATA
  type contract = t
  val arg: contract RPC_arg.arg

  val to_b58check: contract -> string
  val of_b58check: string -> contract tzresult

  val implicit_contract: public_key_hash -> contract
  val is_implicit: contract -> public_key_hash option

  val exists: context -> contract -> bool tzresult Lwt.t
  val must_exist: context -> contract -> unit tzresult Lwt.t

  val allocated: context -> contract -> bool tzresult Lwt.t
  val must_be_allocated: context -> contract -> unit tzresult Lwt.t

  val list: context -> contract list Lwt.t

  type origination_nonce

  val origination_nonce_encoding: origination_nonce Data_encoding.t
  val originated_contract: origination_nonce -> contract
  val originated_contracts: origination_nonce -> contract list

  val initial_origination_nonce: Operation_hash.t -> origination_nonce

  val get_manager:
    context -> contract -> public_key_hash tzresult Lwt.t

  val get_manager_key:
    context -> contract -> public_key tzresult Lwt.t
  val is_manager_key_revealed:
    context -> contract -> bool tzresult Lwt.t

  val reveal_manager_key:
    context -> contract -> public_key -> context tzresult Lwt.t

  val is_delegatable:
    context -> contract -> bool tzresult Lwt.t
  val is_spendable:
    context -> contract -> bool tzresult Lwt.t
  val get_script:
    context -> contract -> (context * Script.t option) tzresult Lwt.t
  val get_storage:
    context -> contract -> (context * Script.expr option) tzresult Lwt.t

  val get_counter: context -> contract -> int32 tzresult Lwt.t
  val get_balance:
    context -> contract -> Tez.t tzresult Lwt.t

  type big_map_diff = (string * Script.expr option) list

  val originate:
    context ->
    origination_nonce ->
    balance: Tez.t ->
    manager: public_key_hash ->
    ?script: (Script.t * big_map_diff option) ->
    delegate: public_key_hash option ->
    spendable: bool ->
    delegatable: bool -> (context * contract * origination_nonce) tzresult Lwt.t

  type error += Balance_too_low of contract * Tez.t * Tez.t

  val spend:
    context -> contract -> Tez.t -> context tzresult Lwt.t
  val spend_from_script:
    context -> contract -> Tez.t -> context tzresult Lwt.t

  val credit:
    context -> contract -> Tez.t -> context tzresult Lwt.t

  val update_script_storage:
    context -> contract ->
    Script.expr -> big_map_diff option ->
    context tzresult Lwt.t

  val fees: context -> t -> Tez.t tzresult Lwt.t
  val paid_fees: context -> t -> Tez.t tzresult Lwt.t
  val add_to_paid_fees: context -> t -> Tez.t -> context tzresult Lwt.t

  val increment_counter:
    context -> contract -> context tzresult Lwt.t

  val check_counter_increment:
    context -> contract -> int32 -> unit tzresult Lwt.t

  module Big_map : sig
    val mem:
      context -> contract -> string -> (context * bool) tzresult Lwt.t
    val get_opt:
      context -> contract -> string -> (context * Script_repr.expr option) tzresult Lwt.t
  end

end

module Delegate : sig

  val get: context -> Contract.t -> public_key_hash option tzresult Lwt.t

  val set:
    context -> Contract.t -> public_key_hash option -> context tzresult Lwt.t

  val fold:
    context ->
    init:'a -> f:(public_key_hash -> 'a -> 'a Lwt.t) -> 'a Lwt.t

  val list: context -> public_key_hash list Lwt.t

  val freeze_deposit:
    context -> public_key_hash -> Tez.t -> context tzresult Lwt.t

  val freeze_rewards:
    context -> public_key_hash -> Tez.t -> context tzresult Lwt.t

  val freeze_fees:
    context -> public_key_hash -> Tez.t -> context tzresult Lwt.t

  val cycle_end:
    context -> Cycle.t -> Nonce.unrevealed list -> context tzresult Lwt.t

  val punish:
    context -> public_key_hash -> Cycle.t ->
    (context * Tez.t) tzresult Lwt.t

  val has_frozen_balance:
    context -> public_key_hash -> Cycle.t ->
    bool tzresult Lwt.t

  val frozen_balance:
    context -> public_key_hash -> Tez.t tzresult Lwt.t

  type frozen_balances = {
    deposit : Tez.t ;
    fees : Tez.t ;
    rewards : Tez.t ;
  }

  val frozen_balances:
    context -> public_key_hash -> frozen_balances tzresult Lwt.t

  val full_balance:
    context -> public_key_hash -> Tez.t tzresult Lwt.t

end

module Vote : sig

  type proposal = Protocol_hash.t

  val record_proposal:
    context -> Protocol_hash.t -> public_key_hash ->
    context Lwt.t
  val get_proposals:
    context -> int32 Protocol_hash.Map.t Lwt.t
  val clear_proposals: context -> context Lwt.t

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
    context -> public_key_hash -> ballot -> context Lwt.t
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
  val init_current_proposal:
    context -> proposal -> context tzresult Lwt.t
  val clear_current_proposal:
    context -> context tzresult Lwt.t

end

module Block_header : sig

  type t = {
    shell: Block_header.shell_header ;
    protocol_data: protocol_data ;
    signature: Signature.t ;
  }

  and protocol_data = {
    priority: int ;
    seed_nonce_hash: Nonce_hash.t option ;
    proof_of_work_nonce: MBytes.t ;
  }

  type block_header = t

  type raw = Block_header.t
  type shell_header = Block_header.shell_header

  val hash: block_header -> Block_hash.t
  val hash_raw: raw -> Block_hash.t

  val encoding: block_header Data_encoding.encoding
  val raw_encoding: raw Data_encoding.t
  val protocol_data_encoding: protocol_data Data_encoding.encoding
  val shell_header_encoding: shell_header Data_encoding.encoding

  val max_header_length: int
  (** The maximum size of block headers in bytes *)

  val parse: Block_header.t -> block_header tzresult
  (** Parse the protocol-specific part of a block header. *)

  val parse_unsigned_protocol_data: MBytes.t -> protocol_data tzresult
  (** Parse the (unsigned) protocol-specific part of a block header. *)

  val forge_unsigned_protocol_data: protocol_data -> MBytes.t
  (** [forge_header proto_hdr] is the binary serialization
      (using [protocol_data_encoding]) of the protocol-specific part
      of a block header, without the signature. *)

  val forge_unsigned:
    Block_header.shell_header -> protocol_data -> MBytes.t
    (** [forge_header shell_hdr proto_hdr] is the binary serialization
        (using [unsigned_header_encoding]) of a block header,
        comprising both the shell and the protocol part of the header,
        without the signature. *)

end

type operation = {
  shell: Operation.shell_header ;
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
  | Double_endorsement_evidence of {
      op1: operation ;
      op2: operation ;
    }
  | Double_baking_evidence of {
      bh1: Block_header.t ;
      bh2: Block_header.t ;
    }
  | Activation of {
      id: Ed25519.Public_key_hash.t ;
      secret: Blinded_public_key_hash.secret ;
    }

and sourced_operations =
  | Consensus_operation of consensus_operation
  | Amendment_operation of {
      source: Signature.Public_key_hash.t ;
      operation: amendment_operation ;
    }
  | Manager_operations of {
      source: Contract.contract ;
      fee: Tez.t ;
      counter: counter ;
      operations: manager_operation list ;
      gas_limit: Z.t ;
    }
  | Dictator_operation of dictator_operation

and consensus_operation =
  | Endorsements of {
      block: Block_hash.t ;
      level: Raw_level.t ;
      slots: int list ;
    }

and amendment_operation =
  | Proposals of {
      period: Voting_period.t ;
      proposals: Protocol_hash.t list ;
    }
  | Ballot of {
      period: Voting_period.t ;
      proposal: Protocol_hash.t ;
      ballot: Vote.ballot ;
    }

and manager_operation =
  | Reveal of Signature.Public_key.t
  | Transaction of {
      amount: Tez.t ;
      parameters: Script.expr option ;
      destination: Contract.contract ;
    }
  | Origination of {
      manager: public_key_hash ;
      delegate: public_key_hash option ;
      script: Script.t option ;
      spendable: bool ;
      delegatable: bool ;
      credit: Tez.t ;
    }
  | Delegation of public_key_hash option

and dictator_operation =
  | Activate of Protocol_hash.t
  | Activate_testchain of Protocol_hash.t

and counter = Int32.t

type internal_operation = {
  source: Contract.contract ;
  operation: manager_operation ;
  signature: Signature.t option
}

module Operation : sig

  type raw = Operation.t = {
    shell: Operation.shell_header ;
    proto: MBytes.t ;
  }
  val raw_encoding: raw Data_encoding.t

  type t = operation
  val encoding: operation Data_encoding.t

  val hash: operation -> Operation_hash.t
  val hash_raw: raw -> Operation_hash.t

  type error += Cannot_parse_operation (* `Branch *)
  val parse: Operation.t -> operation tzresult
  val acceptable_passes: operation -> int list

  val parse_proto:
    MBytes.t -> (proto_operation * signature option) tzresult Lwt.t

  type error += Missing_signature (* `Permanent *)
  type error += Invalid_signature (* `Permanent *)

  val check_signature: public_key -> operation -> unit tzresult Lwt.t

  val forge: Operation.shell_header -> proto_operation -> MBytes.t

  val proto_operation_encoding: proto_operation Data_encoding.t

  val unsigned_operation_encoding:
    (Operation.shell_header * proto_operation) Data_encoding.t

  val internal_operation_encoding: internal_operation Data_encoding.t

end

module Roll : sig

  val snapshot_rolls: context -> context tzresult Lwt.t
  val cycle_end: context -> Cycle.t -> context tzresult Lwt.t

  val baking_rights_owner:
    context -> Level.t -> priority:int -> public_key tzresult Lwt.t

  val endorsement_rights_owner:
    context -> Level.t -> slot:int -> public_key tzresult Lwt.t

  val delegate_pubkey:
    context -> public_key_hash -> public_key tzresult Lwt.t

end

module Commitment : sig

  type t =
    { blinded_public_key_hash : Blinded_public_key_hash.t ;
      amount : Tez.tez }

  val get_opt:
    context ->  Unclaimed_public_key_hash.t -> t option tzresult Lwt.t
  val delete:
    context ->  Unclaimed_public_key_hash.t -> context tzresult Lwt.t

end

module Bootstrap : sig

  val cycle_end:
    context -> Cycle.t -> context tzresult Lwt.t

end

val prepare_first_block:
  Context.t ->
  level:Int32.t ->
  timestamp:Time.t ->
  fitness:Fitness.t ->
  context tzresult Lwt.t

val prepare:
  Context.t ->
  level:Int32.t ->
  timestamp:Time.t ->
  fitness:Fitness.t ->
  context tzresult Lwt.t

val finalize: ?commit_message:string -> context -> Updater.validation_result

val activate: context -> Protocol_hash.t -> context Lwt.t
val fork_test_chain: context -> Protocol_hash.t -> Time.t -> context Lwt.t

val endorsement_already_recorded: context -> int -> bool
val record_endorsement: context -> int -> context

val add_fees: context -> Tez.t -> context tzresult Lwt.t
val add_rewards: context -> Tez.t -> context tzresult Lwt.t

val get_fees: context -> Tez.t
val get_rewards: context -> Tez.t
