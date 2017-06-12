(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val string_of_errors: error list -> string
val handle_error: Client_commands.context -> 'a tzresult -> 'a Lwt.t

type block = Node_rpc_services.Blocks.block

val header:
  Client_rpcs.config -> block -> Block_header.t tzresult Lwt.t

module Header : sig
  val priority:
    Client_rpcs.config -> block -> int tzresult Lwt.t
  val seed_nonce_hash:
    Client_rpcs.config -> block -> Nonce_hash.t tzresult Lwt.t
end

module Constants : sig
  val errors:
    Client_rpcs.config ->
    block -> Json_schema.schema tzresult Lwt.t
  val cycle_length:
    Client_rpcs.config ->
    block -> int32 tzresult Lwt.t
  val voting_period_length:
    Client_rpcs.config ->
    block -> int32 tzresult Lwt.t
  val time_before_reward:
    Client_rpcs.config ->
    block -> Period.t tzresult Lwt.t
  val slot_durations:
    Client_rpcs.config ->
    block -> (Period.t list) tzresult Lwt.t
  val first_free_mining_slot:
    Client_rpcs.config ->
    block -> int tzresult Lwt.t
  val max_signing_slot:
    Client_rpcs.config ->
    block -> int tzresult Lwt.t
  val instructions_per_transaction:
    Client_rpcs.config ->
    block -> int tzresult Lwt.t
  val stamp_threshold:
    Client_rpcs.config ->
    block -> int64 tzresult Lwt.t
end

module Context : sig
  val level:
    Client_rpcs.config ->
    block -> Level.t tzresult Lwt.t
  (** [level cctxt blk] returns the (protocol view of the) level of
      [blk]. *)

  val next_level:
    Client_rpcs.config ->
    block -> Level.t tzresult Lwt.t
  (** [next_level cctxt blk] returns the (protocol view of the) level
      of the successor of [blk]. *)

  val voting_period_kind:
    Client_rpcs.config ->
    block -> Voting_period.kind tzresult Lwt.t
  (** [voting_period_kind cctxt blk] returns the voting period kind
      of [blk]. *)

  module Nonce : sig
    val hash:
    Client_rpcs.config ->
    block -> Nonce_hash.t tzresult Lwt.t
    type nonce_info =
      | Revealed of Nonce.t
      | Missing of Nonce_hash.t
      | Forgotten
    val get:
    Client_rpcs.config ->
    block -> Raw_level.t -> nonce_info tzresult Lwt.t
  end
  module Key : sig
    val get :
      Client_rpcs.config ->
      block ->
      public_key_hash -> (public_key_hash * public_key) tzresult Lwt.t
    val list :
      Client_rpcs.config ->
      block ->
      ((public_key_hash * public_key) list) tzresult Lwt.t
  end
  module Contract : sig
    val list:
    Client_rpcs.config ->
    block -> Contract.t list tzresult Lwt.t
    type info = {
      manager: public_key_hash ;
      balance: Tez.t ;
      spendable: bool ;
      delegate: bool * public_key_hash option ;
      script: Script.t option ;
      counter: int32 ;
    }
    val get:
      Client_rpcs.config ->
      block -> Contract.t -> info tzresult Lwt.t
    val balance:
      Client_rpcs.config ->
      block -> Contract.t ->
      Tez.t tzresult Lwt.t
    val manager:
      Client_rpcs.config ->
      block -> Contract.t ->
      public_key_hash tzresult Lwt.t
    val delegate:
      Client_rpcs.config ->
      block -> Contract.t ->
      public_key_hash option tzresult Lwt.t
    val counter:
      Client_rpcs.config ->
      block -> Contract.t ->
      int32 tzresult Lwt.t
    val spendable:
      Client_rpcs.config ->
      block -> Contract.t ->
      bool tzresult Lwt.t
    val delegatable:
      Client_rpcs.config ->
      block -> Contract.t ->
      bool tzresult Lwt.t
    val script:
      Client_rpcs.config ->
      block -> Contract.t -> Script.t option tzresult Lwt.t
  end
end

module Helpers : sig
  val minimal_time:
    Client_rpcs.config ->
    block -> ?prio:int -> unit -> Time.t tzresult Lwt.t
  (** [minimal_time cctxt blk ?prio ()] is the minimal acceptable
      timestamp for the successor of [blk]. [?prio] defaults to
      [0]. *)

  val apply_operation:
    Client_rpcs.config ->
    block -> Block_hash.t -> Operation_hash.t -> MBytes.t -> MBytes.t option ->
    (Contract.t list) tzresult Lwt.t
  val run_code:
    Client_rpcs.config ->
    block -> Script.code ->
    (Script.expr * Script.expr) ->
    (Script.expr * Script.expr) tzresult Lwt.t
  val trace_code:
    Client_rpcs.config ->
    block -> Script.code ->
    (Script.expr * Script.expr) ->
    (Script.expr * Script.expr *
     (Script.location * int * Script.expr list) list) tzresult Lwt.t
  val typecheck_code:
    Client_rpcs.config ->
    block -> Script.code -> Script_ir_translator.type_map tzresult Lwt.t
  val typecheck_data:
    Client_rpcs.config ->
    block -> Script.expr * Script.expr -> unit tzresult Lwt.t
  val hash_data:
    Client_rpcs.config ->
    block -> Script.expr -> string tzresult Lwt.t
  val level:
    Client_rpcs.config ->
    block -> ?offset:int32 -> Raw_level.t -> Level.t tzresult Lwt.t
  val levels:
    Client_rpcs.config ->
    block -> Cycle.t -> (Raw_level.t * Raw_level.t) tzresult Lwt.t

  module Rights : sig
    type mining_slot = Raw_level.t * int * Time.t
    type endorsement_slot = Raw_level.t * int
    val mining_rights_for_delegate:
      Client_rpcs.config ->
      block -> public_key_hash ->
      ?max_priority:int -> ?first_level:Raw_level.t ->
      ?last_level:Raw_level.t -> unit ->
      (mining_slot list) tzresult Lwt.t
    val endorsement_rights_for_delegate:
      Client_rpcs.config ->
      block -> public_key_hash ->
      ?max_priority:int -> ?first_level:Raw_level.t -> ?last_level:Raw_level.t -> unit ->
      (endorsement_slot list) tzresult Lwt.t
  end

  module Forge : sig
    module Manager : sig
      val operations:
        Client_rpcs.config ->
        block ->
        net_id:Net_id.t ->
        branch:Block_hash.t ->
        source:Contract.t ->
        ?sourcePubKey:public_key ->
        counter:int32 ->
        fee:Tez.t ->
        manager_operation list ->
        MBytes.t tzresult Lwt.t
      val transaction:
        Client_rpcs.config ->
        block ->
        net_id:Net_id.t ->
        branch:Block_hash.t ->
        source:Contract.t ->
        ?sourcePubKey:public_key ->
        counter:int32 ->
        amount:Tez.t ->
        destination:Contract.t ->
        ?parameters:Script.expr ->
        fee:Tez.t ->
        unit -> MBytes.t tzresult Lwt.t
      val origination:
        Client_rpcs.config ->
        block ->
        net_id:Net_id.t ->
        branch:Block_hash.t ->
        source:Contract.t ->
        ?sourcePubKey:public_key ->
        counter:int32 ->
        managerPubKey:public_key_hash ->
        balance:Tez.t ->
        ?spendable:bool ->
        ?delegatable:bool ->
        ?delegatePubKey: public_key_hash ->
        ?script:Script.t ->
        fee:Tez.t->
        unit ->
        MBytes.t tzresult Lwt.t
      val delegation:
        Client_rpcs.config ->
        block ->
        net_id:Net_id.t ->
        branch:Block_hash.t ->
        source:Contract.t ->
        ?sourcePubKey:public_key ->
        counter:int32 ->
        fee:Tez.t ->
        public_key_hash option ->
        MBytes.t tzresult Lwt.t
    end
    module Dictator : sig
      val operation:
        Client_rpcs.config ->
        block ->
        net_id:Net_id.t ->
        branch:Block_hash.t ->
        dictator_operation ->
        MBytes.t tzresult Lwt.t
      val activate:
        Client_rpcs.config ->
        block ->
        net_id:Net_id.t ->
        branch:Block_hash.t ->
        Protocol_hash.t ->
        MBytes.t tzresult Lwt.t
      val activate_testnet:
        Client_rpcs.config ->
        block ->
        net_id:Net_id.t ->
        branch:Block_hash.t ->
        Protocol_hash.t ->
        MBytes.t tzresult Lwt.t
    end
    module Delegate : sig
      val operations:
        Client_rpcs.config ->
        block ->
        net_id:Net_id.t ->
        branch:Block_hash.t ->
        source:public_key ->
        delegate_operation list ->
        MBytes.t tzresult Lwt.t
      val endorsement:
        Client_rpcs.config ->
        block ->
        net_id:Net_id.t ->
        branch:Block_hash.t ->
        source:public_key ->
        block:Block_hash.t ->
        slot:int ->
        unit -> MBytes.t tzresult Lwt.t
      val proposals:
        Client_rpcs.config ->
        block ->
        net_id:Net_id.t ->
        branch:Block_hash.t ->
        source:public_key ->
        period:Voting_period.t ->
        proposals:Hash.Protocol_hash.t list ->
        unit -> MBytes.t tzresult Lwt.t
      val ballot:
        Client_rpcs.config ->
        block ->
        net_id:Net_id.t ->
        branch:Block_hash.t ->
        source:public_key ->
        period:Voting_period.t ->
        proposal:Hash.Protocol_hash.t ->
        ballot:Vote.ballot ->
        unit -> MBytes.t tzresult Lwt.t
    end
    module Anonymous : sig
      val operations:
        Client_rpcs.config ->
        block ->
        net_id:Net_id.t ->
        branch:Block_hash.t ->
        anonymous_operation list ->
        MBytes.t tzresult Lwt.t
      val seed_nonce_revelation:
        Client_rpcs.config ->
        block ->
        net_id:Net_id.t ->
        branch:Block_hash.t ->
        level:Raw_level.t ->
        nonce:Nonce.t ->
        unit -> MBytes.t tzresult Lwt.t
      val faucet:
        Client_rpcs.config ->
        block ->
        net_id:Net_id.t ->
        branch:Block_hash.t ->
        id:public_key_hash ->
        unit -> MBytes.t tzresult Lwt.t
    end
    val block_proto_header:
      Client_rpcs.config ->
      block ->
      priority: int ->
      seed_nonce_hash: Nonce_hash.t ->
      ?proof_of_work_nonce: MBytes.t ->
      unit -> MBytes.t tzresult Lwt.t
  end

  module Parse : sig
    val operations:
      Client_rpcs.config ->
      block -> ?check:bool -> Operation.raw list ->
      Operation.t list tzresult Lwt.t
    val block:
      Client_rpcs.config ->
      block -> Block_header.shell_header -> MBytes.t ->
      Block_header.proto_header tzresult Lwt.t
  end

end
