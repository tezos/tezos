(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val string_of_errors: error list -> string
val handle_error: 'a tzresult -> 'a Lwt.t

type net = State.net_id = Net of Block_hash.t

type block = [
  | `Genesis
  | `Head of int | `Prevalidation
  | `Test_head of int | `Test_prevalidation
  | `Hash of Block_hash.t
]

module Constants : sig
  val errors: block -> Json_schema.schema Lwt.t
  val bootstrap: block -> Bootstrap.account list Lwt.t
  val cycle_length: block -> int32 tzresult Lwt.t
  val voting_period_length: block -> int32 tzresult Lwt.t
  val time_before_reward: block -> Period.t tzresult Lwt.t
  val time_between_slots: block -> Period.t tzresult Lwt.t
  val first_free_mining_slot: block -> int32 tzresult Lwt.t
  val max_signing_slot: block -> int tzresult Lwt.t
  val instructions_per_transaction: block -> int tzresult Lwt.t
  val stamp_threshold: block -> int tzresult Lwt.t
end

module Context : sig
  val level: block -> Level.t tzresult Lwt.t
  val next_level: block -> Level.t tzresult Lwt.t
  module Nonce : sig
    val hash: block -> Nonce_hash.t tzresult Lwt.t
    type nonce_info =
      | Revealed of Nonce.t
      | Missing of Nonce_hash.t
      | Forgotten
    val get: block -> Raw_level.t -> nonce_info tzresult Lwt.t
  end
  module Key : sig
    val get :
      block ->
      public_key_hash -> (public_key_hash * public_key) tzresult Lwt.t
    val list :
      block ->
      ((public_key_hash * public_key) list) tzresult Lwt.t
  end
  module Contract : sig
    val list: block -> Contract.t list tzresult Lwt.t
    type info = {
      manager: public_key_hash ;
      balance: Tez.t ;
      spendable: bool ;
      delegate: bool * public_key_hash option ;
      script: Script.t ;
      assets: Asset.Map.t ;
      counter: int32 ;
    }
    val get: block -> Contract.t -> info tzresult Lwt.t
    val balance:
      block -> Contract.t ->
      Tez.t tzresult Lwt.t
    val manager:
      block -> Contract.t ->
      public_key_hash tzresult Lwt.t
    val delegate:
      block -> Contract.t ->
      public_key_hash option tzresult Lwt.t
    val counter:
      block -> Contract.t ->
      int32 tzresult Lwt.t
    val spendable:
      block -> Contract.t ->
      bool tzresult Lwt.t
    val delegatable:
      block -> Contract.t ->
      bool tzresult Lwt.t
    val script:
      block -> Contract.t -> Script.t tzresult Lwt.t
    val assets:
      block -> Contract.t ->
      Asset.Map.t tzresult Lwt.t
  end
end

module Helpers : sig
  val minimal_time:
    block -> ?prio:int -> unit -> Time.t tzresult Lwt.t
  val run_code: block -> Script.code ->
    (Script.expr * Script.expr) ->
    (Script.expr * Script.expr) tzresult Lwt.t
  val trace_code: block -> Script.code ->
    (Script.expr * Script.expr) ->
    (Script.expr * Script.expr *
     (Script.location * int * Script.expr list) list) tzresult Lwt.t
  val typecheck_code: block -> Script.code -> Script_ir_translator.type_map tzresult Lwt.t
  val typecheck_tagged_data: block -> Script.expr -> unit tzresult Lwt.t
  val typecheck_untagged_data: block -> Script.expr * Script.expr -> unit tzresult Lwt.t
  val hash_data: block -> Script.expr -> string tzresult Lwt.t
  val level: block -> ?offset:int32 -> Raw_level.t -> Level.t tzresult Lwt.t
  val levels: block -> Cycle.t -> Level.t list tzresult Lwt.t

  module Rights : sig
    type slot = Raw_level.t * int * Time.t option
    val mining_rights_for_delegate:
      block -> public_key_hash ->
      ?max_priority:int -> ?first_level:Raw_level.t ->
      ?last_level:Raw_level.t -> unit ->
      (slot list) tzresult Lwt.t
    val endorsement_rights_for_delegate:
      block -> public_key_hash ->
      ?max_priority:int -> ?first_level:Raw_level.t -> ?last_level:Raw_level.t -> unit ->
      (slot list) tzresult Lwt.t
  end

  module Forge : sig
    module Manager : sig
      val operations:
        block ->
        net:net ->
        source:Contract.t ->
        ?sourcePubKey:public_key ->
        counter:int32 ->
        fee:Tez.t ->
        manager_operation list ->
        (MBytes.t * Contract.t list) tzresult Lwt.t
      val transaction:
        block ->
        net:net ->
        source:Contract.t ->
        ?sourcePubKey:public_key ->
        counter:int32 ->
        amount:Tez.t ->
        destination:Contract.t ->
        ?parameters:Script.expr ->
        fee:Tez.t ->
        unit -> MBytes.t tzresult Lwt.t
      val origination:
        block ->
        net:net ->
        source:Contract.t ->
        ?sourcePubKey:public_key ->
        counter:int32 ->
        managerPubKey:public_key_hash ->
        balance:Tez.t ->
        ?spendable:bool ->
        ?delegatable:bool ->
        ?delegatePubKey: public_key_hash ->
        ?script:(Script.code * Script.storage) ->
        fee:Tez.t->
        unit ->
        (Contract.t * MBytes.t) tzresult Lwt.t
      val issuance:
        block ->
        net:net ->
        source:Contract.t ->
        ?sourcePubKey:public_key ->
        counter:int32 ->
        assetType:(Asset.t * public_key_hash) ->
        quantity:Tez.t ->
        fee:Tez.t ->
        unit -> MBytes.t tzresult Lwt.t
      val delegation:
        block ->
        net:net ->
        source:Contract.t ->
        ?sourcePubKey:public_key ->
        counter:int32 ->
        fee:Tez.t ->
        public_key_hash option ->
        MBytes.t tzresult Lwt.t
    end
    module Delegate : sig
      val operations:
        block ->
        net:net ->
        source:public_key ->
        delegate_operation list ->
        MBytes.t tzresult Lwt.t
      val endorsement:
        block ->
        net:net ->
        source:public_key ->
        block:Block_hash.t ->
        slot:int ->
        unit -> MBytes.t tzresult Lwt.t
    end
    module Anonymous : sig
      val operations:
        block ->
        net:net ->
        anonymous_operation list ->
        MBytes.t tzresult Lwt.t
      val seed_nonce_revelation:
        block ->
        net:net ->
        level:Raw_level.t ->
        nonce:Nonce.t ->
        unit -> MBytes.t tzresult Lwt.t
    end
    val block:
      block ->
      net:net ->
      predecessor:Block_hash.t ->
      timestamp:Time.t ->
      fitness:Fitness.t ->
      operations:Operation_hash.t list ->
      level:Raw_level.t ->
      priority:int ->
      seed_nonce_hash:Nonce_hash.t ->
      proof_of_work_nonce:MBytes.t ->
      unit -> MBytes.t tzresult Lwt.t
  end

  module Parse : sig
    val operations:
      block -> ?check:bool -> Updater.shell_operation -> MBytes.t ->
      proto_operation tzresult Lwt.t
  end

end
