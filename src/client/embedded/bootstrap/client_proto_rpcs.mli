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

type block = [
  | `Genesis
  | `Head of int | `Prevalidation
  | `Test_head of int | `Test_prevalidation
  | `Hash of Block_hash.t
]

module Constants : sig
  val errors:
    Client_commands.context ->
    block -> Json_schema.schema Lwt.t
  val bootstrap:
    Client_commands.context ->
    block -> Bootstrap.account list Lwt.t
  val cycle_length:
    Client_commands.context ->
    block -> int32 tzresult Lwt.t
  val voting_period_length:
    Client_commands.context ->
    block -> int32 tzresult Lwt.t
  val time_before_reward:
    Client_commands.context ->
    block -> Period.t tzresult Lwt.t
  val slot_durations:
    Client_commands.context ->
    block -> (Period.t list) tzresult Lwt.t
  val first_free_mining_slot:
    Client_commands.context ->
    block -> int32 tzresult Lwt.t
  val max_signing_slot:
    Client_commands.context ->
    block -> int tzresult Lwt.t
  val instructions_per_transaction:
    Client_commands.context ->
    block -> int tzresult Lwt.t
  val stamp_threshold:
    Client_commands.context ->
    block -> int64 tzresult Lwt.t
end

module Context : sig
  val level:
    Client_commands.context ->
    block -> Level.t tzresult Lwt.t
  val next_level:
    Client_commands.context ->
    block -> Level.t tzresult Lwt.t
  module Nonce : sig
    val hash:
    Client_commands.context ->
    block -> Nonce_hash.t tzresult Lwt.t
    type nonce_info =
      | Revealed of Nonce.t
      | Missing of Nonce_hash.t
      | Forgotten
    val get:
    Client_commands.context ->
    block -> Raw_level.t -> nonce_info tzresult Lwt.t
  end
  module Key : sig
    val get :
      Client_commands.context ->
      block ->
      public_key_hash -> (public_key_hash * public_key) tzresult Lwt.t
    val list :
      Client_commands.context ->
      block ->
      ((public_key_hash * public_key) list) tzresult Lwt.t
  end
  module Contract : sig
    val list:
    Client_commands.context ->
    block -> Contract.t list tzresult Lwt.t
    type info = {
      manager: public_key_hash ;
      balance: Tez.t ;
      spendable: bool ;
      delegate: bool * public_key_hash option ;
      script: Script.t ;
      assets: Asset.Map.t ;
      counter: int32 ;
    }
    val get:
      Client_commands.context ->
      block -> Contract.t -> info tzresult Lwt.t
    val balance:
      Client_commands.context ->
      block -> Contract.t ->
      Tez.t tzresult Lwt.t
    val manager:
      Client_commands.context ->
      block -> Contract.t ->
      public_key_hash tzresult Lwt.t
    val delegate:
      Client_commands.context ->
      block -> Contract.t ->
      public_key_hash option tzresult Lwt.t
    val counter:
      Client_commands.context ->
      block -> Contract.t ->
      int32 tzresult Lwt.t
    val spendable:
      Client_commands.context ->
      block -> Contract.t ->
      bool tzresult Lwt.t
    val delegatable:
      Client_commands.context ->
      block -> Contract.t ->
      bool tzresult Lwt.t
    val script:
      Client_commands.context ->
      block -> Contract.t -> Script.t tzresult Lwt.t
    val assets:
      Client_commands.context ->
      block -> Contract.t ->
      Asset.Map.t tzresult Lwt.t
  end
end

module Helpers : sig
  val minimal_time:
    Client_commands.context ->
    block -> ?prio:int -> unit -> Time.t tzresult Lwt.t
  val apply_operation:
    Client_commands.context ->
    block -> Block_hash.t -> Operation_hash.t -> MBytes.t -> MBytes.t option ->
    (Contract.t list) tzresult Lwt.t
  val run_code:
    Client_commands.context ->
    block -> Script.code ->
    (Script.expr * Script.expr) ->
    (Script.expr * Script.expr) tzresult Lwt.t
  val trace_code:
    Client_commands.context ->
    block -> Script.code ->
    (Script.expr * Script.expr) ->
    (Script.expr * Script.expr *
     (Script.location * int * Script.expr list) list) tzresult Lwt.t
  val typecheck_code:
    Client_commands.context ->
    block -> Script.code -> Script_ir_translator.type_map tzresult Lwt.t
  val typecheck_data:
    Client_commands.context ->
    block -> Script.expr * Script.expr -> unit tzresult Lwt.t
  val hash_data:
    Client_commands.context ->
    block -> Script.expr -> string tzresult Lwt.t
  val level:
    Client_commands.context ->
    block -> ?offset:int32 -> Raw_level.t -> Level.t tzresult Lwt.t
  val levels:
    Client_commands.context ->
    block -> Cycle.t -> Level.t list tzresult Lwt.t

  module Rights : sig
    type slot = Raw_level.t * int * Time.t option
    val mining_rights_for_delegate:
      Client_commands.context ->
      block -> public_key_hash ->
      ?max_priority:int -> ?first_level:Raw_level.t ->
      ?last_level:Raw_level.t -> unit ->
      (slot list) tzresult Lwt.t
    val endorsement_rights_for_delegate:
      Client_commands.context ->
      block -> public_key_hash ->
      ?max_priority:int -> ?first_level:Raw_level.t -> ?last_level:Raw_level.t -> unit ->
      (slot list) tzresult Lwt.t
  end

  module Forge : sig
    module Manager : sig
      val operations:
        Client_commands.context ->
        block ->
        net:Updater.Net_id.t ->
        source:Contract.t ->
        ?sourcePubKey:public_key ->
        counter:int32 ->
        fee:Tez.t ->
        manager_operation list ->
        MBytes.t tzresult Lwt.t
      val transaction:
        Client_commands.context ->
        block ->
        net:Updater.Net_id.t ->
        source:Contract.t ->
        ?sourcePubKey:public_key ->
        counter:int32 ->
        amount:Tez.t ->
        destination:Contract.t ->
        ?parameters:Script.expr ->
        fee:Tez.t ->
        unit -> MBytes.t tzresult Lwt.t
      val origination:
        Client_commands.context ->
        block ->
        net:Updater.Net_id.t ->
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
        MBytes.t tzresult Lwt.t
      val issuance:
        Client_commands.context ->
        block ->
        net:Updater.Net_id.t ->
        source:Contract.t ->
        ?sourcePubKey:public_key ->
        counter:int32 ->
        assetType:(Asset.t * public_key_hash) ->
        quantity:Tez.t ->
        fee:Tez.t ->
        unit -> MBytes.t tzresult Lwt.t
      val delegation:
        Client_commands.context ->
        block ->
        net:Updater.Net_id.t ->
        source:Contract.t ->
        ?sourcePubKey:public_key ->
        counter:int32 ->
        fee:Tez.t ->
        public_key_hash option ->
        MBytes.t tzresult Lwt.t
    end
    module Delegate : sig
      val operations:
        Client_commands.context ->
        block ->
        net:Updater.Net_id.t ->
        source:public_key ->
        delegate_operation list ->
        MBytes.t tzresult Lwt.t
      val endorsement:
        Client_commands.context ->
        block ->
        net:Updater.Net_id.t ->
        source:public_key ->
        block:Block_hash.t ->
        slot:int ->
        unit -> MBytes.t tzresult Lwt.t
    end
    module Anonymous : sig
      val operations:
        Client_commands.context ->
        block ->
        net:Updater.Net_id.t ->
        anonymous_operation list ->
        MBytes.t tzresult Lwt.t
      val seed_nonce_revelation:
        Client_commands.context ->
        block ->
        net:Updater.Net_id.t ->
        level:Raw_level.t ->
        nonce:Nonce.t ->
        unit -> MBytes.t tzresult Lwt.t
    end
    val block:
      Client_commands.context ->
      block ->
      net:Updater.Net_id.t ->
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
      Client_commands.context ->
      block -> ?check:bool -> Updater.shell_operation -> MBytes.t ->
      proto_operation tzresult Lwt.t
  end

end
