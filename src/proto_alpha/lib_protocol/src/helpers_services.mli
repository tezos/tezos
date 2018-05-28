(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

val minimal_time:
  'a #RPC_context.simple ->
  ?priority:int -> 'a -> Time.t shell_tzresult Lwt.t
(** [minimal_time cctxt ?prio blk] is the minimal acceptable
    timestamp for the successor of [blk]. [?priority] defaults to
    [0]. *)

val apply_operation:
  'a #RPC_context.simple ->
  'a -> Block_hash.t -> Operation_hash.t -> MBytes.t -> Signature.t option ->
  Apply_operation_result.operation_result shell_tzresult Lwt.t

val run_code:
  'a #RPC_context.simple ->
  'a -> Script.expr ->
  (Script.expr * Script.expr * Tez.t * Contract.t) ->
  (Script.expr *
   internal_operation list *
   Contract.big_map_diff option) shell_tzresult Lwt.t

val trace_code:
  'a #RPC_context.simple ->
  'a -> Script.expr ->
  (Script.expr * Script.expr * Tez.t * Contract.t) ->
  (Script.expr *
   internal_operation list *
   Script_interpreter.execution_trace *
   Contract.big_map_diff option) shell_tzresult Lwt.t

val typecheck_code:
  'a #RPC_context.simple ->
  'a -> (Script.expr * Z.t option) ->
  (Script_tc_errors.type_map * Gas.t) shell_tzresult Lwt.t

val typecheck_data:
  'a #RPC_context.simple ->
  'a -> Script.expr * Script.expr * Z.t option -> Gas.t shell_tzresult Lwt.t

val hash_data:
  'a #RPC_context.simple ->
  'a -> Script.expr * Script.expr * Z.t option -> (string * Gas.t) shell_tzresult Lwt.t

val level:
  'a #RPC_context.simple ->
  'a -> ?offset:int32 -> Raw_level.t -> Level.t shell_tzresult Lwt.t

val levels:
  'a #RPC_context.simple ->
  'a -> Cycle.t -> (Raw_level.t * Raw_level.t) shell_tzresult Lwt.t


module Forge : sig

  module Manager : sig

    val operations:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      source:Contract.t ->
      ?sourcePubKey:public_key ->
      counter:int32 ->
      fee:Tez.t ->
      gas_limit:Z.t ->
      storage_limit:Int64.t ->
      manager_operation list -> MBytes.t shell_tzresult Lwt.t

    val reveal:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      source:Contract.t ->
      sourcePubKey:public_key ->
      counter:int32 ->
      fee:Tez.t ->
      unit -> MBytes.t shell_tzresult Lwt.t

    val transaction:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      source:Contract.t ->
      ?sourcePubKey:public_key ->
      counter:int32 ->
      amount:Tez.t ->
      destination:Contract.t ->
      ?parameters:Script.expr ->
      gas_limit:Z.t ->
      storage_limit:Int64.t ->
      fee:Tez.t ->
      unit -> MBytes.t shell_tzresult Lwt.t

    val origination:
      'a #RPC_context.simple -> 'a ->
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
      gas_limit:Z.t ->
      storage_limit:Int64.t ->
      fee:Tez.t->
      unit -> MBytes.t shell_tzresult Lwt.t

    val delegation:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      source:Contract.t ->
      ?sourcePubKey:public_key ->
      counter:int32 ->
      fee:Tez.t ->
      public_key_hash option ->
      MBytes.t shell_tzresult Lwt.t

  end

  module Dictator : sig

    val operation:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      dictator_operation -> MBytes.t shell_tzresult Lwt.t

    val activate:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      Protocol_hash.t -> MBytes.t shell_tzresult Lwt.t

    val activate_testchain:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      Protocol_hash.t -> MBytes.t shell_tzresult Lwt.t

  end

  module Consensus : sig

    val endorsement:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      block:Block_hash.t ->
      level:Raw_level.t ->
      slots:int list ->
      unit -> MBytes.t shell_tzresult Lwt.t

  end

  module Amendment : sig

    val proposals:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      source:public_key_hash ->
      period:Voting_period.t ->
      proposals:Protocol_hash.t list ->
      unit -> MBytes.t shell_tzresult Lwt.t

    val ballot:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      source:public_key_hash ->
      period:Voting_period.t ->
      proposal:Protocol_hash.t ->
      ballot:Vote.ballot ->
      unit -> MBytes.t shell_tzresult Lwt.t

  end

  module Anonymous : sig

    val operations:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      anonymous_operation list -> MBytes.t shell_tzresult Lwt.t

    val seed_nonce_revelation:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      level:Raw_level.t ->
      nonce:Nonce.t ->
      unit -> MBytes.t shell_tzresult Lwt.t

  end

  val protocol_data:
    'a #RPC_context.simple -> 'a ->
    priority: int ->
    ?seed_nonce_hash: Nonce_hash.t ->
    ?proof_of_work_nonce: MBytes.t ->
    unit -> MBytes.t shell_tzresult Lwt.t

end

module Parse : sig

  val operations:
    'a #RPC_context.simple -> 'a ->
    ?check:bool -> Operation.raw list ->
    Operation.t list shell_tzresult Lwt.t

  val block:
    'a #RPC_context.simple -> 'a ->
    Block_header.shell_header -> MBytes.t ->
    Block_header.protocol_data shell_tzresult Lwt.t

end
