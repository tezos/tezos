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
  'a -> Block_hash.t -> Operation_hash.t -> MBytes.t -> Ed25519.Signature.t option ->
  (Contract.t list) shell_tzresult Lwt.t

val run_code:
  'a #RPC_context.simple ->
  'a -> Script.expr ->
  (Script.expr * Script.expr * Tez.t) ->
  (Script.expr * Script.expr * (Script.expr * Script.expr option) list option) shell_tzresult Lwt.t

val trace_code:
  'a #RPC_context.simple ->
  'a -> Script.expr ->
  (Script.expr * Script.expr * Tez.t) ->
  (Script.expr * Script.expr *
   (Script.location * Gas.t * Script.expr list) list *
   (Script.expr * Script.expr option) list option) shell_tzresult Lwt.t

val typecheck_code:
  'a #RPC_context.simple ->
  'a -> Script.expr -> Script_tc_errors.type_map shell_tzresult Lwt.t

val typecheck_data:
  'a #RPC_context.simple ->
  'a -> Script.expr * Script.expr -> unit shell_tzresult Lwt.t

val hash_data:
  'a #RPC_context.simple ->
  'a -> Script.expr * Script.expr -> string shell_tzresult Lwt.t

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
      manager_operation list -> MBytes.t shell_tzresult Lwt.t

    val transaction:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      source:Contract.t ->
      ?sourcePubKey:public_key ->
      counter:int32 ->
      amount:Tez.t ->
      destination:Contract.t ->
      ?parameters:Script.expr ->
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

    val activate_testnet:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      Protocol_hash.t -> MBytes.t shell_tzresult Lwt.t

  end

  module Delegate : sig

    val operations:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      source:public_key ->
      delegate_operation list -> MBytes.t shell_tzresult Lwt.t

    val endorsement:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      source:public_key ->
      block:Block_hash.t ->
      slot:int ->
      unit -> MBytes.t shell_tzresult Lwt.t

    val proposals:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      source:public_key ->
      period:Voting_period.t ->
      proposals:Protocol_hash.t list ->
      unit -> MBytes.t shell_tzresult Lwt.t

    val ballot:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      source:public_key ->
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

    val faucet:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      id:public_key_hash ->
      nonce:MBytes.t ->
      unit -> MBytes.t shell_tzresult Lwt.t

  end

  val block_proto_header:
    'a #RPC_context.simple -> 'a ->
    priority: int ->
    seed_nonce_hash: Nonce_hash.t ->
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
    Block_header.proto_header shell_tzresult Lwt.t

end
