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

open Alpha_context

type error += Cannot_parse_operation (* `Branch *)

val current_level:
  'a #RPC_context.simple ->
  ?offset:int32 -> 'a -> Level.t shell_tzresult Lwt.t

val levels_in_current_cycle:
  'a #RPC_context.simple ->
  ?offset:int32 -> 'a -> (Raw_level.t * Raw_level.t) shell_tzresult Lwt.t

module Scripts : sig

  val run_code:
    'a #RPC_context.simple ->
    'a -> Script.expr -> (Script.expr * Script.expr * Tez.t) ->
    (Script.expr *
     packed_internal_operation list *
     Contract.big_map_diff option) shell_tzresult Lwt.t

  val trace_code:
    'a #RPC_context.simple ->
    'a -> Script.expr ->
    (Script.expr * Script.expr * Tez.t) ->
    (Script.expr *
     packed_internal_operation list *
     Script_interpreter.execution_trace *
     Contract.big_map_diff option) shell_tzresult Lwt.t

  val typecheck_code:
    'a #RPC_context.simple ->
    'a -> (Script.expr * Z.t option) ->
    (Script_tc_errors.type_map * Gas.t) shell_tzresult Lwt.t

  val typecheck_data:
    'a #RPC_context.simple ->
    'a -> Script.expr * Script.expr * Z.t option -> Gas.t shell_tzresult Lwt.t

  val pack_data:
    'a #RPC_context.simple ->
    'a -> Script.expr * Script.expr * Z.t option -> (MBytes.t * Gas.t) shell_tzresult Lwt.t

  val run_operation:
    'a #RPC_context.simple ->
    'a -> packed_operation ->
    (packed_protocol_data * Apply_results.packed_operation_metadata) shell_tzresult Lwt.t

end

module Forge : sig

  module Manager : sig

    val operations:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      source:Contract.t ->
      ?sourcePubKey:public_key ->
      counter:counter ->
      fee:Tez.t ->
      gas_limit:Z.t ->
      storage_limit:Z.t ->
      packed_manager_operation list -> MBytes.t shell_tzresult Lwt.t

    val reveal:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      source:Contract.t ->
      sourcePubKey:public_key ->
      counter:counter ->
      fee:Tez.t ->
      unit -> MBytes.t shell_tzresult Lwt.t

    val transaction:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      source:Contract.t ->
      ?sourcePubKey:public_key ->
      counter:counter ->
      amount:Tez.t ->
      destination:Contract.t ->
      ?parameters:Script.expr ->
      gas_limit:Z.t ->
      storage_limit:Z.t ->
      fee:Tez.t ->
      unit -> MBytes.t shell_tzresult Lwt.t

    val origination:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      source:Contract.t ->
      ?sourcePubKey:public_key ->
      counter:counter ->
      managerPubKey:public_key_hash ->
      balance:Tez.t ->
      ?spendable:bool ->
      ?delegatable:bool ->
      ?delegatePubKey: public_key_hash ->
      ?script:Script.t ->
      gas_limit:Z.t ->
      storage_limit:Z.t ->
      fee:Tez.t->
      unit -> MBytes.t shell_tzresult Lwt.t

    val delegation:
      'a #RPC_context.simple -> 'a ->
      branch:Block_hash.t ->
      source:Contract.t ->
      ?sourcePubKey:public_key ->
      counter:counter ->
      fee:Tez.t ->
      public_key_hash option ->
      MBytes.t shell_tzresult Lwt.t

  end

  val endorsement:
    'a #RPC_context.simple -> 'a ->
    branch:Block_hash.t ->
    level:Raw_level.t ->
    unit -> MBytes.t shell_tzresult Lwt.t

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

  val seed_nonce_revelation:
    'a #RPC_context.simple -> 'a ->
    branch:Block_hash.t ->
    level:Raw_level.t ->
    nonce:Nonce.t ->
    unit -> MBytes.t shell_tzresult Lwt.t

  val double_baking_evidence:
    'a #RPC_context.simple -> 'a ->
    branch:Block_hash.t ->
    bh1: Block_header.t ->
    bh2: Block_header.t ->
    unit -> MBytes.t shell_tzresult Lwt.t

  val double_endorsement_evidence:
    'a #RPC_context.simple -> 'a ->
    branch:Block_hash.t ->
    op1: Kind.endorsement operation ->
    op2: Kind.endorsement operation ->
    unit -> MBytes.t shell_tzresult Lwt.t

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
    Operation.packed list shell_tzresult Lwt.t

  val block:
    'a #RPC_context.simple -> 'a ->
    Block_header.shell_header -> MBytes.t ->
    Block_header.protocol_data shell_tzresult Lwt.t

end

val register: unit -> unit
