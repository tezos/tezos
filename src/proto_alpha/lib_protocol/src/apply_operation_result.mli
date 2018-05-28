(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Result of applying an operation, can be used for experimenting
    with protocol updates, by clients to print out a summary of the
    operation at pre-injection simulation and at confirmation time,
    and by block explorers. *)

open Alpha_context

(** Places where tezzies can be found in the ledger's state. *)
type balance =
  | Contract of Contract.t
  | Rewards of Signature.Public_key_hash.t * Cycle.t
  | Fees of Signature.Public_key_hash.t * Cycle.t
  | Deposits of Signature.Public_key_hash.t * Cycle.t

(** A credit or debit of tezzies to a balance. *)
type balance_update =
  | Debited of Tez.t
  | Credited of Tez.t

(** A list of balance updates. Duplicates may happen. *)
type balance_updates = (balance * balance_update) list

(** Result of applying a {!proto_operation}. Follows the same structure. *)
type operation_result =
  | Anonymous_operations_result of anonymous_operation_result list
  | Sourced_operation_result of sourced_operation_result

(** Result of applying an {!anonymous_operation}. Follows the same structure. *)
and anonymous_operation_result =
  | Seed_nonce_revelation_result of balance_updates
  | Double_endorsement_evidence_result of balance_updates
  | Double_baking_evidence_result of balance_updates
  | Activation_result of balance_updates

(** Result of applying a {!sourced_operation}.
    Follows the same structure, except for [Manager_operations_result]
    which includes the results of internal operations, in execution order. *)
and sourced_operation_result =
  | Consensus_operation_result of consensus_operation_result
  | Amendment_operation_result
  | Manager_operations_result of
      { balance_updates : balance_updates ;
        operation_results : (manager_operation_kind * manager_operation_result) list }
  | Dictator_operation_result

(** Result of applying a {!consensus_operation}. Follows the same structure. *)
and consensus_operation_result =
  | Endorsements_result of Signature.Public_key_hash.t * int list

(** An operation descriptor in the queue of emitted manager
    operations. [External] points to a {!manager_operation_content} in
    the toplevel {!manager_operation}. The operations are executed in a
    queue, so the n-th [External] corresponds to the [n-th]
    {!manager_operation_content}. [Internal] points to an operation
    emitted by a contract, whose contents is given verbatim. *)
and manager_operation_kind =
  | External
  | Internal of internal_operation

(** The result of an operation in the queue. [Skipped] ones should
    always be at the tail, and after a single [Failed]. *)
and manager_operation_result =
  | Applied of successful_manager_operation_result
  | Failed of error list
  | Skipped

(** Result of applying a {!manager_operation_content}, either internal
    or external. *)
and successful_manager_operation_result =
  | Reveal_result
  | Transaction_result of
      { operations : internal_operation list ;
        storage : Script.expr option ;
        balance_updates : balance_updates ;
        originated_contracts : Contract.t list ;
        consumed_gas : Z.t ;
        storage_size_diff : Int64.t }
  | Origination_result of
      { balance_updates : balance_updates ;
        originated_contracts : Contract.t list ;
        consumed_gas : Z.t ;
        storage_size_diff : Int64.t }
  | Delegation_result

(** Serializer for {!proto_operation_result}. *)
val encoding : operation_result Data_encoding.t
