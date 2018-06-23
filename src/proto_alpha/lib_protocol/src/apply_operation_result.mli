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

(** Remove zero-valued balances from a list of updates. *)
val cleanup_balance_updates : balance_updates -> balance_updates

(** Result of applying a {!Operation.t}. Follows the same structure. *)
type 'kind operation_metadata = {
  contents: 'kind contents_result_list ;
}

and packed_operation_metadata =
  | Operation_metadata : 'kind operation_metadata -> packed_operation_metadata
  | No_operation_metadata : packed_operation_metadata

(** Result of applying a {!Operation.contents_list}. Follows the same structure. *)
and 'kind contents_result_list =
  | Single_result : 'kind contents_result -> 'kind contents_result_list
  | Cons_result :
      'kind Kind.manager contents_result * 'rest Kind.manager contents_result_list ->
    (('kind * 'rest) Kind.manager ) contents_result_list

and packed_contents_result_list =
  | Contents_result_list : 'kind contents_result_list -> packed_contents_result_list

(** Result of applying an {!Operation.contents}. Follows the same structure. *)
and 'kind contents_result =
  | Endorsement_result :
      { balance_updates : balance_updates ;
        delegate : Signature.Public_key_hash.t ;
        slots: int list ;
      } -> Kind.endorsement contents_result
  | Seed_nonce_revelation_result :
      balance_updates -> Kind.seed_nonce_revelation contents_result
  | Double_endorsement_evidence_result :
      balance_updates -> Kind.double_endorsement_evidence contents_result
  | Double_baking_evidence_result :
      balance_updates -> Kind.double_baking_evidence contents_result
  | Activate_account_result :
      balance_updates -> Kind.activate_account contents_result
  | Proposals_result : Kind.proposals contents_result
  | Ballot_result : Kind.ballot contents_result
  | Manager_operation_result :
      { balance_updates : balance_updates ;
        operation_result : 'kind manager_operation_result ;
        internal_operation_results : packed_internal_operation_result list ;
      } -> 'kind Kind.manager contents_result

and packed_contents_result =
  | Contents_result : 'kind contents_result -> packed_contents_result

(** The result of an operation in the queue. [Skipped] ones should
    always be at the tail, and after a single [Failed]. *)
and 'kind manager_operation_result =
  | Applied of 'kind successful_manager_operation_result
  | Failed : 'kind Kind.manager * error list -> 'kind manager_operation_result
  | Skipped : 'kind Kind.manager -> 'kind manager_operation_result

(** Result of applying a {!manager_operation_content}, either internal
    or external. *)
and _ successful_manager_operation_result =
  | Reveal_result : Kind.reveal successful_manager_operation_result
  | Transaction_result :
      { storage : Script.expr option ;
        balance_updates : balance_updates ;
        originated_contracts : Contract.t list ;
        consumed_gas : Z.t ;
        storage_size : Z.t ;
        paid_storage_size_diff : Z.t ;
      } -> Kind.transaction successful_manager_operation_result
  | Origination_result :
      { balance_updates : balance_updates ;
        originated_contracts : Contract.t list ;
        consumed_gas : Z.t ;
        storage_size : Z.t ;
        paid_storage_size_diff : Z.t ;
      } -> Kind.origination successful_manager_operation_result
  | Delegation_result : Kind.delegation successful_manager_operation_result

and packed_successful_manager_operation_result =
  | Successful_manager_result :
      'kind successful_manager_operation_result -> packed_successful_manager_operation_result

and packed_internal_operation_result =
  | Internal_operation_result :
      'kind internal_operation * 'kind manager_operation_result ->
    packed_internal_operation_result

(** Serializer for {!packed_operation_result}. *)
val operation_metadata_encoding : packed_operation_metadata Data_encoding.t

val operation_data_and_metadata_encoding
  : (Operation.packed_protocol_data * packed_operation_metadata) Data_encoding.t



type 'kind contents_and_result_list =
  | Single_and_result : 'kind Alpha_context.contents * 'kind contents_result -> 'kind contents_and_result_list
  | Cons_and_result : 'kind Kind.manager Alpha_context.contents * 'kind Kind.manager contents_result * 'rest Kind.manager contents_and_result_list -> ('kind * 'rest) Kind.manager contents_and_result_list

type packed_contents_and_result_list =
  | Contents_and_result_list : 'kind contents_and_result_list -> packed_contents_and_result_list

val contents_and_result_list_encoding :
  packed_contents_and_result_list Data_encoding.t

val pack_contents_list :
  'kind contents_list -> 'kind contents_result_list ->
  'kind contents_and_result_list

val unpack_contents_list :
  'kind contents_and_result_list ->
  'kind contents_list * 'kind contents_result_list

type ('a, 'b) eq = Eq : ('a, 'a) eq
val kind_equal_list :
  'kind contents_list -> 'kind2 contents_result_list -> ('kind, 'kind2) eq option
