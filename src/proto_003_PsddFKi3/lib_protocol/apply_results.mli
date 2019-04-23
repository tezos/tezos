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

(** Result of applying an operation, can be used for experimenting
    with protocol updates, by clients to print out a summary of the
    operation at pre-injection simulation and at confirmation time,
    and by block explorers. *)

open Alpha_context

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
      { balance_updates : Delegate.balance_updates ;
        delegate : Signature.Public_key_hash.t ;
        slots: int list ;
      } -> Kind.endorsement contents_result
  | Seed_nonce_revelation_result :
      Delegate.balance_updates -> Kind.seed_nonce_revelation contents_result
  | Double_endorsement_evidence_result :
      Delegate.balance_updates -> Kind.double_endorsement_evidence contents_result
  | Double_baking_evidence_result :
      Delegate.balance_updates -> Kind.double_baking_evidence contents_result
  | Activate_account_result :
      Delegate.balance_updates -> Kind.activate_account contents_result
  | Proposals_result : Kind.proposals contents_result
  | Ballot_result : Kind.ballot contents_result
  | Manager_operation_result :
      { balance_updates : Delegate.balance_updates ;
        operation_result : 'kind manager_operation_result ;
        internal_operation_results : packed_internal_operation_result list ;
      } -> 'kind Kind.manager contents_result

and packed_contents_result =
  | Contents_result : 'kind contents_result -> packed_contents_result

(** The result of an operation in the queue. [Skipped] ones should
    always be at the tail, and after a single [Failed]. *)
and 'kind manager_operation_result =
  | Applied of 'kind successful_manager_operation_result
  | Backtracked of 'kind successful_manager_operation_result * error list option
  | Failed : 'kind Kind.manager * error list -> 'kind manager_operation_result
  | Skipped : 'kind Kind.manager -> 'kind manager_operation_result

(** Result of applying a {!manager_operation_content}, either internal
    or external. *)
and _ successful_manager_operation_result =
  | Reveal_result :
      { consumed_gas : Z.t
      } -> Kind.reveal successful_manager_operation_result
  | Transaction_result :
      { storage : Script.expr option ;
        big_map_diff : Contract.big_map_diff option ;
        balance_updates : Delegate.balance_updates ;
        originated_contracts : Contract.t list ;
        consumed_gas : Z.t ;
        storage_size : Z.t ;
        paid_storage_size_diff : Z.t ;
        allocated_destination_contract : bool ;
      } -> Kind.transaction successful_manager_operation_result
  | Origination_result :
      { balance_updates : Delegate.balance_updates ;
        originated_contracts : Contract.t list ;
        consumed_gas : Z.t ;
        storage_size : Z.t ;
        paid_storage_size_diff : Z.t ;
      } -> Kind.origination successful_manager_operation_result
  | Delegation_result :
      { consumed_gas : Z.t
      } -> Kind.delegation successful_manager_operation_result

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

val to_list :
  packed_contents_result_list -> packed_contents_result list

val of_list :
  packed_contents_result list -> packed_contents_result_list

type ('a, 'b) eq = Eq : ('a, 'a) eq
val kind_equal_list :
  'kind contents_list -> 'kind2 contents_result_list -> ('kind, 'kind2) eq option

type block_metadata = {
  baker: Signature.Public_key_hash.t ;
  level: Level.t ;
  voting_period_kind: Voting_period.kind ;
  nonce_hash: Nonce_hash.t option ;
  consumed_gas: Z.t ;
  deactivated: Signature.Public_key_hash.t list ;
  balance_updates: Delegate.balance_updates ;
}
val block_metadata_encoding: block_metadata Data_encoding.encoding
