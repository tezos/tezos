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

type error +=
  | Balance_too_low of Contract_repr.contract * Tez_repr.t * Tez_repr.t (* `Temporary *)
  | Counter_in_the_past of Contract_repr.contract * Z.t * Z.t (* `Branch *)
  | Counter_in_the_future of Contract_repr.contract * Z.t * Z.t (* `Temporary *)
  | Unspendable_contract of Contract_repr.contract (* `Permanent *)
  | Non_existing_contract of Contract_repr.contract (* `Temporary *)
  | Empty_implicit_contract of Signature.Public_key_hash.t (* `Temporary *)
  | Empty_transaction of Contract_repr.t (* `Temporary *)
  | Inconsistent_hash of Signature.Public_key.t * Signature.Public_key_hash.t * Signature.Public_key_hash.t (* `Permanent *)
  | Inconsistent_public_key of Signature.Public_key.t * Signature.Public_key.t (* `Permanent *)
  | Failure of string (* `Permanent *)
  | Previously_revealed_key of Contract_repr.t (* `Permanent *)
  | Unrevealed_manager_key of Contract_repr.t (* `Permanent *)

val exists: Raw_context.t -> Contract_repr.t -> bool tzresult Lwt.t
val must_exist: Raw_context.t -> Contract_repr.t -> unit tzresult Lwt.t

val allocated: Raw_context.t -> Contract_repr.t -> bool tzresult Lwt.t
val must_be_allocated: Raw_context.t -> Contract_repr.t -> unit tzresult Lwt.t


val list: Raw_context.t -> Contract_repr.t list Lwt.t

val check_counter_increment:
  Raw_context.t -> Contract_repr.t -> Z.t -> unit tzresult Lwt.t

val increment_counter:
  Raw_context.t -> Contract_repr.t -> Raw_context.t tzresult Lwt.t

val is_delegatable:
  Raw_context.t -> Contract_repr.t -> bool tzresult Lwt.t

val is_spendable: Raw_context.t -> Contract_repr.t -> bool tzresult Lwt.t

val get_manager:
  Raw_context.t -> Contract_repr.t -> Signature.Public_key_hash.t tzresult Lwt.t

val get_manager_key:
  Raw_context.t -> Contract_repr.t -> Signature.Public_key.t tzresult Lwt.t
val is_manager_key_revealed:
  Raw_context.t -> Contract_repr.t -> bool tzresult Lwt.t

val reveal_manager_key:
  Raw_context.t -> Contract_repr.t -> Signature.Public_key.t  ->
  Raw_context.t tzresult Lwt.t

val get_balance: Raw_context.t -> Contract_repr.t -> Tez_repr.t tzresult Lwt.t
val get_counter: Raw_context.t -> Contract_repr.t -> Z.t tzresult Lwt.t

val get_script:
  Raw_context.t -> Contract_repr.t -> (Raw_context.t * Script_repr.t option) tzresult Lwt.t
val get_storage:
  Raw_context.t -> Contract_repr.t -> (Raw_context.t * Script_repr.expr option) tzresult Lwt.t


type big_map_diff_item = {
  diff_key : Script_repr.expr;
  diff_key_hash : Script_expr_hash.t;
  diff_value : Script_repr.expr option;
}
type big_map_diff = big_map_diff_item list

val big_map_diff_encoding : big_map_diff Data_encoding.t

val update_script_storage:
  Raw_context.t -> Contract_repr.t ->
  Script_repr.expr -> big_map_diff option ->
  Raw_context.t tzresult Lwt.t

val credit:
  Raw_context.t -> Contract_repr.t -> Tez_repr.t ->
  Raw_context.t tzresult Lwt.t

(** checks that the contract is spendable and decrease_balance *)
val spend:
  Raw_context.t -> Contract_repr.t -> Tez_repr.t ->
  Raw_context.t tzresult Lwt.t

(** decrease_balance even if the contract is not spendable *)
val spend_from_script:
  Raw_context.t -> Contract_repr.t -> Tez_repr.t ->
  Raw_context.t tzresult Lwt.t

val originate:
  Raw_context.t ->
  ?prepaid_bootstrap_storage:bool ->
  Contract_repr.t ->
  balance:Tez_repr.t ->
  manager:Signature.Public_key_hash.t ->
  ?script:(Script_repr.t * big_map_diff option) ->
  delegate:Signature.Public_key_hash.t option ->
  spendable:bool ->
  delegatable:bool ->
  Raw_context.t tzresult Lwt.t

val fresh_contract_from_current_nonce :
  Raw_context.t -> (Raw_context.t * Contract_repr.t) tzresult Lwt.t
val originated_from_current_nonce :
  since: Raw_context.t ->
  until: Raw_context.t ->
  Contract_repr.t list tzresult Lwt.t

val init:
  Raw_context.t -> Raw_context.t tzresult Lwt.t

val used_storage_space: Raw_context.t -> Contract_repr.t -> Z.t tzresult Lwt.t
val paid_storage_space: Raw_context.t -> Contract_repr.t -> Z.t tzresult Lwt.t
val set_paid_storage_space_and_return_fees_to_pay: Raw_context.t -> Contract_repr.t -> Z.t -> (Z.t * Raw_context.t) tzresult Lwt.t

module Big_map : sig
  val mem :
    Raw_context.t -> Contract_repr.t -> Script_expr_hash.t -> (Raw_context.t * bool) tzresult Lwt.t
  val get_opt :
    Raw_context.t -> Contract_repr.t -> Script_expr_hash.t -> (Raw_context.t * Script_repr.expr option) tzresult Lwt.t
end
