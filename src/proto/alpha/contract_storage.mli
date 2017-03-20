(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error +=
  | Initial_amount_too_low of Contract_repr.contract * Tez_repr.t * Tez_repr.t (* `Permanent *)
  | Balance_too_low of Contract_repr.contract * Tez_repr.t * Tez_repr.t (* `Temporary *)
  | Cannot_pay_storage_fee of Contract_repr.contract * Tez_repr.t * Tez_repr.t (* `Temporary *)
  | Counter_in_the_past of Contract_repr.contract * int32 * int32 (* `Branch *)
  | Counter_in_the_future of Contract_repr.contract * int32 * int32 (* `Temporary *)
  | Unspendable_contract of Contract_repr.contract (* `Permanent *)
  | Non_existing_contract of Contract_repr.contract (* `Temporary *)
  | Non_delegatable_contract of Contract_repr.contract (* `Permanent *)
  | Failure of string (* `Permanent *)

val delete : Storage.t -> Contract_repr.t -> Storage.t tzresult Lwt.t

val exists: Storage.t -> Contract_repr.t -> bool tzresult Lwt.t
val must_exist: Storage.t -> Contract_repr.t -> unit tzresult Lwt.t

val list: Storage.t -> Contract_repr.t list tzresult Lwt.t

val check_counter_increment: Storage.t -> Contract_repr.t -> int32 -> unit tzresult Lwt.t
val increment_counter: Storage.t -> Contract_repr.t -> Storage.t tzresult Lwt.t

val is_delegatable : Storage.t -> Contract_repr.t -> bool tzresult Lwt.t
val is_spendable : Storage.t -> Contract_repr.t -> bool tzresult Lwt.t

val get_manager: Storage.t -> Contract_repr.t -> Ed25519.Public_key_hash.t tzresult Lwt.t
val get_delegate_opt: Storage.t -> Contract_repr.t -> Ed25519.Public_key_hash.t option tzresult Lwt.t
val get_balance: Storage.t -> Contract_repr.t -> Tez_repr.t tzresult Lwt.t
val get_counter: Storage.t -> Contract_repr.t -> int32 tzresult Lwt.t

val get_script: Storage.t -> Contract_repr.t -> Script_repr.t option tzresult Lwt.t

val update_script_storage_and_fees: Storage.t -> Contract_repr.t -> Tez_repr.t -> Script_repr.expr -> Storage.t tzresult Lwt.t

(** fails if the contract is not delegatable *)
val set_delegate : Storage.t -> Contract_repr.t -> Ed25519.Public_key_hash.t option -> Storage.t tzresult Lwt.t

val credit : Storage.t -> Contract_repr.t -> Tez_repr.t -> Storage.t tzresult Lwt.t

(** checks that the contract is spendable and decrease_balance *)
val spend : Storage.t -> Contract_repr.t -> Tez_repr.t -> Storage.t tzresult Lwt.t

(** decrease_balance even if the contract is not spendable *)
val spend_from_script : Storage.t -> Contract_repr.t -> Tez_repr.t -> Storage.t tzresult Lwt.t

val originate :
  Storage.t ->
  Contract_repr.origination_nonce ->
  balance:Tez_repr.t ->
  manager:Ed25519.Public_key_hash.t ->
  ?script:(Script_repr.t * (Tez_repr.t * Tez_repr.t)) ->
  delegate:Ed25519.Public_key_hash.t option ->
  spendable:bool ->
  delegatable:bool ->
  (Storage.t * Contract_repr.t * Contract_repr.origination_nonce) tzresult Lwt.t

val init :
  Storage.t -> Storage.t tzresult Lwt.t
