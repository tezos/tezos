(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error +=
  | Insert_coin of Contract_repr.t
  | Initial_amount_too_low
  | Failure of string
  | Invalid_counter of Contract_repr.t * int32 * int32
  | Code_without_storage
  | Unspendable_contract
  | Non_existing_contract
  | No_delegate
  | Undelagatable_contract
  | Scriptless_contract
  | Too_low_balance

val delete : Storage.t -> Contract_repr.t -> Storage.t tzresult Lwt.t

val exists: Storage.t -> Contract_repr.t -> bool tzresult Lwt.t

val list: Storage.t -> Contract_repr.t list tzresult Lwt.t

val check_counter_increment: Storage.t -> Contract_repr.t -> int32 -> unit tzresult Lwt.t
val increment_counter: Storage.t -> Contract_repr.t -> Storage.t tzresult Lwt.t

val is_delegatable : Storage.t -> Contract_repr.t -> bool tzresult Lwt.t
val is_spendable : Storage.t -> Contract_repr.t -> bool tzresult Lwt.t

val get_manager: Storage.t -> Contract_repr.t -> Ed25519.Public_key_hash.t tzresult Lwt.t
val get_delegate: Storage.t -> Contract_repr.t -> Ed25519.Public_key_hash.t tzresult Lwt.t
val get_delegate_opt: Storage.t -> Contract_repr.t -> Ed25519.Public_key_hash.t option tzresult Lwt.t
val get_balance: Storage.t -> Contract_repr.t -> Tez_repr.t tzresult Lwt.t
val get_assets: Storage.t -> Contract_repr.t -> Asset_repr.Map.t tzresult Lwt.t
val get_counter: Storage.t -> Contract_repr.t -> int32 tzresult Lwt.t

val get_script: Storage.t -> Contract_repr.t -> Script_repr.t tzresult Lwt.t

(** Update_script_storage fails if the contract has not enouth tez to
    store the new data.
    It does not fail if the contract does not exists *)
val update_script_storage: Storage.t -> Contract_repr.t -> Script_repr.expr ->
  Storage.t tzresult Lwt.t

(** fails if the contract is not delegatable *)
val set_delegate : Storage.t -> Contract_repr.t -> Ed25519.Public_key_hash.t option -> Storage.t tzresult Lwt.t

val credit : Storage.t -> Contract_repr.t -> Tez_repr.t -> Storage.t tzresult Lwt.t

(** checks that the contract is spendable and decrease_balance *)
val spend : Storage.t -> Contract_repr.t -> Tez_repr.t -> Storage.t tzresult Lwt.t

(* decrease balance uncondionally *)
val unconditional_spend : Storage.t -> Contract_repr.t -> Tez_repr.t -> Storage.t tzresult Lwt.t

val issue :
  Storage.t -> Contract_repr.t ->  Asset_repr.t -> Ed25519.Public_key_hash.t -> Tez_repr.t -> Storage.t tzresult Lwt.t

val originate :
  Storage.t ->
  Contract_repr.origination_nonce ->
  balance:Tez_repr.t ->
  manager:Ed25519.Public_key_hash.t ->
  script:Script_repr.t ->
  delegate:Ed25519.Public_key_hash.t option ->
  spendable:bool ->
  delegatable:bool ->
  (Storage.t * Contract_repr.t * Contract_repr.origination_nonce) tzresult Lwt.t

val init :
  Storage.t -> Storage.t tzresult Lwt.t

val pp: Format.formatter -> Contract_repr.t -> unit
