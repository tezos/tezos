(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_hash

type t = private
  | Default of Ed25519.public_key_hash
  | Hash of Contract_hash.t
type contract = t

type descr = {
  manager: Ed25519.public_key_hash ;
  delegate: Ed25519.public_key_hash option ;
  spendable: bool ;
  delegatable: bool ;
  script: Script_repr.t ;
}

include Compare.S with type t := contract

val default_contract : Ed25519.public_key_hash -> contract

val is_default : contract -> Ed25519.public_key_hash option

val generic_contract :
  manager:Ed25519.public_key_hash ->
  delegate:Ed25519.public_key_hash option ->
  spendable:bool ->
  delegatable:bool ->
  script:Script_repr.t ->
  contract

(** {2 Human readable notation} ***********************************************)

type error += Invalid_contract_notation of string

val to_b48check: contract -> string

val of_b48check: string -> contract tzresult

(** {2 Serializers} ***********************************************************)

val encoding : contract Data_encoding.t
val descr_encoding : descr Data_encoding.t

val arg : contract RPC.Arg.arg
