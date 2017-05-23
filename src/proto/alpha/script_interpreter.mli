(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_context
open Script_typed_ir

type error += Quota_exceeded
type error += Overflow of Script.location
type error += Reject of Script.location
type error += Runtime_contract_error : Contract.t * Script.expr * _ ty * _ ty * _ ty -> error

val dummy_code_fee : Tez.t
val dummy_storage_fee : Tez.t

val execute:
  Contract.origination_nonce ->
  Contract.t -> Contract.t -> Tezos_context.t ->
  Script.storage -> Script.code -> Tez.t ->
  Script.expr -> int ->
  (Script.expr * Script.expr * int * context * Contract.origination_nonce) tzresult Lwt.t

val trace:
  Contract.origination_nonce ->
  Contract.t -> Contract.t -> Tezos_context.t ->
  Script.storage -> Script.code -> Tez.t ->
  Script.expr -> int ->
  ((Script.expr * Script.expr * int * context * Contract.origination_nonce) *
   (Script.location * int * Script.expr list) list) tzresult Lwt.t
