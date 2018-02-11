(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context

type error += Overflow of Script.location
type error += Reject of Script.location
type error += Runtime_contract_error : Contract.t * Script.expr -> error

val dummy_code_fee : Tez.t
val dummy_storage_fee : Tez.t

val execute:
  Contract.origination_nonce ->
  Contract.t -> Contract.t -> Alpha_context.t ->
  Script.t -> Tez.t ->
  Script.expr -> Gas.t ->
  (Script.expr * Script.expr * Gas.t * context * Contract.origination_nonce *
   Script_typed_ir.ex_big_map option) tzresult Lwt.t

val trace:
  Contract.origination_nonce ->
  Contract.t -> Contract.t -> Alpha_context.t ->
  Script.t -> Tez.t ->
  Script.expr -> Gas.t ->
  ((Script.expr * Script.expr * Gas.t * context * Contract.origination_nonce * Script_typed_ir.ex_big_map option) *
   (Script.location * Gas.t * Script.expr list) list) tzresult Lwt.t
