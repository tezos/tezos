(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_context

type error += Overflow of Script.location
type error += Reject of Script.location
type error += Runtime_contract_error : Contract.t * Script.expr -> error

val dummy_code_fee : Tez.t
val dummy_storage_fee : Tez.t

val execute:
  Contract.origination_nonce ->
  Contract.t -> Contract.t -> Tezos_context.t ->
  Script.t -> Tez.t ->
  Script.expr -> Gas.t ->
  (Script.expr * Script.expr * Gas.t * context * Contract.origination_nonce) tzresult Lwt.t

val trace:
  Contract.origination_nonce ->
  Contract.t -> Contract.t -> Tezos_context.t ->
  Script.t -> Tez.t ->
  Script.expr -> Gas.t ->
  ((Script.expr * Script.expr * Gas.t * context * Contract.origination_nonce) *
   (Script.location * Gas.t * Script.expr list) list) tzresult Lwt.t
