(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_context

type error += Quota_exceeded
type error += Overflow of Script.location
type error += Reject of Script.location
type error += Division_by_zero of Script.location

(* calling convention :
   ((amount, arg), globals)) -> (ret, globals) *)

val execute: Contract.t -> Contract.t -> Tezos_context.t ->
  Script.storage -> Script.code -> Tez.t ->
  Script.expr -> int ->
  (Script.expr * Script.expr * int * context) tzresult Lwt.t

val trace: Contract.t -> Contract.t -> Tezos_context.t ->
  Script.storage -> Script.code -> Tez.t ->
  Script.expr -> int ->
  ((Script.expr * Script.expr * int * context) *
   (Script.location * int * Script.expr list) list) tzresult Lwt.t
