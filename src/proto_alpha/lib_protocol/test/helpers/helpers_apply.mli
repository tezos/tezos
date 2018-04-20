(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Functions to build and apply operations *)

open Proto_alpha
open Alpha_context

val operation :
  tc:context -> ?src:Helpers_account.t ->
  Block_hash.t -> Tezos_base.Operation.shell_header -> Operation.contents ->
  (Contract.contract list * context) proto_tzresult Lwt.t

val transaction :
  tc:context -> ?fee:int -> Block_hash.t ->
  Tezos_base.Operation.shell_header -> Helpers_account.t -> Helpers_account.t -> int ->
  (Contract.contract list * context) proto_tzresult Lwt.t

val transaction_pred :
  ?tc:t -> pred:Helpers_block.result ->
  Helpers_account.t * Helpers_account.t * int * int option ->
  (Contract.contract list * context) proto_tzresult Lwt.t

val script_origination :
  tc:context -> Block_hash.t -> Tezos_base.Operation.shell_header ->
  Script.t option -> Helpers_account.t -> int ->
  (Contract.contract list * context) proto_tzresult Lwt.t

val origination :
  tc:context -> ?spendable:bool -> ?fee:int ->
  ?delegatable:bool -> Block_hash.t -> Tezos_base.Operation.shell_header ->
  Helpers_account.t -> int ->
  (Contract.contract list * context) proto_tzresult Lwt.t

val script_origination_pred :
  ?tc:t -> pred:Helpers_block.result -> Script.t * Helpers_account.t * int ->
  (Contract.contract list * context) proto_tzresult Lwt.t

val origination_pred :
  ?tc:t -> pred:Helpers_block.result ->
  Helpers_account.t * int * bool * bool * int ->
  (Contract.contract list * context) proto_tzresult Lwt.t

val delegation :
  tc:context -> ?fee:int -> Block_hash.t ->
  Tezos_base.Operation.shell_header -> Helpers_account.t -> public_key_hash ->
  (Contract.contract list * context) proto_tzresult Lwt.t

val delegation_pred :
  ?tc:t -> pred:Helpers_block.result ->
  Helpers_account.t * public_key_hash * int ->
  (Contract.contract list * context) proto_tzresult Lwt.t
