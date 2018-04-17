(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

(** Functions building operations *)

val sourced : sourced_operations -> proto_operation

val manager :
  Helpers_account.t -> ?fee:Tez.tez -> manager_operation list ->
  Alpha_environment.Context.t -> sourced_operations proto_tzresult Lwt.t

val manager_full :
  Helpers_account.t -> ?fee:Tez.tez -> manager_operation list ->
  Alpha_environment.Context.t -> proto_operation proto_tzresult Lwt.t

val transaction :
  ?parameters:Script.expr -> Tez.t -> Contract.contract ->
  manager_operation

val origination :
  ?delegatable:bool -> ?script:Script.t option -> ?spendable:bool ->
  ?delegate:public_key_hash option -> Helpers_account.t -> Tez.t -> manager_operation

val delegation : public_key_hash -> manager_operation

val delegation_full :
  ?fee:Tez.tez -> Helpers_account.t -> public_key_hash -> Alpha_environment.Context.t ->
  proto_operation proto_tzresult Lwt.t

val script_origination_full :
  Script.t option -> Helpers_account.t -> Tez.t -> Alpha_environment.Context.t ->
  proto_operation proto_tzresult  Lwt.t

val origination_full :
  ?spendable:bool -> ?delegatable:bool -> ?fee:Tez.tez ->
  Helpers_account.t -> Tez.t -> Alpha_environment.Context.t ->
  proto_operation proto_tzresult Lwt.t

val transaction_full :
  ?fee:Tez.tez -> ?parameters:Proto_alpha.Alpha_context.Script.expr -> Helpers_account.t -> Contract.contract -> Tez.t ->
  Alpha_environment.Context.t -> proto_operation proto_tzresult Lwt.t

val amendment_operation :
  Helpers_account.t -> amendment_operation -> sourced_operations

val endorsements :
  ?slot:int -> Block_hash.t -> Raw_level.t -> consensus_operation

val endorsement_full :
  ?slot:int -> Block_hash.t -> Raw_level.t -> proto_operation

val sign :
  Helpers_account.t option -> Tezos_base.Operation.shell_header ->
  proto_operation -> MBytes.t * Signature.t option

val main_of_proto :
  Helpers_account.t -> Tezos_base.Operation.shell_header ->
  proto_operation -> (Main.operation * Operation_hash.t) proto_tzresult

val apply_of_proto :
  Helpers_account.t option -> Tezos_base.Operation.shell_header ->
  proto_operation -> operation
