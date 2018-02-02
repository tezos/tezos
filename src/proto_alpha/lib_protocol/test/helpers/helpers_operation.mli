(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha.Tezos_context

(** Functions building operations *)

val sourced : sourced_operations -> proto_operation

val manager :
  Helpers_account.t -> ?fee:Tez.tez -> manager_operation list ->
  Proto_alpha.Environment.Context.t -> sourced_operations Proto_alpha.tzresult Lwt.t

val manager_full :
  Helpers_account.t -> ?fee:Tez.tez -> manager_operation list ->
  Proto_alpha.Environment.Context.t -> proto_operation Proto_alpha.tzresult Lwt.t

val transaction :
  ?parameters:Script.expr option -> Tez.t -> Contract.contract ->
  manager_operation

val origination :
  ?delegatable:bool -> ?script:Script.t option -> ?spendable:bool ->
  ?delegate:public_key_hash option -> Helpers_account.t -> Tez.t -> manager_operation

val delegation : public_key_hash -> manager_operation

val delegation_full :
  ?fee:Tez.tez -> Helpers_account.t -> public_key_hash -> Proto_alpha.Environment.Context.t ->
  proto_operation Proto_alpha.tzresult Lwt.t

val script_origination_full :
  Script.t option -> Helpers_account.t -> Tez.t -> Proto_alpha.Environment.Context.t ->
  proto_operation Proto_alpha.tzresult  Lwt.t

val origination_full :
  ?spendable:bool -> ?delegatable:bool -> ?fee:Tez.tez ->
  Helpers_account.t -> Tez.t -> Proto_alpha.Environment.Context.t ->
  proto_operation Proto_alpha.tzresult Lwt.t

val transaction_full :
  ?fee:Tez.tez -> Helpers_account.t -> Contract.contract -> Tez.t ->
  Proto_alpha.Environment.Context.t -> proto_operation Proto_alpha.tzresult Lwt.t

val delegate :
  Helpers_account.t -> delegate_operation list -> sourced_operations

val endorsement :
  ?slot:int -> Block_hash.t -> delegate_operation

val endorsement_full :
  ?slot:int -> Helpers_account.t -> Block_hash.t -> proto_operation

val sign :
  Helpers_account.t option -> Tezos_base.Operation.shell_header ->
  proto_operation -> MBytes.t * Ed25519.Signature.t option

val main_of_proto :
  Helpers_account.t -> Tezos_base.Operation.shell_header ->
  proto_operation -> (Main.operation * Tezos_base.Operation_hash.t) Proto_alpha.tzresult

val apply_of_proto :
  Helpers_account.t option -> Tezos_base.Operation.shell_header ->
  proto_operation -> operation

