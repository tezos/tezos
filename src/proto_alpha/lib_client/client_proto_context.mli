(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Tezos_context
open Environment

val list_contract_labels :
  Client_commands.full_context ->
  Client_proto_rpcs.block ->
  (string * string * string) list tzresult Lwt.t

val get_storage :
  #RPC_context.simple ->
  Client_proto_rpcs.block ->
  Contract.t ->
  Script.expr option tzresult Lwt.t

val get_manager :
  Client_commands.full_context ->
  Client_proto_rpcs.block ->
  Contract.t ->
  (string * public_key_hash *
   public_key * Client_keys.sk_locator) tzresult Lwt.t

val get_balance:
  #RPC_context.simple ->
  Client_proto_rpcs.block ->
  Contract.t ->
  Tez.t tzresult Lwt.t

val set_delegate :
  #RPC_context.simple ->
  Client_proto_rpcs.block ->
  fee:Tez.tez ->
  Contract.t ->
  src_pk:public_key ->
  manager_sk:Client_keys.sk_locator ->
  public_key_hash option ->
  Operation_list_hash.elt tzresult Lwt.t

val operation_submitted_message :
  #Client_commands.logger ->
  Operation_hash.t ->
  unit tzresult Lwt.t

val source_to_keys:
  Client_commands.full_context ->
  Client_proto_rpcs.block ->
  Contract.t ->
  (public_key * Client_keys.sk_locator) tzresult Lwt.t

val originate_account :
  ?branch:int ->
  source:Contract.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_locator ->
  manager_pkh:public_key_hash ->
  ?delegatable:bool ->
  ?delegate:public_key_hash ->
  balance:Tez.tez ->
  fee:Tez.tez ->
  Block_services.block ->
  #RPC_context.simple ->
  unit -> (Operation_list_hash.elt * Contract.t) tzresult Lwt.t

val save_contract :
  force:bool ->
  Client_commands.full_context ->
  string ->
  Contract.t ->
  unit tzresult Lwt.t

val operation_submitted_message :
  #Client_commands.logger ->
  ?contracts:Contract.t list ->
  Operation_hash.t ->
  unit tzresult Lwt.t

val originate_contract:
  fee:Tez.t ->
  delegate:public_key_hash option ->
  ?delegatable:bool ->
  ?spendable:bool ->
  initial_storage:string ->
  manager:public_key_hash ->
  balance:Tez.t ->
  source:Contract.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_locator ->
  code:Script.expr ->
  Client_commands.full_context ->
  (Operation_hash.t * Contract.t) tzresult Lwt.t

val faucet :
  ?branch:int ->
  manager_pkh:public_key_hash ->
  Block_services.block ->
  #RPC_context.simple ->
  unit -> (Operation_list_hash.elt * Contract.t) tzresult Lwt.t

val transfer :
  #RPC_context.simple ->
  Client_proto_rpcs.block ->
  ?branch:int ->
  source:Contract.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_locator ->
  destination:Contract.t ->
  ?arg:string ->
  amount:Tez.t ->
  fee:Tez.t ->
  unit ->
  (Operation_hash.t * Contract.t list) tzresult Lwt.t

val dictate :
  #RPC_context.simple ->
  Client_proto_rpcs.block ->
  dictator_operation ->
  secret_key ->
  Operation_hash.t tzresult Lwt.t
