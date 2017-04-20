(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Environment

val get_balance:
  Client_rpcs.config ->
  Client_proto_rpcs.block ->
  Contract.t ->
  Tez.t tzresult Lwt.t

val transfer:
  Client_rpcs.config ->
  Client_proto_rpcs.block ->
  ?force:bool ->
  ?branch:int ->
  source:Contract.t ->
  src_pk:public_key ->
  src_sk:secret_key ->
  destination:Contract.t ->
  ?arg:string ->
  amount:Tez.t ->
  fee:Tez.t ->
  unit -> (Operation_hash.t * Contract.t list) tzresult Lwt.t

val originate_account:
  Client_rpcs.config ->
  Client_proto_rpcs.block ->
  ?force:bool ->
  ?branch:int ->
  source:Contract.t ->
  src_pk:public_key ->
  src_sk:secret_key ->
  manager_pkh:public_key_hash ->
  ?delegatable:bool ->
  ?spendable:bool ->
  ?delegate:public_key_hash ->
  balance:Tez.t ->
  fee:Tez.t ->
  unit -> (Operation_hash.t * Contract.t) tzresult Lwt.t

val originate_contract:
  Client_rpcs.config ->
  Client_proto_rpcs.block ->
  ?force:bool ->
  ?branch:int ->
  source:Contract.t ->
  src_pk:public_key ->
  src_sk:secret_key ->
  manager_pkh:public_key_hash ->
  balance:Tez.t ->
  ?delegatable:bool ->
  ?delegatePubKey:public_key_hash ->
  code:Script.code ->
  init:string ->
  fee:Tez.t ->
  unit -> (Operation_hash.t * Contract.t) tzresult Lwt.t

val delegate_contract:
  Client_rpcs.config ->
  Client_proto_rpcs.block ->
  ?force:bool ->
  ?branch:int ->
  source:Contract.t ->
  ?src_pk:public_key ->
  manager_sk:secret_key ->
  fee:Tez.t ->
  public_key_hash option ->
  Operation_hash.t tzresult Lwt.t

val commands: unit -> Client_commands.command list
