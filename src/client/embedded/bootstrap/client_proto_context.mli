(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val transfer:
  Client_proto_rpcs.block ->
  ?force:bool ->
  source:Contract.t ->
  src_pk:public_key ->
  src_sk:secret_key ->
  destination:Contract.t ->
  ?arg:string ->
  amount:Tez.t ->
  fee:Tez.t ->
  unit -> unit tzresult Lwt.t

val originate_account:
  Client_proto_rpcs.block ->
  ?force:bool ->
  source:Contract.t ->
  src_pk:public_key ->
  src_sk:secret_key ->
  manager_pkh:public_key_hash ->
  ?delegatable:bool ->
  ?spendable:bool ->
  ?delegate:public_key_hash ->
  balance:Tez.t ->
  fee:Tez.t ->
  unit -> Contract.t tzresult Lwt.t

val originate_contract:
  Client_proto_rpcs.block ->
  ?force:bool ->
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
  unit -> Contract.t tzresult Lwt.t

val commands: unit -> Cli_entries.command list
