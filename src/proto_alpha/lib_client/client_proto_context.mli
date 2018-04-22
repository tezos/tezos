(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

val list_contract_labels:
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  (string * string * string) list tzresult Lwt.t

val get_storage:
  #Proto_alpha.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Script.expr option tzresult Lwt.t

val get_manager:
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  (string * public_key_hash *
   public_key * Client_keys.sk_uri) tzresult Lwt.t

val get_balance:
  #Proto_alpha.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Tez.t tzresult Lwt.t

val set_delegate:
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  fee:Tez.tez ->
  Contract.t ->
  src_pk:public_key ->
  manager_sk:Client_keys.sk_uri ->
  public_key_hash option ->
  Injection.result tzresult Lwt.t

val register_as_delegate:
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  fee:Tez.tez ->
  manager_sk:Client_keys.sk_uri ->
  public_key ->
  Injection.result tzresult Lwt.t

val source_to_keys:
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  (public_key * Client_keys.sk_uri) tzresult Lwt.t

val originate_account :
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?branch:int ->
  source:Contract.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  manager_pkh:public_key_hash ->
  ?delegatable:bool ->
  ?delegate:public_key_hash ->
  balance:Tez.tez ->
  fee:Tez.tez ->
  unit -> (Injection.result * Contract.t) tzresult Lwt.t

val save_contract :
  force:bool ->
  #Proto_alpha.full ->
  string ->
  Contract.t ->
  unit tzresult Lwt.t

val originate_contract:
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?branch:int ->
  fee:Tez.t ->
  ?gas_limit:Z.t ->
  ?storage_limit:Int64.t ->
  delegate:public_key_hash option ->
  ?delegatable:bool ->
  ?spendable:bool ->
  initial_storage:string ->
  manager:public_key_hash ->
  balance:Tez.t ->
  source:Contract.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  code:Script.expr ->
  unit -> (Injection.result * Contract.t) tzresult Lwt.t

val transfer :
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?branch:int ->
  source:Contract.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  destination:Contract.t ->
  ?arg:string ->
  amount:Tez.t ->
  fee:Tez.t ->
  ?gas_limit:Z.t ->
  ?storage_limit:Int64.t ->
  unit ->
  (Injection.result * Contract.t list) tzresult Lwt.t

val reveal :
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?branch:int ->
  source:Contract.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  fee:Tez.t ->
  unit -> Injection.result tzresult Lwt.t

val dictate :
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  dictator_operation ->
  Client_keys.sk_uri ->
  Injection.result tzresult Lwt.t

type activation_key =
  { pkh : Ed25519.Public_key_hash.t ;
    amount : Tez.t ;
    activation_code : Blinded_public_key_hash.activation_code ;
    mnemonic : string list ;
    password : string ;
    email : string ;
  }

val activation_key_encoding: activation_key Data_encoding.t

val claim_commitment:
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?encrypted:bool ->
  ?force:bool ->
  activation_key ->
  string ->
  Injection.result tzresult Lwt.t

