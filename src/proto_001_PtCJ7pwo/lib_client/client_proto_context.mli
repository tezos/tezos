(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Proto_001_PtCJ7pwo
open Alpha_context

val list_contract_labels:
  #Proto_001_PtCJ7pwo.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  (string * string * string) list tzresult Lwt.t

val get_storage:
  #Proto_001_PtCJ7pwo.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Script.expr option tzresult Lwt.t

val get_script:
  #Proto_001_PtCJ7pwo.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Script.t option tzresult Lwt.t

val get_manager:
  #Proto_001_PtCJ7pwo.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  (string * public_key_hash *
   public_key * Client_keys.sk_uri) tzresult Lwt.t

val get_balance:
  #Proto_001_PtCJ7pwo.rpc_context ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  Tez.t tzresult Lwt.t

val set_delegate:
  #Proto_001_PtCJ7pwo.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  fee:Tez.tez ->
  Contract.t ->
  src_pk:public_key ->
  manager_sk:Client_keys.sk_uri ->
  public_key_hash option ->
  Kind.delegation Kind.manager Injection.result tzresult Lwt.t

val register_as_delegate:
  #Proto_001_PtCJ7pwo.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  fee:Tez.tez ->
  manager_sk:Client_keys.sk_uri ->
  public_key ->
  Kind.delegation Kind.manager Injection.result tzresult Lwt.t

val source_to_keys:
  #Proto_001_PtCJ7pwo.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  Contract.t ->
  (public_key * Client_keys.sk_uri) tzresult Lwt.t

val originate_account :
  #Proto_001_PtCJ7pwo.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?branch:int ->
  source:Contract.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  manager_pkh:public_key_hash ->
  ?delegatable:bool ->
  ?delegate:public_key_hash ->
  balance:Tez.tez ->
  fee:Tez.tez ->
  unit -> (Kind.origination Kind.manager Injection.result * Contract.t) tzresult Lwt.t

val save_contract :
  force:bool ->
  #Proto_001_PtCJ7pwo.full ->
  string ->
  Contract.t ->
  unit tzresult Lwt.t

val originate_contract:
  #Proto_001_PtCJ7pwo.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?branch:int ->
  fee:Tez.t ->
  ?gas_limit:Z.t ->
  ?storage_limit:Z.t ->
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
  unit -> (Kind.origination Kind.manager Injection.result * Contract.t) tzresult Lwt.t

val transfer :
  #Proto_001_PtCJ7pwo.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?branch:int ->
  source:Contract.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  destination:Contract.t ->
  ?arg:string ->
  amount:Tez.t ->
  fee:Tez.t ->
  ?gas_limit:Z.t ->
  ?storage_limit:Z.t ->
  unit ->
  (Kind.transaction Kind.manager Injection.result * Contract.t list) tzresult Lwt.t

val reveal :
  #Proto_001_PtCJ7pwo.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?branch:int ->
  source:Contract.t ->
  src_pk:public_key ->
  src_sk:Client_keys.sk_uri ->
  fee:Tez.t ->
  unit -> Kind.reveal Kind.manager Injection.result tzresult Lwt.t

type activation_key =
  { pkh : Ed25519.Public_key_hash.t ;
    amount : Tez.t ;
    activation_code : Blinded_public_key_hash.activation_code ;
    mnemonic : string list ;
    password : string ;
    email : string ;
  }

val activation_key_encoding: activation_key Data_encoding.t

val activate_account:
  #Proto_001_PtCJ7pwo.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?encrypted:bool ->
  ?force:bool ->
  activation_key ->
  string ->
  Kind.activate_account Injection.result tzresult Lwt.t

val activate_existing_account:
  #Proto_001_PtCJ7pwo.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  string ->
  Blinded_public_key_hash.activation_code ->
  Kind.activate_account Injection.result tzresult Lwt.t
