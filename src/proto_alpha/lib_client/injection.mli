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
open Apply_operation_result

type 'kind preapply_result =
  Operation_hash.t * 'kind operation * 'kind operation_metadata

val preapply:
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?branch:int ->
  ?src_sk:Client_keys.sk_uri ->
  'kind contents_list ->
  'kind preapply_result tzresult Lwt.t

type 'kind result_list =
  Operation_hash.t * 'kind contents_list * 'kind contents_result_list

val inject_operation:
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?confirmations:int ->
  ?dry_run:bool ->
  ?branch:int ->
  ?src_sk:Client_keys.sk_uri ->
  'kind contents_list ->
  'kind result_list tzresult Lwt.t

type 'kind result =
  Operation_hash.t * 'kind contents * 'kind contents_result

val inject_manager_operation:
  #Proto_alpha.full ->
  chain:Shell_services.chain ->
  block:Shell_services.block ->
  ?branch:int ->
  ?confirmations:int ->
  ?dry_run:bool ->
  source:Contract.t ->
  src_pk:Signature.public_key ->
  src_sk:Client_keys.sk_uri ->
  fee:Tez.t ->
  ?gas_limit:Z.t ->
  ?storage_limit:Z.t ->
  'kind manager_operation ->
  'kind Kind.manager result tzresult Lwt.t

val originated_contracts:
  'kind contents_result_list -> Contract.t list tzresult
