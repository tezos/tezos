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

type result = Operation_hash.t * operation * operation_result

val preapply:
  #Proto_alpha.full ->
  Block_services.block ->
  ?branch:int ->
  ?src_sk:Client_keys.sk_uri ->
  proto_operation ->
  result tzresult Lwt.t

val inject_operation:
  #Proto_alpha.full ->
  Block_services.block ->
  ?confirmations:int ->
  ?branch:int ->
  ?src_sk:Client_keys.sk_uri ->
  proto_operation ->
  result tzresult Lwt.t

val originated_contracts: operation_result -> Contract.t list tzresult
