(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val wait_for_operation_inclusion:
  #Client_context.full ->
  chain:Chain_services.chain ->
  ?predecessors:int ->
  ?confirmations:int ->
  Operation_hash.t ->
  unit tzresult Lwt.t
