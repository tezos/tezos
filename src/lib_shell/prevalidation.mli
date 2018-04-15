(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type prevalidation_state

val start_prevalidation :
  ?protocol_data: MBytes.t ->
  predecessor: State.Block.t ->
  timestamp: Time.t ->
  unit -> prevalidation_state tzresult Lwt.t

val prevalidate :
  prevalidation_state -> sort:bool ->
  (Operation_hash.t * Operation.t) list ->
  (prevalidation_state * error Preapply_result.t) Lwt.t

val end_prevalidation :
  prevalidation_state ->
  Tezos_protocol_environment_shell.validation_result tzresult Lwt.t

val preapply :
  predecessor:State.Block.t ->
  timestamp:Time.t ->
  protocol_data:MBytes.t ->
  sort_operations:bool ->
  Operation.t list list ->
  (Block_header.shell_header * error Preapply_result.t list) tzresult Lwt.t

