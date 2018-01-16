(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type prevalidation_state

val start_prevalidation :
  ?proto_header: MBytes.t ->
  ?max_number_of_operations: int ->
  predecessor: State.Block.t ->
  timestamp: Time.t ->
  unit -> prevalidation_state tzresult Lwt.t

val prevalidate :
  prevalidation_state -> sort:bool ->
  (Operation_hash.t * Operation.t) list ->
  (prevalidation_state * error Preapply_result.t) Lwt.t

val end_prevalidation :
  prevalidation_state -> Updater.validation_result tzresult Lwt.t
