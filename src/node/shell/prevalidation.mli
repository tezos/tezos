(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type 'error preapply_result = {
  applied: (Operation_hash.t * Operation.t) list;
  refused: (Operation.t * 'error list) Operation_hash.Map.t;
  (* e.g. invalid signature *)
  branch_refused: (Operation.t * 'error list) Operation_hash.Map.t;
  (* e.g. insufficent balance *)
  branch_delayed: (Operation.t * 'error list) Operation_hash.Map.t;
  (* e.g. timestamp in the future *)
}

val empty_result : 'error preapply_result

val preapply_result_operations :
  'error preapply_result -> Operation.t Operation_hash.Map.t

val preapply_result_encoding :
  'error list Data_encoding.t ->
  'error preapply_result Data_encoding.t

type prevalidation_state

val start_prevalidation :
  ?proto_header: MBytes.t ->
  predecessor: State.Block.t ->
  timestamp: Time.t ->
  unit -> prevalidation_state tzresult Lwt.t

val prevalidate :
  prevalidation_state -> sort:bool ->
  (Operation_hash.t * Operation.t) list ->
  (prevalidation_state * error preapply_result) Lwt.t

val end_prevalidation :
  prevalidation_state -> Updater.validation_result tzresult Lwt.t
