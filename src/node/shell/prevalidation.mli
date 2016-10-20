(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type 'error preapply_result = {
  applied: Operation_hash.t list;
  refused: 'error list Operation_hash.Map.t;
  (* e.g. invalid signature *)
  branch_refused: 'error list Operation_hash.Map.t;
  (* e.g. insufficent balance *)
  branch_delayed: 'error list Operation_hash.Map.t;
  (* e.g. timestamp in the future *)
}

val empty_result : 'error preapply_result

val preapply_result_operations :
  'error preapply_result -> Operation_hash.Set.t

val preapply_result_encoding :
  'error list Data_encoding.t ->
  'error preapply_result Data_encoding.t

type prevalidation_state

val start_prevalidation :
  predecessor: State.Valid_block.t ->
  timestamp: Time.t ->
  prevalidation_state tzresult Lwt.t

val prevalidate :
  prevalidation_state -> sort:bool ->
  (Operation_hash.t * Store.Operation.t) list ->
  (prevalidation_state * error preapply_result) tzresult Lwt.t

val end_prevalidation :
  prevalidation_state -> Context.t tzresult Lwt.t
