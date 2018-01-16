(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type 'error t = {
  applied: (Operation_hash.t * Operation.t) list;
  refused: (Operation.t * 'error list) Operation_hash.Map.t;
  (* e.g. invalid signature *)
  branch_refused: (Operation.t * 'error list) Operation_hash.Map.t;
  (* e.g. insufficent balance *)
  branch_delayed: (Operation.t * 'error list) Operation_hash.Map.t;
  (* e.g. timestamp in the future *)
}

val empty : 'error t

val map :
  (Operation.t * 'a list -> Operation.t * 'b list) -> 'a t -> 'b t

val operations :
  'error t -> Operation.t Operation_hash.Map.t

val encoding :
  'error list Data_encoding.t ->
  'error t Data_encoding.t
