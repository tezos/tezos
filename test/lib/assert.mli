(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Hash
include (module type of struct include Kaputt.Assertion end)

val fail_msg : ('a, Format.formatter, unit, 'b) format4 -> 'a

val fail : string -> string -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val equal_persist_list :
  ?msg:string -> Persist.key list -> Persist.key list -> unit

val equal_block_hash_list :
  ?msg:string -> Block_hash.t list -> Block_hash.t list -> unit

val equal_string_list :
  ?msg:string -> string list -> string list -> unit

val equal_string_option : ?msg:string -> string option -> string option -> unit

val equal_error_monad :
  ?msg:string -> Error_monad.error -> Error_monad.error -> unit

val equal_block_map : ?msg:string -> eq:('a -> 'a -> bool) -> 'a -> 'a -> unit

val equal_operation :
  ?msg:string ->
  (Operation_hash.t * State.Operation.operation) option ->
  (Operation_hash.t * State.Operation.operation) option ->
  unit

val equal_block :
  ?msg:string ->
  (Block_hash.t * Store.block) option ->
  (Block_hash.t * Store.block) option ->
  unit

val equal_result :
  ?msg:string ->
  ('a, 'b) result ->
  ('a, 'b) result ->
  equal_ok:(?msg:string -> 'a -> 'a -> 'c) ->
  equal_err:(?msg:string -> 'b -> 'b -> 'c) -> 'c

val equal_exn : ?msg:string -> exn -> exn -> unit

val test_fail :
  ?msg:string ->
  ?prn:('a -> string) ->
  (unit -> 'a) ->
  (exn -> bool) ->
  unit
