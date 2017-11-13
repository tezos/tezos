(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Hash
open Error_monad
include module type of Kaputt.Assertion

val format_msg : string option -> string option

val is_ok : ?msg:string -> 'a tzresult -> unit
val is_error : ?msg:string -> 'a tzresult -> unit
val contain_error : ?msg:string -> f:(error -> bool) -> 'a tzresult -> unit

val fail_msg : ('a, Format.formatter, unit, 'b) format4 -> 'a

val fail : string -> string -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val equal_block_hash_list :
  ?msg:string -> Block_hash.t list -> Block_hash.t list -> unit

val equal_string_list :
  ?msg:string -> string list -> string list -> unit

val equal_string_list_list :
  ?msg:string -> string list list -> string list list -> unit

val equal_string_option : ?msg:string -> string option -> string option -> unit

val equal_error_monad :
  ?msg:string -> Error_monad.error -> Error_monad.error -> unit

val equal_block_set :
  ?msg:string -> Block_hash.Set.t -> Block_hash.Set.t -> unit

val equal_block_map :
  ?msg:string -> eq:('a -> 'a -> bool) ->
  'a Block_hash.Map.t -> 'a Block_hash.Map.t -> unit

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

val equal_float:
  ?eq:(float -> float -> bool) ->
  ?prn:(float -> string) -> ?msg:string -> float -> float -> unit
