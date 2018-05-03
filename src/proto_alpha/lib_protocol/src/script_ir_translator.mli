(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context
open Script_tc_errors

type ('ta, 'tb) eq = Eq : ('same, 'same) eq

type ex_comparable_ty = Ex_comparable_ty : 'a Script_typed_ir.comparable_ty -> ex_comparable_ty
type ex_ty = Ex_ty : 'a Script_typed_ir.ty -> ex_ty
type ex_stack_ty = Ex_stack_ty : 'a Script_typed_ir.stack_ty -> ex_stack_ty
type ex_script = Ex_script : ('a, 'b) Script_typed_ir.script -> ex_script

(* ---- Sets and Maps -------------------------------------------------------*)

val empty_set : 'a Script_typed_ir.comparable_ty -> 'a Script_typed_ir.set
val set_fold :
  ('elt -> 'acc -> 'acc) ->
  'elt Script_typed_ir.set -> 'acc -> 'acc
val set_update : 'a -> bool -> 'a Script_typed_ir.set -> 'a Script_typed_ir.set
val set_mem : 'elt -> 'elt Script_typed_ir.set -> bool
val set_size : 'elt Script_typed_ir.set -> Script_int.n Script_int.num

val empty_map : 'a Script_typed_ir.comparable_ty -> ('a, 'b) Script_typed_ir.map
val map_fold :
  ('key -> 'value -> 'acc -> 'acc) ->
  ('key, 'value) Script_typed_ir.map -> 'acc -> 'acc
val map_update :
  'a -> 'b option -> ('a, 'b) Script_typed_ir.map -> ('a, 'b) Script_typed_ir.map
val map_mem : 'key -> ('key, 'value) Script_typed_ir.map -> bool
val map_get : 'key -> ('key, 'value) Script_typed_ir.map -> 'value option
val map_key_ty : ('a, 'b) Script_typed_ir.map -> 'a Script_typed_ir.comparable_ty
val map_size : ('a, 'b) Script_typed_ir.map -> Script_int.n Script_int.num

val big_map_mem :
  context -> Contract.t -> 'key ->
  ('key, 'value) Script_typed_ir.big_map ->
  (bool * context) tzresult Lwt.t
val big_map_get :
  context ->
  Contract.t -> 'key ->
  ('key, 'value) Script_typed_ir.big_map ->
  ('value option * context) tzresult Lwt.t
val big_map_update :
  'key -> 'value option -> ('key, 'value) Script_typed_ir.big_map ->
  ('key, 'value) Script_typed_ir.big_map

val ty_eq :
  'ta Script_typed_ir.ty -> 'tb Script_typed_ir.ty ->
  ('ta Script_typed_ir.ty, 'tb Script_typed_ir.ty) eq tzresult

val parse_data :
  ?type_logger: (int -> Script.expr list -> Script.expr list -> unit) ->
  context ->
  'a Script_typed_ir.ty -> Script.node -> ('a * context) tzresult Lwt.t
val unparse_data :
  context -> 'a Script_typed_ir.ty -> 'a -> (Script.node * context) tzresult

val parse_ty :
  allow_big_map: bool ->
  allow_operation: bool ->
  Script.node ->
  (ex_ty * Script_typed_ir.annot) tzresult
val unparse_ty :
  string option -> 'a Script_typed_ir.ty -> Script.node

val parse_toplevel
  : Script.expr -> (Script.node * Script.node * Script.node) tzresult

val typecheck_code :
  context -> Script.expr -> (type_map * context) tzresult Lwt.t

val typecheck_data :
  ?type_logger: (int -> Script.expr list -> Script.expr list -> unit) ->
  context -> Script.expr * Script.expr -> context tzresult Lwt.t

val parse_script :
  ?type_logger: (int -> Script.expr list -> Script.expr list -> unit) ->
  context -> Script.t -> (ex_script * context) tzresult Lwt.t

val parse_contract :
  context -> Script.location -> 'a Script_typed_ir.ty -> Contract.t ->
  (context * 'a Script_typed_ir.typed_contract) tzresult Lwt.t

val hash_data : context -> 'a Script_typed_ir.ty -> 'a -> (string * context) tzresult

val extract_big_map : 'a Script_typed_ir.ty -> 'a -> Script_typed_ir.ex_big_map option

val to_serializable_big_map :
  context -> Script_typed_ir.ex_big_map ->
  (Contract.big_map_diff * context) tzresult Lwt.t

val to_printable_big_map :
  context -> Script_typed_ir.ex_big_map ->
  (Script.expr * Script.expr option) list

val erase_big_map_initialization :
  context -> Script.t ->
  (Script.t * Contract.big_map_diff option * context) tzresult Lwt.t
