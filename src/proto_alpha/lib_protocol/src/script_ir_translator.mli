(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_context

type ('ta, 'tb) eq = Eq : ('same, 'same) eq

type ex_comparable_ty = Ex_comparable_ty : 'a Script_typed_ir.comparable_ty -> ex_comparable_ty
type ex_ty = Ex_ty : 'a Script_typed_ir.ty -> ex_ty
type ex_stack_ty = Ex_stack_ty : 'a Script_typed_ir.stack_ty -> ex_stack_ty
type ex_script = Ex_script : ('a, 'b, 'c) Script_typed_ir.script -> ex_script

(* ---- Error definitions ---------------------------------------------------*)

(* Auxiliary types for error documentation *)
type namespace = Type_namespace | Constant_namespace | Instr_namespace | Keyword_namespace
type kind = Int_kind | String_kind | Prim_kind | Seq_kind
type type_map = (int * (Script.expr list * Script.expr list)) list

(* Structure errors *)
type error += Invalid_arity of Script.location * Script.prim * int * int
type error += Invalid_namespace of Script.location * Script.prim * namespace * namespace
type error += Invalid_primitive of Script.location * Script.prim list * Script.prim
type error += Invalid_kind of Script.location * kind list * kind
type error += Missing_field of Script.prim
type error += Type_too_large : Script.location * int * int -> error
type error += Duplicate_field of Script.location * Script.prim

(* Instruction typing errors *)
type error += Fail_not_in_tail_position of Script.location
type error += Undefined_binop : Script.location * Script.prim * _ Script_typed_ir.ty * _ Script_typed_ir.ty -> error
type error += Undefined_unop : Script.location * Script.prim * _ Script_typed_ir.ty -> error
type error += Bad_return : Script.location * _ Script_typed_ir.stack_ty * _ Script_typed_ir.ty -> error
type error += Bad_stack : Script.location * Script.prim * int * _ Script_typed_ir.stack_ty -> error
type error += Unmatched_branches : Script.location * _ Script_typed_ir.stack_ty * _ Script_typed_ir.stack_ty -> error

type error += Inconsistent_annotations of string * string
type error += Inconsistent_type_annotations :
                Script.location * _ Script_typed_ir.ty * _ Script_typed_ir.ty -> error
type error += Unexpected_annotation of Script.location

type error += Transfer_in_lambda of Script.location
type error += Transfer_in_dip of Script.location
type error += Self_in_lambda of Script.location
type error += Bad_stack_length
type error += Bad_stack_item of int
type error += Invalid_map_body : Script.location * _ Script_typed_ir.stack_ty -> error
type error += Invalid_map_block_fail of Script.location
type error += Invalid_iter_body : Script.location * _ Script_typed_ir.stack_ty * _ Script_typed_ir.stack_ty -> error

(* Value typing errors *)
type error += Invalid_constant : Script.location * Script.expr * _ Script_typed_ir.ty -> error
type error += Invalid_contract of Script.location * Contract.t
type error += Comparable_type_expected : Script.location * _ Script_typed_ir.ty -> error
type error += Inconsistent_types : _ Script_typed_ir.ty * _ Script_typed_ir.ty -> error
type error += Unordered_map_keys of Script.location * Script.expr
type error += Unordered_set_values of Script.location * Script.expr
type error += Duplicate_map_keys of Script.location * Script.expr
type error += Duplicate_set_values of Script.location * Script.expr

(* Toplevel errors *)
type error += Ill_typed_data : string option * Script.expr * _ Script_typed_ir.ty -> error
type error += Ill_formed_type of string option * Script.expr * Script.location
type error += Ill_typed_contract : Script.expr * type_map -> error

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

val ty_eq :
  'ta Script_typed_ir.ty -> 'tb Script_typed_ir.ty ->
  ('ta Script_typed_ir.ty, 'tb Script_typed_ir.ty) eq tzresult

val parse_data :
  ?type_logger: (int -> Script.expr list -> Script.expr list -> unit) ->
  context -> 'a Script_typed_ir.ty -> Script.node -> 'a tzresult Lwt.t
val unparse_data :
  'a Script_typed_ir.ty -> 'a -> Script.node

val parse_ty :
  Script.node -> (ex_ty * Script_typed_ir.annot) tzresult
val unparse_ty :
  string option -> 'a Script_typed_ir.ty -> Script.node

val type_map_enc : type_map Data_encoding.encoding
val ex_ty_enc : ex_ty Data_encoding.encoding

val parse_toplevel
  : Script.expr -> (Script.node * Script.node * Script.node * Script.node) tzresult

val typecheck_code :
  context -> Script.expr -> type_map tzresult Lwt.t

val typecheck_data :
  ?type_logger: (int -> Script.expr list -> Script.expr list -> unit) ->
  context -> Script.expr * Script.expr -> unit tzresult Lwt.t

val parse_script :
  ?type_logger: (int -> Script.expr list -> Script.expr list -> unit) ->
  context -> Script.t -> ex_script tzresult Lwt.t

val hash_data : 'a Script_typed_ir.ty -> 'a -> string
