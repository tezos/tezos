(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context
open Script
open Script_typed_ir


(* ---- Error definitions ---------------------------------------------------*)

(* Auxiliary types for error documentation *)
type namespace = Type_namespace | Constant_namespace | Instr_namespace | Keyword_namespace
type kind = Int_kind | String_kind | Prim_kind | Seq_kind
type type_map = (int * ((Script.expr * Script.annot) list * (Script.expr * Script.annot) list)) list

(* Structure errors *)
type error += Invalid_arity of Script.location * prim * int * int
type error += Invalid_namespace of Script.location * prim * namespace * namespace
type error += Invalid_primitive of Script.location * prim list * prim
type error += Invalid_kind of Script.location * kind list * kind
type error += Missing_field of prim
type error += Duplicate_field of Script.location * prim
type error += Unexpected_big_map of Script.location
type error += Unexpected_operation of Script.location

(* Instruction typing errors *)
type error += Fail_not_in_tail_position of Script.location
type error += Undefined_binop : Script.location * prim * _ ty * _ ty -> error
type error += Undefined_unop : Script.location * prim * _ ty -> error
type error += Bad_return : Script.location * _ stack_ty * _ ty -> error
type error += Bad_stack : Script.location * prim * int * _ stack_ty -> error
type error += Unmatched_branches : Script.location * _ stack_ty * _ stack_ty -> error
type error += Self_in_lambda of Script.location
type error += Bad_stack_length
type error += Bad_stack_item of int
type error += Inconsistent_annotations of string * string
type error += Inconsistent_type_annotations : Script.location * _ ty * _ ty -> error
type error += Invalid_type_annotation : Script.location * annot list -> error
type error += Invalid_var_annotation : Script.location * annot list -> error
type error += Unexpected_annotation of Script.location
type error += Invalid_map_body : Script.location * _ stack_ty -> error
type error += Invalid_map_block_fail of Script.location
type error += Invalid_iter_body : Script.location * _ stack_ty * _ stack_ty -> error
type error += Type_too_large : Script.location * int * int -> error

(* Value typing errors *)
type error += Invalid_constant : Script.location * Script.expr * _ ty -> error
type error += Invalid_contract of Script.location * Contract.t
type error += Comparable_type_expected : Script.location * _ ty -> error
type error += Inconsistent_types : _ ty * _ ty -> error
type error += Unordered_map_keys of Script.location * Script.expr
type error += Unordered_set_values of Script.location * Script.expr
type error += Duplicate_map_keys of Script.location * Script.expr
type error += Duplicate_set_values of Script.location * Script.expr

(* Toplevel errors *)
type error += Ill_typed_data : string option * Script.expr * _ ty -> error
type error += Ill_formed_type of string option * Script.expr * Script.location
type error += Ill_typed_contract : Script.expr * type_map -> error
