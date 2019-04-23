(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Script


(* ---- Error definitions ---------------------------------------------------*)

(* Auxiliary types for error documentation *)
type namespace = Type_namespace | Constant_namespace | Instr_namespace | Keyword_namespace
type kind = Int_kind | String_kind | Bytes_kind | Prim_kind | Seq_kind
type unparsed_stack_ty = (Script.expr * Script.annot) list
type type_map = (int * (unparsed_stack_ty * unparsed_stack_ty)) list

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
type error += Undefined_binop : Script.location * prim * Script.expr * Script.expr -> error
type error += Undefined_unop : Script.location * prim * Script.expr -> error
type error += Bad_return : Script.location * unparsed_stack_ty * Script.expr -> error
type error += Bad_stack : Script.location * prim * int * unparsed_stack_ty -> error
type error += Unmatched_branches : Script.location * unparsed_stack_ty * unparsed_stack_ty -> error
type error += Self_in_lambda of Script.location
type error += Bad_stack_length
type error += Bad_stack_item of int
type error += Inconsistent_annotations of string * string
type error += Inconsistent_type_annotations : Script.location * Script.expr * Script.expr -> error
type error += Inconsistent_field_annotations of string * string
type error += Unexpected_annotation of Script.location
type error += Ungrouped_annotations of Script.location
type error += Invalid_map_body : Script.location * unparsed_stack_ty -> error
type error += Invalid_map_block_fail of Script.location
type error += Invalid_iter_body : Script.location * unparsed_stack_ty * unparsed_stack_ty -> error
type error += Type_too_large : Script.location * int * int -> error

(* Value typing errors *)
type error += Invalid_constant : Script.location * Script.expr * Script.expr -> error
type error += Invalid_contract of Script.location * Contract.t
type error += Comparable_type_expected : Script.location * Script.expr -> error
type error += Inconsistent_types : Script.expr * Script.expr -> error
type error += Unordered_map_keys of Script.location * Script.expr
type error += Unordered_set_values of Script.location * Script.expr
type error += Duplicate_map_keys of Script.location * Script.expr
type error += Duplicate_set_values of Script.location * Script.expr

(* Toplevel errors *)
type error += Ill_typed_data : string option * Script.expr * Script.expr -> error
type error += Ill_formed_type of string option * Script.expr * Script.location
type error += Ill_typed_contract : Script.expr * type_map -> error

(* Gas related errors *)
type error += Cannot_serialize_error
