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
open Script_typed_ir

(** Default annotations *)

val default_now_annot : var_annot option
val default_amount_annot : var_annot option
val default_balance_annot : var_annot option
val default_steps_annot : var_annot option
val default_source_annot : var_annot option
val default_sender_annot : var_annot option
val default_self_annot : var_annot option
val default_arg_annot : var_annot option
val default_param_annot : var_annot option
val default_storage_annot : var_annot option

val default_car_annot : field_annot option
val default_cdr_annot : field_annot option
val default_contract_annot : field_annot option
val default_addr_annot : field_annot option
val default_manager_annot : field_annot option
val default_pack_annot : field_annot option
val default_unpack_annot : field_annot option
val default_slice_annot : field_annot option

val default_elt_annot : field_annot option
val default_key_annot : field_annot option
val default_hd_annot : field_annot option
val default_tl_annot : field_annot option
val default_some_annot : field_annot option
val default_left_annot : field_annot option
val default_right_annot : field_annot option
val default_binding_annot : field_annot option

(** Unparse annotations to their string representation *)

val unparse_type_annot : type_annot option -> string list
val unparse_var_annot : var_annot option -> string list
val unparse_field_annot : field_annot option -> string list

(** Convertions functions between different annotation kinds *)

val field_to_var_annot : field_annot option -> var_annot option
val type_to_var_annot : type_annot option -> var_annot option
val var_to_field_annot : var_annot option -> field_annot option

(** Replace an annotation by its default value if it is [None] *)
val default_annot : default:'a option -> 'a option -> 'a option

(** Generate annotation for field accesses, of the form [var.field1.field2] *)
val gen_access_annot :
  var_annot option ->
  ?default:field_annot option -> field_annot option -> var_annot option

(** Merge type annotations.
    @return an error {!Inconsistent_type_annotations} if they are both present
    and different *)
val merge_type_annot :
  type_annot option -> type_annot option -> type_annot option tzresult

(** Merge field annotations.
    @return an error {!Inconsistent_type_annotations} if they are both present
    and different *)
val merge_field_annot :
  field_annot option -> field_annot option -> field_annot option tzresult

(** Merge variable annotations, does not fail ([None] if different). *)
val merge_var_annot :
  var_annot option -> var_annot option -> var_annot option

(** @return an error {!Unexpected_annotation} in the monad the list is not empty. *)
val error_unexpected_annot : int -> 'a list -> unit tzresult

(** Same as {!error_unexpected_annot} in Lwt. *)
val fail_unexpected_annot : int -> 'a list -> unit tzresult Lwt.t

(** Parse a type annotation only. *)
val parse_type_annot : int -> string list -> type_annot option tzresult

(** Parse a field annotation only. *)
val parse_field_annot :
  int -> string list -> field_annot option tzresult

(** Parse an annotation for composed types, of the form
    [:ty_name %field] in any order. *)
val parse_type_field_annot :
  int -> string list -> (type_annot option * field_annot option) tzresult

(** Parse an annotation for composed types, of the form
    [:ty_name %field1 %field2] in any order. *)
val parse_composed_type_annot :
  int -> string list ->
  (type_annot option * field_annot option * field_annot option) tzresult

(** Check that type annotations on constants are consistent *)
val check_const_type_annot :
  int -> string list -> type_annot option -> field_annot option list ->
  unit tzresult Lwt.t

(** Extract and remove a field annotation from a node *)
val extract_field_annot :
  Script.node -> (Script.node * field_annot option) tzresult

(** Check that field annotations match, used for field accesses. *)
val check_correct_field :
  field_annot option -> field_annot option -> unit tzresult

(** Instruction annotations parsing *)

(** Parse a variable annotation, replaced by a default value if [None]. *)
val parse_var_annot :
  int ->
  ?default:var_annot option ->
  string list -> var_annot option tzresult

val parse_constr_annot :
  int ->
  ?if_special_first:field_annot option ->
  ?if_special_second:field_annot option ->
  string list ->
  (var_annot option * type_annot option *
   field_annot option * field_annot option) tzresult

val parse_two_var_annot :
  int -> string list -> (var_annot option * var_annot option) tzresult

val parse_destr_annot :
  int -> string list ->
  default_accessor:field_annot option ->
  field_name:field_annot option ->
  pair_annot:var_annot option ->
  value_annot:var_annot option ->
  (var_annot option * field_annot option) tzresult

val parse_var_type_annot :
  int -> string list -> (var_annot option * type_annot option) tzresult
