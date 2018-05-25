(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context
open Script_typed_ir

(** Default annotations *)

val default_now_annot : var_annot option
val default_amount_annot : var_annot option
val default_balance_annot : var_annot option
val default_steps_annot : var_annot option
val default_source_annot : var_annot option
val default_self_annot : var_annot option

val default_param_annot : field_annot option
val default_storage_annot : field_annot option
val default_car_annot : field_annot option
val default_cdr_annot : field_annot option
val default_contract_annot : field_annot option
val default_addr_annot : field_annot option
val default_manager_annot : field_annot option

val default_arg_annot : binding_annot option
val default_elt_annot : binding_annot option
val default_key_annot : binding_annot option
val default_hd_annot : binding_annot option
val default_some_annot : binding_annot option
val default_left_annot : binding_annot option
val default_right_annot : binding_annot option

(** Unparse annotations to their string representation *)

val unparse_type_annot : type_annot option -> string list
val unparse_var_annot : var_annot option -> string list
val unparse_field_annot : field_annot option -> string list
val unparse_binding_annot : binding_annot option -> string list

(** Convertions functions between different annotation kinds *)

val field_to_var_annot : field_annot option -> var_annot option
val field_to_binding_annot : field_annot option -> binding_annot option
val binding_to_var_annot : binding_annot option -> var_annot option
val binding_to_field_annot : binding_annot option -> field_annot option
val var_to_binding_annot : var_annot option -> binding_annot option
val type_to_field_annot : type_annot option -> field_annot option
val var_to_field_annot : var_annot option -> field_annot option

(** Replace an annotation by its default value if it is [None] *)
val default_annot : default:'a option -> 'a option -> 'a option

(** Generate annotation for field accesses, of the form @var.field1.field2 *)
val gen_access_annot :
  var_annot option ->
  ?default:field_annot option -> field_annot option -> var_annot option

(** Generate a binding annotation, of the form $var.some *)
val gen_binding_access_annot :
  var_annot option ->
  ?default:binding_annot option ->
  binding_annot option -> binding_annot option

(** Merge type annotations.
    @returns an error {!Inconsistent_type_annotations} if they are both present
    and different *)
val merge_type_annot :
  type_annot option -> type_annot option -> type_annot option tzresult

(** Merge field annotations, does not fail ([None] if different). *)
val merge_field_annot :
  field_annot option -> field_annot option -> field_annot option tzresult

(** Merge variable annotations, does not fail ([None] if different). *)
val merge_var_annot :
  var_annot option -> var_annot option -> var_annot option

(** @returns an error {!Unexpected_annotation} in the monad the list is not empty. *)
val error_unexpected_annot : int -> 'a list -> unit tzresult

(** Same as {!error_unexpected_annot} in Lwt. *)
val fail_unexpected_annot : int -> 'a list -> unit tzresult Lwt.t

(** Parse string annotations. *)
val parse_annots : int -> string list -> annot list tzresult

(** Parse a type annotation only. *)
val parse_type_annot : int -> string list -> type_annot option tzresult

(** Parse an annotation for composed types, of the form
    [:ty_name %field1 %field2] in any order. *)
val parse_composed_type_annot :
  int -> string list ->
  (type_annot option * field_annot option * field_annot option) tzresult

(** Check that type annotations are consistent *)
val check_const_type_annot :
  int -> string list -> type_annot option -> unit tzresult Lwt.t

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
  string list -> var_annot option tzresult Lwt.t

val parse_field_annot :
  int -> string list -> field_annot option tzresult Lwt.t

val parse_constr_annot :
  int -> string list ->
  (var_annot option * type_annot option * field_annot option * field_annot option) tzresult Lwt.t

val parse_map_annot :
  int -> string list ->
  (var_annot option * type_annot option * binding_annot option * binding_annot option) tzresult Lwt.t

val parse_two_var_annot :
  int -> string list -> (var_annot option * var_annot option) tzresult Lwt.t

val parse_two_binding_annot :
  int -> string list ->
  (binding_annot option * binding_annot option) tzresult Lwt.t

val parse_var_field_annot :
  int -> string list -> (var_annot option * field_annot option) tzresult Lwt.t

val parse_var_type_annot :
  int -> string list -> (var_annot option * type_annot option) tzresult Lwt.t

val parse_binding_annot :
  int -> string list -> binding_annot option tzresult Lwt.t

val parse_var_binding_annot :
  int -> string list -> (var_annot option * binding_annot option) tzresult Lwt.t

val parse_var_type_binding_annot :
  int -> string list ->
  (var_annot option * type_annot option * binding_annot option) tzresult Lwt.t
