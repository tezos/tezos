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
open Micheline
open Script_tc_errors
open Script_typed_ir

let default_now_annot = Some (`Var_annot "now")
let default_amount_annot = Some (`Var_annot "amount")
let default_balance_annot = Some (`Var_annot "balance")
let default_steps_annot = Some (`Var_annot "steps")
let default_source_annot = Some (`Var_annot "source")
let default_sender_annot = Some (`Var_annot "sender")
let default_self_annot = Some (`Var_annot "self")
let default_arg_annot = Some (`Var_annot "arg")
let default_param_annot = Some (`Var_annot "parameter")
let default_storage_annot = Some (`Var_annot "storage")

let default_car_annot = Some (`Field_annot "car")
let default_cdr_annot = Some (`Field_annot "cdr")
let default_contract_annot = Some (`Field_annot "contract")
let default_addr_annot = Some (`Field_annot "address")
let default_manager_annot = Some (`Field_annot "manager")
let default_pack_annot = Some (`Field_annot "packed")
let default_unpack_annot = Some (`Field_annot "unpacked")
let default_slice_annot = Some (`Field_annot "slice")

let default_elt_annot = Some (`Field_annot "elt")
let default_key_annot = Some (`Field_annot "key")
let default_hd_annot = Some (`Field_annot "hd")
let default_tl_annot = Some (`Field_annot "tl")
let default_some_annot = Some (`Field_annot "some")
let default_left_annot = Some (`Field_annot "left")
let default_right_annot = Some (`Field_annot "right")
let default_binding_annot = Some (`Field_annot "bnd")

let unparse_type_annot : type_annot option -> string list = function
  | None -> []
  | Some `Type_annot a -> [ ":" ^ a ]

let unparse_var_annot : var_annot option -> string list = function
  | None -> []
  | Some `Var_annot a -> [ "@" ^ a ]

let unparse_field_annot : field_annot option -> string list = function
  | None -> []
  | Some `Field_annot a -> [ "%" ^ a ]

let field_to_var_annot : field_annot option -> var_annot option =
  function
  | None -> None
  | Some (`Field_annot s) -> Some (`Var_annot s)

let type_to_var_annot : type_annot option -> var_annot option =
  function
  | None -> None
  | Some (`Type_annot s) -> Some (`Var_annot s)

let var_to_field_annot : var_annot option -> field_annot option =
  function
  | None -> None
  | Some (`Var_annot s) -> Some (`Field_annot s)

let default_annot ~default = function
  | None -> default
  | annot -> annot

let gen_access_annot
  : var_annot option -> ?default:field_annot option -> field_annot option -> var_annot option
  = fun value_annot ?(default=None) field_annot ->
    match value_annot, field_annot, default with
    | None, None, _ | Some _, None, None | None, Some `Field_annot "", _ -> None
    | None, Some `Field_annot f, _ ->
        Some (`Var_annot f)
    | Some `Var_annot v, (None | Some `Field_annot ""), Some `Field_annot f ->
        Some (`Var_annot (String.concat "." [v; f]))
    | Some `Var_annot v, Some `Field_annot f, _ ->
        Some (`Var_annot (String.concat "." [v; f]))

let merge_type_annot
  : type_annot option -> type_annot option -> type_annot option tzresult
  = fun annot1 annot2 ->
    match annot1, annot2 with
    | None, None
    | Some _, None
    | None, Some _ -> ok None
    | Some `Type_annot a1, Some `Type_annot a2 ->
        if String.equal a1 a2
        then ok annot1
        else error (Inconsistent_annotations (":" ^ a1, ":" ^ a2))

let merge_field_annot
  : field_annot option -> field_annot option -> field_annot option tzresult
  = fun annot1 annot2 ->
    match annot1, annot2 with
    | None, None
    | Some _, None
    | None, Some _ -> ok None
    | Some `Field_annot a1, Some `Field_annot a2 ->
        if String.equal a1 a2
        then ok annot1
        else error (Inconsistent_annotations ("%" ^ a1, "%" ^ a2))

let merge_var_annot
  : var_annot option -> var_annot option -> var_annot option
  = fun annot1 annot2 ->
    match annot1, annot2 with
    | None, None
    | Some _, None
    | None, Some _ -> None
    | Some `Var_annot a1, Some `Var_annot a2 ->
        if String.equal a1 a2 then annot1 else None

let error_unexpected_annot loc annot =
  match annot with
  | [] -> ok ()
  | _ :: _ -> error (Unexpected_annotation loc)

let fail_unexpected_annot loc annot =
  Lwt.return (error_unexpected_annot loc annot)

let parse_annots loc ?(allow_special_var = false) ?(allow_special_field = false) l =
  (* allow emtpty annotations as wildcards but otherwise only accept
     annotations that start with [a-zA-Z_] *)
  let sub_or_wildcard ~specials wrap s acc =
    let len = String.length s in
    if Compare.Int.(len = 1) then ok @@ wrap None :: acc
    else match s.[1] with
      | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
          ok @@ wrap (Some (String.sub s 1 (len - 1))) :: acc
      | '@' when Compare.Int.(len = 2) && List.mem '@' specials ->
          ok @@ wrap (Some "@") :: acc
      | '%' when List.mem '%' specials ->
          if Compare.Int.(len = 2)
          then ok @@ wrap (Some "%") :: acc
          else if Compare.Int.(len = 3) && Compare.Char.(s.[2] = '%')
          then ok @@ wrap (Some "%%") :: acc
          else error (Unexpected_annotation loc)
      | _ -> error (Unexpected_annotation loc) in
  List.fold_left (fun acc s ->
      acc >>? fun acc ->
      if Compare.Int.(String.length s = 0) then
        error (Unexpected_annotation loc)
      else match s.[0] with
        | ':' -> sub_or_wildcard ~specials:[] (fun a -> `Type_annot a) s acc
        | '@' ->
            sub_or_wildcard
              ~specials:(if allow_special_var then ['%'] else [])
              (fun a -> `Var_annot a) s acc
        | '%' -> sub_or_wildcard
                   ~specials:(if allow_special_field then ['@'] else [])
                   (fun a -> `Field_annot a) s acc
        | _ -> error (Unexpected_annotation loc)
    ) (ok []) l
  >|? List.rev

let opt_var_of_var_opt = function
  | `Var_annot None -> None
  | `Var_annot Some a -> Some (`Var_annot a)

let opt_field_of_field_opt = function
  | `Field_annot None -> None
  | `Field_annot Some a -> Some (`Field_annot a)

let opt_type_of_type_opt = function
  | `Type_annot None -> None
  | `Type_annot Some a -> Some (`Type_annot a)

let classify_annot loc l
  : (var_annot option list * type_annot option list * field_annot option list) tzresult
  =
  try
    let _, rv, _, rt, _, rf =
      List.fold_left
        (fun (in_v, rv, in_t, rt, in_f, rf) a ->
           match a, in_v, rv, in_t, rt, in_f, rf with
           | (`Var_annot _ as a), true, _, _, _, _, _
           | (`Var_annot _ as a), false, [], _, _, _, _ ->
               true, opt_var_of_var_opt a :: rv,
               false, rt,
               false, rf
           | (`Type_annot _ as a), _, _, true, _, _, _
           | (`Type_annot _ as a), _, _, false, [], _, _ ->
               false, rv,
               true, opt_type_of_type_opt a :: rt,
               false, rf
           | (`Field_annot _ as a), _, _, _, _, true, _
           | (`Field_annot _ as a), _, _, _, _, false, [] ->
               false, rv,
               false, rt,
               true, opt_field_of_field_opt a :: rf
           | _ -> raise Exit
        ) (false, [], false, [], false, []) l in
    ok (List.rev rv, List.rev rt, List.rev rf)
  with Exit -> error (Ungrouped_annotations loc)

let get_one_annot loc = function
  | [] -> ok None
  | [ a ] -> ok a
  | _ -> error (Unexpected_annotation loc)

let get_two_annot loc = function
  | [] -> ok (None, None)
  | [ a ] -> ok (a, None)
  | [ a; b ] -> ok (a, b)
  | _ -> error (Unexpected_annotation loc)

let parse_type_annot
  : int -> string list -> type_annot option tzresult
  = fun loc annot ->
    parse_annots loc annot >>?
    classify_annot loc >>? fun (vars, types, fields) ->
    error_unexpected_annot loc vars >>? fun () ->
    error_unexpected_annot loc fields >>? fun () ->
    get_one_annot loc types

let parse_type_field_annot
  : int -> string list -> (type_annot option * field_annot option) tzresult
  = fun loc annot ->
    parse_annots loc annot >>?
    classify_annot loc >>? fun (vars, types, fields) ->
    error_unexpected_annot loc vars >>? fun () ->
    get_one_annot loc types >>? fun t ->
    get_one_annot loc fields >|? fun f ->
    (t, f)

let parse_composed_type_annot
  : int -> string list -> (type_annot option * field_annot option * field_annot option) tzresult
  = fun loc annot ->
    parse_annots loc annot >>?
    classify_annot loc >>? fun (vars, types, fields) ->
    error_unexpected_annot loc vars >>? fun () ->
    get_one_annot loc types >>? fun t ->
    get_two_annot loc fields >|? fun (f1, f2) ->
    (t, f1, f2)

let check_const_type_annot
  : int -> string list -> type_annot option -> field_annot option list -> unit tzresult Lwt.t
  = fun loc annot expected_name expected_fields ->
    Lwt.return
      (parse_composed_type_annot loc annot >>? fun (ty_name, field1, field2) ->
       merge_type_annot expected_name ty_name >>? fun _ ->
       match expected_fields, field1, field2 with
       | [], Some _, _ | [], _, Some _ | [_], Some _, Some _ ->
           (* Too many annotations *)
           error (Unexpected_annotation loc)
       | _ :: _ :: _ :: _, _, _ | [_], None, Some _ ->
           error (Unexpected_annotation loc)
       | [], None, None -> ok ()
       | [ f1; f2 ], _,  _ ->
           merge_field_annot f1 field1 >>? fun _ ->
           merge_field_annot f2 field2 >|? fun _ -> ()
       | [ f1 ], _,  None ->
           merge_field_annot f1 field1 >|? fun _ -> ()
      )

let parse_field_annot
  : int -> string list -> field_annot option tzresult
  = fun loc annot ->
    parse_annots loc annot >>?
    classify_annot loc >>? fun (vars, types, fields) ->
    error_unexpected_annot loc vars >>? fun () ->
    error_unexpected_annot loc types >>? fun () ->
    get_one_annot loc fields

let extract_field_annot
  : Script.node -> (Script.node * field_annot option) tzresult
  = function
    | Prim (loc, prim, args, annot) ->
        let field_annots, annot = List.partition (fun s ->
            Compare.Int.(String.length s > 0) &&
            Compare.Char.(s.[0] = '%')
          ) annot in
        parse_field_annot loc field_annots >|? fun field_annot ->
        Prim (loc, prim, args, annot), field_annot
    | expr -> ok (expr, None)

let check_correct_field
  : field_annot option -> field_annot option -> unit tzresult
  = fun f1 f2 ->
    match f1, f2 with
    | None, _ | _, None -> ok ()
    | Some `Field_annot s1, Some `Field_annot s2 ->
        if String.equal s1 s2 then ok ()
        else error (Inconsistent_field_annotations ("%" ^ s1, "%" ^ s2))


let parse_var_annot
  : int -> ?default:var_annot option -> string list ->
    var_annot option tzresult
  = fun loc ?default annot ->
    parse_annots loc annot >>?
    classify_annot loc >>? fun (vars, types, fields) ->
    error_unexpected_annot loc types >>? fun () ->
    error_unexpected_annot loc fields >>? fun () ->
    get_one_annot loc vars >|? function
    | Some _ as a -> a
    | None -> match default with
      | Some a -> a
      | None -> None

let split_last_dot = function
  | None -> None, None
  | Some `Field_annot s ->
      match String.rindex_opt s '.' with
      | None -> None, Some (`Field_annot s)
      | Some i ->
          let s1 = String.sub s 0 i in
          let s2 = String.sub s (i + 1) (String.length s - i - 1) in
          let f =
            if Compare.String.equal s2 "car"
            || Compare.String.equal s2 "cdr" then
              None
            else
              Some (`Field_annot s2) in
          Some (`Var_annot s1), f

let common_prefix v1 v2 =
  match v1, v2 with
  | Some (`Var_annot s1), Some (`Var_annot s2) when Compare.String.equal s1 s2 -> v1
  | Some _, None -> v1
  | None, Some _ -> v2
  | _, _ -> None

let parse_constr_annot
  : int ->
    ?if_special_first:field_annot option ->
    ?if_special_second:field_annot option ->
    string list ->
    (var_annot option * type_annot option * field_annot option * field_annot option) tzresult
  = fun loc ?if_special_first ?if_special_second annot ->
    parse_annots ~allow_special_field:true loc annot >>?
    classify_annot loc >>? fun (vars, types, fields) ->
    get_one_annot loc vars >>? fun v ->
    get_one_annot loc types >>? fun t ->
    get_two_annot loc fields >>? fun (f1, f2) ->
    begin match if_special_first, f1 with
      | Some special_var, Some `Field_annot "@" ->
          ok (split_last_dot special_var)
      | None, Some `Field_annot "@" -> error (Unexpected_annotation loc)
      | _, _ -> ok (v, f1)
    end >>? fun (v1, f1) ->
    begin match if_special_second, f2 with
      | Some special_var, Some `Field_annot "@" ->
          ok (split_last_dot special_var)
      | None, Some `Field_annot "@" -> error (Unexpected_annotation loc)
      | _, _ -> ok (v, f2)
    end >|? fun (v2, f2) ->
    let v = match v with
      | None -> common_prefix v1 v2
      | Some _ -> v in
    (v, t, f1, f2)

let parse_two_var_annot
  : int -> string list -> (var_annot option * var_annot option) tzresult
  = fun loc annot ->
    parse_annots loc annot >>?
    classify_annot loc >>? fun (vars, types, fields) ->
    error_unexpected_annot loc types >>? fun () ->
    error_unexpected_annot loc fields >>? fun () ->
    get_two_annot loc vars

let parse_destr_annot
  : int -> string list -> default_accessor:field_annot option ->
  field_name:field_annot option ->
  pair_annot:var_annot option -> value_annot:var_annot option ->
  (var_annot option * field_annot option) tzresult
  = fun loc annot ~default_accessor ~field_name ~pair_annot ~value_annot ->
    parse_annots loc ~allow_special_var:true annot >>?
    classify_annot loc >>? fun (vars, types, fields) ->
    error_unexpected_annot loc types >>? fun () ->
    get_one_annot loc vars >>? fun v ->
    get_one_annot loc fields >|? fun f ->
    let default = gen_access_annot pair_annot field_name ~default:default_accessor in
    let v = match v with
      | Some `Var_annot "%" -> field_to_var_annot field_name
      | Some `Var_annot "%%" -> default
      | Some _ -> v
      | None -> value_annot in
    (v, f)

let parse_var_type_annot
  : int -> string list -> (var_annot option * type_annot option) tzresult
  = fun loc annot ->
    parse_annots loc annot >>?
    classify_annot loc >>? fun (vars, types, fields) ->
    error_unexpected_annot loc fields >>? fun () ->
    get_one_annot loc vars >>? fun v ->
    get_one_annot loc types >|? fun t ->
    (v, t)
