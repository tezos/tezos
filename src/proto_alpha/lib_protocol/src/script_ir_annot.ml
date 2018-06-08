(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context
open Micheline
open Script_tc_errors
open Script_typed_ir

let default_now_annot = Some (`Var_annot "now")
let default_amount_annot = Some (`Var_annot "amount")
let default_balance_annot = Some (`Var_annot "balance")
let default_steps_annot = Some (`Var_annot "steps")
let default_source_annot = Some (`Var_annot "source")
let default_self_annot = Some (`Var_annot "self")
let default_arg_annot = Some (`Var_annot "arg")

let default_param_annot = Some (`Field_annot "parameter")
let default_storage_annot = Some (`Field_annot "storage")
let default_car_annot = Some (`Field_annot "car")
let default_cdr_annot = Some (`Field_annot "cdr")
let default_contract_annot = Some (`Field_annot "contract")
let default_addr_annot = Some (`Field_annot "address")
let default_manager_annot = Some (`Field_annot "manager")

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

let type_to_field_annot : type_annot option -> field_annot option =
  function
  | None -> None
  | Some (`Type_annot s) -> Some (`Field_annot s)

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

let parse_annots loc l =
  (* allow emtpty annotations as wildcards but otherwise only accept
     annotations that starto with [a-zA-Z_] *)
  let sub_or_wildcard wrap s acc =
    let len = String.length s in
    if Compare.Int.(len = 1) then ok @@ wrap None :: acc
    else match s.[1] with
      | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
          ok @@ wrap (Some (String.sub s 1 (len - 1))) :: acc
      | _ -> error (Unexpected_annotation loc) in
  List.fold_left (fun acc s ->
      match acc with
      | Ok acc ->
          begin match s.[0] with
            | '@' -> sub_or_wildcard (fun a -> `Var_annot a) s acc
            | ':' -> sub_or_wildcard (fun a -> `Type_annot a) s acc
            | '%' -> sub_or_wildcard (fun a -> `Field_annot a) s acc
            | _ -> error (Unexpected_annotation loc)
            | exception Invalid_argument _ -> error (Unexpected_annotation loc)
          end
      | Error _ -> acc
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
  : int -> string list -> type_annot option -> unit tzresult Lwt.t
  = fun loc annot expected_annot ->
    Lwt.return
      (parse_type_annot loc annot >>? merge_type_annot expected_annot >|? fun _ -> ())

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
            match s.[0] with
            | '%' -> true
            | _ -> false
            | exception Invalid_argument _ -> false
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

let parse_constr_annot
  : int -> string list ->
    (var_annot option * type_annot option * field_annot option * field_annot option) tzresult
  = fun loc annot ->
    parse_annots loc annot >>?
    classify_annot loc >>? fun (vars, types, fields) ->
    get_one_annot loc vars >>? fun v ->
    get_one_annot loc types >>? fun t ->
    get_two_annot loc fields >|? fun (f1, f2) ->
    (v, t, f1, f2)

let parse_two_var_annot
  : int -> string list -> (var_annot option * var_annot option) tzresult
  = fun loc annot ->
    parse_annots loc annot >>?
    classify_annot loc >>? fun (vars, types, fields) ->
    error_unexpected_annot loc types >>? fun () ->
    error_unexpected_annot loc fields >>? fun () ->
    get_two_annot loc vars

let parse_var_field_annot
  : int -> string list -> (var_annot option * field_annot option) tzresult
  = fun loc annot ->
    parse_annots loc annot >>?
    classify_annot loc >>? fun (vars, types, fields) ->
    error_unexpected_annot loc types >>? fun () ->
    get_one_annot loc vars >>? fun v ->
    get_one_annot loc fields >|? fun f ->
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
