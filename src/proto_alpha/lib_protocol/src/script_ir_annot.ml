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

let default_param_annot = Some (`Field_annot "parameter")
let default_storage_annot = Some (`Field_annot "storage")
let default_car_annot = Some (`Field_annot "car")
let default_cdr_annot = Some (`Field_annot "cdr")
let default_contract_annot = Some (`Field_annot "contract")
let default_addr_annot = Some (`Field_annot "address")
let default_manager_annot = Some (`Field_annot "manager")

let default_arg_annot = Some (`Binding_annot "arg")
let default_elt_annot = Some (`Binding_annot "elt")
let default_key_annot = Some (`Binding_annot "key")
let default_hd_annot = Some (`Binding_annot "hd")
let default_some_annot = Some (`Binding_annot "some")
let default_left_annot = Some (`Binding_annot "left")
let default_right_annot = Some (`Binding_annot "right")

let unparse_type_annot : type_annot option -> string list = function
  | None -> []
  | Some `Type_annot a -> [ ":" ^ a ]

let unparse_var_annot : var_annot option -> string list = function
  | None -> []
  | Some `Var_annot a -> [ "@" ^ a ]

let unparse_field_annot : field_annot option -> string list = function
  | None -> []
  | Some `Field_annot a -> [ "%" ^ a ]

let unparse_binding_annot : binding_annot option -> string list = function
  | None -> []
  | Some `Binding_annot a -> [ "$" ^ a ]

let field_to_var_annot : field_annot option -> var_annot option =
  function
  | None -> None
  | Some (`Field_annot s) -> Some (`Var_annot s)

let field_to_binding_annot : field_annot option -> binding_annot option =
  function
  | None -> None
  | Some (`Field_annot s) -> Some (`Binding_annot s)

let binding_to_var_annot : binding_annot option -> var_annot option =
  function
  | None -> None
  | Some (`Binding_annot s) -> Some (`Var_annot s)

let binding_to_field_annot : binding_annot option -> field_annot option =
  function
  | None -> None
  | Some (`Binding_annot s) -> Some (`Field_annot s)

let var_to_binding_annot : var_annot option -> binding_annot option =
  function
  | None -> None
  | Some (`Var_annot s) -> Some (`Binding_annot s)

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
    | None, None, _ | Some _, None, None -> None
    | None, Some `Field_annot f, _ ->
        Some (`Var_annot f)
    | Some `Var_annot v, None, Some `Field_annot f ->
        Some (`Var_annot (String.concat "." [v; f]))
    | Some `Var_annot v, Some `Field_annot f, _ ->
        Some (`Var_annot (String.concat "." [v; f]))

let gen_binding_access_annot
  : var_annot option -> ?default:binding_annot option -> binding_annot option -> binding_annot option
  = fun value_annot ?(default=None) binding_annot ->
    match value_annot, binding_annot, default with
    | None, None, _ | Some _, None, None -> None
    | None, Some `Binding_annot b, _ ->
        Some (`Binding_annot b)
    | Some `Var_annot v, None, Some `Binding_annot b ->
        Some (`Binding_annot (String.concat "." [v; b]))
    | Some `Var_annot v, Some `Binding_annot b, _ ->
        Some (`Binding_annot (String.concat "." [v; b]))

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
        else ok None (* TODO check this, do we want typechecking here ?
                        error (Inconsistent_annotations ("%" ^ a1, "%" ^ a2)) *)

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
  List.fold_left (fun acc s ->
      match acc with
      | Ok acc ->
          begin match s.[0] with
            | '@' -> ok (`Var_annot (String.sub s 1 @@ String.length s - 1) :: acc)
            | ':' -> ok (`Type_annot (String.sub s 1 @@ String.length s - 1) :: acc)
            | '%' -> ok (`Field_annot (String.sub s 1 @@ String.length s - 1) :: acc)
            | '$' -> ok (`Binding_annot (String.sub s 1 @@ String.length s - 1) :: acc)
            | _ -> error (Unexpected_annotation loc)
            | exception Invalid_argument _ -> error (Unexpected_annotation loc)
          end
      | Error _ -> acc
    ) (ok []) l
  >|? List.rev

let parse_type_annot
  : int -> string list -> type_annot option tzresult
  = fun loc annot ->
    parse_annots loc annot >>? function
    | [] -> ok None
    | [ `Type_annot _ as a ] -> ok (Some a)
    | _ -> error (Unexpected_annotation loc)

let parse_composed_type_annot
  : int -> string list -> (type_annot option * field_annot option * field_annot option) tzresult
  = fun loc annot ->
    parse_annots loc annot >>? function
    | [] -> ok (None, None, None)
    | [ `Type_annot _ as a ] -> ok (Some a, None, None)
    | [ `Type_annot _ as a ; `Field_annot _ as b] -> ok (Some a, Some b, None)
    | [ `Type_annot _ as a ; `Field_annot _ as b; `Field_annot _ as c ] ->
        ok (Some a, Some b, Some c)
    | [ `Field_annot _ as b ] ->
        ok (None, Some b, None)
    | [ `Field_annot _ as b; `Field_annot _ as c ] ->
        ok (None, Some b, Some c)
    | _ -> error (Unexpected_annotation loc)

let check_const_type_annot
  : int -> string list -> type_annot option -> unit tzresult Lwt.t
  = fun loc annot expected_annot ->
    Lwt.return
      (parse_type_annot loc annot >>? merge_type_annot expected_annot >|? fun _ -> ())

let parse_field_annot
  : int -> string list -> field_annot option tzresult
  = fun loc annot ->
    parse_annots loc annot >>?
    function
    | [] -> ok None
    | [ `Field_annot _ as a ] -> ok (Some a)
    | _ -> error (Unexpected_annotation loc) (* (Invalid_var_annotation (loc, annot)) *)

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
    var_annot option tzresult Lwt.t
  = fun loc ?default annot ->
    Lwt.return (parse_annots loc annot) >>=? fun annot ->
    begin match annot, default with
      | [], None -> ok None
      | [], Some d -> ok d
      | [ `Var_annot _ as a ], _ -> ok (Some a)
      | _ -> error (Unexpected_annotation loc) (* (Invalid_var_annotation (loc, annot)) *)
    end |> Lwt.return

let parse_field_annot loc annot =
  Lwt.return (parse_field_annot loc annot)

let classify_annot
  : annot list -> var_annot list * type_annot list * field_annot list * binding_annot list
  = fun l ->
    let rv, rt, rf, rb = List.fold_left (fun (rv, rt, rf, rb) -> function
        | `Var_annot _ as a -> a :: rv, rt, rf, rb
        | `Type_annot _ as a -> rv, a :: rt, rf, rb
        | `Field_annot _ as a -> rv, rt, a :: rf, rb
        | `Binding_annot _ as a -> rv, rt, rf, a :: rb
      ) ([], [], [], []) l in
    List.rev rv, List.rev rt, List.rev rf, List.rev rb

let get_one_annot loc = function
  | [] -> Lwt.return (ok None)
  | [ a ] -> Lwt.return (ok (Some a))
  | _ -> Lwt.return (error (Unexpected_annotation loc))

let get_two_annot loc = function
  | [] -> Lwt.return (ok (None, None))
  | [ a ] -> Lwt.return (ok (Some a, None))
  | [ a; b ] -> Lwt.return (ok (Some a, Some b))
  | _ -> Lwt.return (error (Unexpected_annotation loc))

let parse_constr_annot
  : int -> string list ->
    (var_annot option * type_annot option * field_annot option * field_annot option) tzresult Lwt.t
  = fun loc annot ->
    Lwt.return (parse_annots loc annot) >>=? fun annot ->
    let vars, types, fields, bindings = classify_annot annot in
    fail_unexpected_annot loc bindings >>=? fun () ->
    get_one_annot loc vars >>=? fun v ->
    get_one_annot loc types >>=? fun t ->
    get_two_annot loc fields >>|? fun (f1, f2) ->
    (v, t, f1, f2)

let parse_map_annot
  : int -> string list ->
    (var_annot option * type_annot option * binding_annot option * binding_annot option) tzresult Lwt.t
  = fun loc annot ->
    Lwt.return (parse_annots loc annot) >>=? fun annot ->
    let vars, types, fields, bindings = classify_annot annot in
    fail_unexpected_annot loc fields >>=? fun () ->
    get_one_annot loc vars >>=? fun v ->
    get_one_annot loc types >>=? fun t ->
    get_two_annot loc bindings >>|? fun (b1, b2) ->
    (v, t, b1, b2)

let parse_two_var_annot
  : int -> string list -> (var_annot option * var_annot option) tzresult Lwt.t
  = fun loc annot ->
    Lwt.return (parse_annots loc annot) >>=? fun annot ->
    let vars, types, fields, bindings = classify_annot annot in
    fail_unexpected_annot loc bindings >>=? fun () ->
    fail_unexpected_annot loc types >>=? fun () ->
    fail_unexpected_annot loc fields >>=? fun () ->
    get_two_annot loc vars

let parse_two_binding_annot
  : int -> string list -> (binding_annot option * binding_annot option) tzresult Lwt.t
  = fun loc annot ->
    Lwt.return (parse_annots loc annot) >>=? fun annot ->
    let vars, types, fields, bindings = classify_annot annot in
    fail_unexpected_annot loc vars >>=? fun () ->
    fail_unexpected_annot loc types >>=? fun () ->
    fail_unexpected_annot loc fields >>=? fun () ->
    get_two_annot loc bindings

let parse_var_field_annot
  : int -> string list -> (var_annot option * field_annot option) tzresult Lwt.t
  = fun loc annot ->
    Lwt.return (parse_annots loc annot) >>=? fun annot ->
    let vars, types, fields, bindings = classify_annot annot in
    fail_unexpected_annot loc types >>=? fun () ->
    fail_unexpected_annot loc bindings >>=? fun () ->
    get_one_annot loc vars >>=? fun v ->
    get_one_annot loc fields >>|? fun f ->
    (v, f)

let parse_var_type_annot
  : int -> string list -> (var_annot option * type_annot option) tzresult Lwt.t
  = fun loc annot ->
    Lwt.return (parse_annots loc annot) >>=? fun annot ->
    let vars, types, fields, bindings = classify_annot annot in
    fail_unexpected_annot loc fields >>=? fun () ->
    fail_unexpected_annot loc bindings >>=? fun () ->
    get_one_annot loc vars >>=? fun v ->
    get_one_annot loc types >>|? fun t ->
    (v, t)

let parse_binding_annot
  : int -> string list -> binding_annot option tzresult Lwt.t
  = fun loc annot ->
    Lwt.return (parse_annots loc annot) >>=? fun annot ->
    let vars, types, fields, bindings = classify_annot annot in
    fail_unexpected_annot loc vars >>=? fun () ->
    fail_unexpected_annot loc types >>=? fun () ->
    fail_unexpected_annot loc fields >>=? fun () ->
    get_one_annot loc bindings

let parse_var_binding_annot
  : int -> string list -> (var_annot option * binding_annot option) tzresult Lwt.t
  = fun loc annot ->
    Lwt.return (parse_annots loc annot) >>=? fun annot ->
    let vars, types, fields, bindings = classify_annot annot in
    fail_unexpected_annot loc types >>=? fun () ->
    fail_unexpected_annot loc fields >>=? fun () ->
    get_one_annot loc vars >>=? fun v ->
    get_one_annot loc bindings >>|? fun b ->
    (v, b)
