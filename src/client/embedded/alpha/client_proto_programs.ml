(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Ed25519 = Environment.Ed25519
open Client_proto_args

let report_parse_error prefix exn =
  let open Lexing in
  let open Script_located_ir in
  let print_point ppf { line ; column } =
    Format.fprintf ppf
      "at line %d character %d"
      line column in
  let print_token ppf = function
    | Michelson_parser.Open_paren
    | Michelson_parser.Close_paren ->
        Format.fprintf ppf "parenthesis"
    | Michelson_parser.Open_brace
    | Michelson_parser.Close_brace ->
        Format.fprintf ppf "curly brace"
    | Michelson_parser.String _ ->
        Format.fprintf ppf "string constant"
    | Michelson_parser.Int _ ->
        Format.fprintf ppf "integer constant"
    | Michelson_parser.Ident _ ->
        Format.fprintf ppf "identifier"
    | Michelson_parser.Annot _ ->
        Format.fprintf ppf "annotation"
    | Michelson_parser.Comment _
    | Michelson_parser.Eol_comment _ ->
        Format.fprintf ppf "comment"
    | Michelson_parser.Semi ->
        Format.fprintf ppf "semi colon" in
  let print_loc ppf loc =
    Format.fprintf ppf "in %s, " prefix ;
    if loc.start.line = loc.stop.line then
      if loc.start.column = loc.stop.column then
      Format.fprintf ppf
        "at line %d character %d"
        loc.start.line loc.start.column
      else
      Format.fprintf ppf
        "at line %d characters %d to %d"
        loc.start.line loc.start.column loc.stop.column
    else
      Format.fprintf ppf
        "from line %d character %d to line %d character %d"
        loc.start.line loc.start.column loc.stop.line loc.stop.column in
  match exn with
  | Script_located_ir.Missing_program_field n ->
      failwith "missing script %s" n
  | Michelson_parser.Invalid_utf8_sequence (point, str) ->
      failwith "%a, invalid UTF-8 sequence %S" print_point point str
  | Michelson_parser.Unexpected_character (point, str) ->
      failwith "%a, unexpected character %s" print_point point str
  | Michelson_parser.Undefined_escape_character (point, str) ->
      failwith "%a, undefined escape character \"%s\"" print_point point str
  | Michelson_parser.Missing_break_after_number point ->
      failwith "%a, missing break" print_point point
  | Michelson_parser.Unterminated_string loc ->
      failwith "%a, unterminated string" print_loc loc
  | Michelson_parser.Unterminated_integer loc ->
      failwith "%a, unterminated integer" print_loc loc
  | Michelson_parser.Unterminated_comment loc ->
      failwith "%a, unterminated comment" print_loc loc
  | Michelson_parser.Unclosed { loc ; token } ->
      failwith "%a, unclosed %a" print_loc loc print_token token
  | Michelson_parser.Unexpected { loc ; token } ->
      failwith "%a, unexpected %a" print_loc loc print_token token
  | Michelson_parser.Extra { loc ; token } ->
      failwith "%a, extra %a" print_loc loc print_token token
  | Michelson_parser.Misaligned node ->
      failwith "%a, misaligned expression" print_loc (node_location node)
  | Michelson_parser.Empty ->
      failwith "empty expression"
  | Failure s ->
      failwith "%s" s
  | exn ->
      failwith "%s" @@ Printexc.to_string exn

let print_location_mark ppf = function
  | None -> ()
  | Some l -> Format.fprintf ppf " /* %d */" l

let no_locations _ = None

let print_annotation ppf = function
  | None -> ()
  | Some a -> Format.fprintf ppf " %s@," a

let rec print_expr_unwrapped_help emacs locations ppf = function
  | Script.Prim (loc, name, [], None) ->
      begin match locations loc with
        | None -> Format.fprintf ppf "%s" name
        | Some _ as l -> Format.fprintf ppf "%s%a" name print_location_mark l
      end
  | Script.Prim (loc, name, _, (Some _ as annot)) ->
      Format.fprintf ppf (if emacs then "%s%a %a" else "@[<hov 2>%s%a@ %a]")
        name print_location_mark (locations loc) print_annotation annot
  | Script.Prim (loc, name, args, annot) ->
      Format.fprintf ppf "@[<hv 2>%s%a%a@ %a@]"
        name
        print_location_mark (locations loc)
        print_annotation annot
        (Format.pp_print_list
           ~pp_sep: Format.pp_print_space
           (print_expr_help emacs locations))
        args
  | Script.Seq (loc, [], None) ->
      begin match locations loc with
        | None -> Format.fprintf ppf "{}"
        | Some _ as l -> Format.fprintf ppf "{%a }" print_location_mark l
      end
  | Script.Seq (loc, exprs, annot) ->
      begin match locations loc with
        | None -> Format.fprintf ppf "@[<hv 2>{ "
        | Some _ as l -> Format.fprintf ppf "@[<hv 2>{%a@ " print_location_mark l
      end ;
      Format.fprintf ppf "%a%a@] }"
        (Format.pp_print_list
           ~pp_sep: (fun ppf () -> Format.fprintf ppf " ;@ ")
           (print_expr_unwrapped_help emacs locations))
        exprs
        print_annotation annot
  | Script.Int (loc, n) ->
      Format.fprintf ppf "%s%a" n print_location_mark (locations loc)
  | Script.String (loc, s) ->
      Format.fprintf ppf "%S%a" s print_location_mark (locations loc)

and print_expr_help emacs locations ppf = function
  | Script.Prim (_, _, _ :: _, _)
  | Script.Prim (_, _, [], Some _) as expr ->
      Format.fprintf ppf "(%a)" (print_expr_unwrapped_help emacs locations) expr
  | Script.Prim (loc, _, [], None) as expr when locations loc <> None ->
      Format.fprintf ppf "(%a)" (print_expr_unwrapped_help emacs locations) expr
  | expr -> print_expr_unwrapped_help emacs locations ppf expr

let print_expr_unwrapped = print_expr_unwrapped_help false
let print_expr = print_expr_help false

let print_storage ppf ({ storage } : Script.storage) =
  print_expr no_locations ppf storage

let print_stack_help emacs ppf = function
  | [] -> Format.fprintf ppf (if emacs then "()" else "[]")
  | more ->
      Format.fprintf ppf (if emacs then "(%a)" else "@[<hov 2>[ %a ]@]")
        (Format.pp_print_list
           ~pp_sep: (fun ppf () -> Format.fprintf ppf (if emacs then "@ " else " :@ "))
           ((if emacs then print_expr else print_expr_unwrapped) no_locations))
        more

let print_stack = print_stack_help false

let print_emacs_stack = print_stack_help true

let print_typed_code locations ppf (expr, type_map) =
  let print_stack ppf = function
    | [] -> Format.fprintf ppf "[]"
    | more ->
        Format.fprintf ppf "@[<hov 2>[ %a ]@]"
          (Format.pp_print_list
             ~pp_sep: (fun ppf () -> Format.fprintf ppf " :@ ")
             (print_expr_unwrapped no_locations))
          more in
  let print_annot ppf = function
    | None -> ()
    | Some annot -> Format.fprintf ppf " %s@," annot in
  let rec print_typed_code_unwrapped ppf expr =
    match expr with
    | Script.Prim (loc, name, [], None) ->
        Format.fprintf ppf "%s%a"
          name print_location_mark (locations loc)
    | Script.Prim (loc, name, [], Some annot) ->
        Format.fprintf ppf "(%s %s%a)"
          name annot print_location_mark (locations loc)
    | Script.Prim (loc, name, args, annot) ->
        Format.fprintf ppf "@[<v 2>%s%a%a@ %a@]"
          name print_annot annot print_location_mark (locations loc)
          (Format.pp_print_list
             ~pp_sep: Format.pp_print_space
             print_typed_code)
          args
    | Script.Seq (loc, [], None) ->
        begin match List.assoc loc type_map with
          | exception Not_found -> Format.fprintf ppf "{}"
          | (first, _) ->
              match locations loc with
              | None ->
                  Format.fprintf ppf "{} /* %a */"
                    print_stack first
              | Some _ as l ->
                  Format.fprintf ppf "{%a %a }"
                    print_location_mark l print_stack first
        end
    | Script.Seq (loc, [], Some annot) ->
        begin match List.assoc loc type_map with
          | exception Not_found -> Format.fprintf ppf "{ %@%s }" annot
          | (first, _) ->
              match locations loc with
              | None ->
                  Format.fprintf ppf "{ %@%s } /* %a */"
                    annot
                    print_stack first
              | Some _ as l ->
                  Format.fprintf ppf "{ %@%s%a %a }"
                    annot print_location_mark l print_stack first
        end
    | Script.Seq (loc, exprs, annot) ->
        begin match locations loc, annot with
          | None, None ->
              Format.fprintf ppf "@[<v 2>{ "
          | None, Some annot ->
              Format.fprintf ppf "@[<v 2>{ %@%s@," annot
          | Some _ as l, _ ->
              Format.fprintf ppf "@[<v 2>{%a%a@,"
                print_annot annot
                print_location_mark l
        end ;
        let rec loop = function
          | [] -> assert false
          | [ Script.Int (loc, _) | String (loc, _) | Prim (loc, _, _, _) as expr ] ->
              begin match List.assoc loc type_map with
                | exception Not_found ->
                    Format.fprintf ppf "%a }@]"
                      print_typed_code_unwrapped expr
                | (before, after) ->
                    Format.fprintf ppf "/* %a */@,%a@,/* %a */ }@]"
                      print_stack before
                      print_typed_code_unwrapped expr
                      print_stack after
              end ;
          | Script.Int (loc, _) | String (loc, _) | Prim (loc, _, _, _) as expr :: rest ->
              begin match List.assoc loc type_map with
                | exception Not_found ->
                    Format.fprintf ppf "%a ;@,"
                      print_typed_code_unwrapped expr ;
                    loop rest
                | (before, _) ->
                    Format.fprintf ppf "/* %a */@,%a ;@,"
                      print_stack before
                      print_typed_code_unwrapped expr ;
                    loop rest
              end ;
          | [ Seq (_, _, _) as expr ] ->
              Format.fprintf ppf "%a }@]"
                print_typed_code_unwrapped expr
          | Seq (_, _, _) as expr :: rest ->
              Format.fprintf ppf "%a@,"
                print_typed_code_unwrapped expr ;
              loop rest in
        loop exprs ;
    | Script.Int (loc, n) ->
        Format.fprintf ppf "%s%a" n print_location_mark (locations loc)
    | Script.String (loc, s) ->
        Format.fprintf ppf "%S%a" s print_location_mark (locations loc)
  and print_typed_code ppf = function
    | Script.Prim (_, _, _ :: _, _) as expr ->
        Format.fprintf ppf "(%a)" print_typed_code_unwrapped expr
    | expr -> print_typed_code_unwrapped ppf expr in
  print_typed_code_unwrapped ppf expr

let print_program locations ppf ((c : Script.code), type_map) =
  Format.fprintf ppf
    "@[<v 0>%a ;@,%a ;@,%a ;@,\
     @[<hov 2>code@ %a@]@]"
    (print_expr_unwrapped no_locations) (Script.Prim (-1, "storage", [ c.storage_type ], None))
    (print_expr_unwrapped no_locations) (Script.Prim (-1, "parameter", [ c.arg_type ], None))
    (print_expr_unwrapped no_locations) (Script.Prim (-1, "return", [ c.ret_type ], None))
    (print_typed_code locations) (c.code, type_map)

let collect_error_locations errs =
  let open Script_typed_ir in
  let open Script_ir_translator in
  let open Script_interpreter in
  let rec collect acc = function
    | (Ill_typed_data (_, _, _)
      | Ill_formed_type (_, _)
      | Ill_typed_contract (_, _, _, _, _)) :: _
    | [] -> acc
    | (Invalid_arity (loc, _, _, _)
      | Invalid_namespace (loc, _, _, _)
      | Invalid_primitive (loc, _, _)
      | Invalid_case (loc, _)
      | Invalid_kind (loc, _, _)
      | Fail_not_in_tail_position loc
      | Undefined_binop (loc, _, _, _)
      | Undefined_unop (loc, _, _)
      | Bad_return (loc, _, _)
      | Bad_stack (loc, _, _, _)
      | Unmatched_branches (loc, _, _)
      | Transfer_in_lambda loc
      | Transfer_in_dip loc
      | Invalid_constant (loc, _, _)
      | Invalid_contract (loc, _)
      | Comparable_type_expected (loc, _)
      | Overflow loc
      | Reject loc) :: rest ->
        collect (loc :: acc) rest
    | _ :: rest -> collect acc rest in
  collect [] errs

let report_errors cctxt errs =
  let open Client_commands in
  let open Script_typed_ir in
  let open Script_ir_translator in
  let open Script_interpreter in
  let rec print_ty (type t) ppf (ty : t ty) =
    let expr = unparse_ty ty in
    print_expr no_locations ppf expr in
  let rec print_stack_ty (type t) ?(depth = max_int) ppf (s : t stack_ty) =
    let rec loop
      : type t. int -> Format.formatter -> t stack_ty -> unit
      = fun depth ppf -> function
        | Empty_t -> ()
        | _ when depth <= 0 ->
            Format.fprintf ppf "..."
        | Item_t (last, Empty_t) ->
            Format.fprintf ppf "%a"
              print_ty last
        | Item_t (last, rest) ->
            Format.fprintf ppf "%a :@ %a"
              print_ty last (loop (depth - 1)) rest in
    match s with
    | Empty_t ->
        Format.fprintf ppf "[]"
    | sty ->
        Format.fprintf ppf "@[<hov 2>[ %a ]@]" (loop depth) sty in
  let rec print_enumeration ppf = function
    | [ single ] ->
        Format.fprintf ppf "%a"
          Format.pp_print_text single
    | [ prev ; last ] ->
        Format.fprintf ppf "%a@ or@ %a"
          Format.pp_print_text prev Format.pp_print_text last
    | first :: rest ->
        Format.fprintf ppf "%a,@ %a"
          Format.pp_print_text first print_enumeration rest
    | [] -> assert false in
  let print_error locations err =
    let print_loc ppf loc =
      match locations loc with
      | None ->
          Format.fprintf ppf "At (unmarked) location %d, " loc
      | Some loc ->
          Format.fprintf ppf "At mark /* %d */, " loc in
    match err with
    | Ill_typed_data (name, expr, ty) ->
        cctxt.warning
          "@[<hv 0>@[<hov 2>Ill typed %adata:@ %a@]@ \
           @[<hov 2>is not an expression of type@ %a@]@]"
          (fun ppf -> function
             | None -> ()
             | Some s -> Format.fprintf ppf "%s " s)
          name
          (print_expr locations) expr
          print_ty ty
    | Ill_formed_type (name, expr) ->
        cctxt.warning
          "@[<hov 2>Ill formed type %aexpression@ %a@]"
          (fun ppf -> function
             | None -> ()
             | Some s -> Format.fprintf ppf "%s " s)
          name
          (print_expr locations) expr
    | Apply.Bad_contract_parameter (c, None, _) ->
        cctxt.warning
          "@[<v 0>Account %a is not a smart contract, it does not take arguments.@,\
           The `-arg' flag cannot be used when transferring to an account.@]"
          Contract.pp c
    | Apply.Bad_contract_parameter (c, Some expected, None) ->
        cctxt.warning
          "@[<v 0>Contract %a expected an argument of type@,  %a@,but no argument was provided.@,\
           The `-arg' flag can be used when transferring to a smart contract.@]"
          Contract.pp c
          (print_expr_unwrapped no_locations) expected
    | Apply.Bad_contract_parameter (c, Some expected, Some argument) ->
        cctxt.warning
          "@[<v 0>Contract %a expected an argument of type@,  %a@but received@,  %a@]"
          Contract.pp c
          (print_expr_unwrapped no_locations) expected
          (print_expr_unwrapped no_locations) argument
    | Ill_typed_contract (expr, arg_ty, ret_ty, storage_ty, type_map) ->
        cctxt.warning
          "@[<v 2>Ill typed contract:@ %a@]"
          (print_program locations)
          ({ Script.storage_type = unparse_ty storage_ty ;
             arg_type = unparse_ty arg_ty ;
             ret_type = unparse_ty ret_ty ;
             code = expr }, type_map)
    | Runtime_contract_error (contract, expr, arg_ty, ret_ty, storage_ty) ->
        cctxt.warning
          "@[<v 2>Runtime error in contract %a:@ %a@]"
          Contract.pp contract
          (print_program locations)
          ({ Script.storage_type = unparse_ty storage_ty ;
             arg_type = unparse_ty arg_ty ;
             ret_type = unparse_ty ret_ty ;
             code = expr }, [])
    | Invalid_arity (loc, name, exp, got) ->
        cctxt.warning
          "%aprimitive %s expects %d arguments but is given %d."
          print_loc loc name exp got
    | Invalid_namespace (loc, name, exp, got) ->
        let human_namespace = function
          | Instr_namespace -> ("an", "instruction")
          | Type_namespace -> ("a", "type name")
          | Constant_namespace -> ("a", "constant constructor") in
        cctxt.warning
          "@[%aunexpected %s %s, only@ %s@ %s@ can@ be@ used@ here."
          print_loc loc
          (snd (human_namespace got))
          name
          (fst (human_namespace exp)) (snd (human_namespace exp))
    | Invalid_primitive (loc, exp, got) ->
        cctxt.warning
          "@[%ainvalid primitive %s, only@ %a@ can@ be@ used@ here."
          print_loc loc
          got
          print_enumeration exp
    | Invalid_case (loc, name) ->
        cctxt.warning
          "%a%s is not a valid primitive name."
          print_loc loc
          name
    | Invalid_kind (loc, exp, got) ->
        let human_kind = function
          | Seq_kind -> ("a", "sequence")
          | Prim_kind -> ("a", "primitive")
          | Int_kind -> ("an", "int")
          | String_kind -> ("a", "string") in
        cctxt.warning
          "@[%aunexpected %s, only@ %a@ can@ be@ used@ here."
          print_loc loc
          (snd (human_kind got))
          print_enumeration
          (List.map (fun k -> let (a, n) = human_kind k in a ^ " " ^ n) exp)
    | Duplicate_map_keys (_, expr) ->
        cctxt.warning
          "@[<v 2>Map literals cannot contain duplicate keys, \
           however a duplicate key was found:@ \
           @[%a@]"
          (print_expr no_locations) expr
    | Unordered_map_keys (_, expr) ->
        cctxt.warning
          "@[<v 2>Keys in a map literal must be in strictly ascending order, \
           but they were unordered in literal:@ \
           @[%a@]"
          (print_expr no_locations) expr
    | Duplicate_set_values (_, expr) ->
        cctxt.warning
          "@[<v 2>Set literals cannot contain duplicate values, \
           however a duplicate value was found:@ \
           @[%a@]"
          (print_expr no_locations) expr
    | Unordered_set_values (_, expr) ->
        cctxt.warning
          "@[<v 2>Values in a set literal must be in strictly ascending order, \
           but they were unordered in literal:@ \
           @[%a@]"
          (print_expr no_locations) expr
    | Fail_not_in_tail_position loc ->
        cctxt.warning
          "%aThe FAIL instruction must appear in a tail position."
          print_loc loc
    | Undefined_binop (loc, name, tya, tyb) ->
        cctxt.warning
          "@[<hov 0>@[<hov 2>%aoperator %s is undefined between@ %a@]@ \
           @[<hov 2>and@ %a.@]@]"
          print_loc loc
          name
          print_ty tya
          print_ty tyb
    | Undefined_unop (loc, name, ty) ->
        cctxt.warning
          "@[<hov 0>@[<hov 2>%aoperator %s is undefined on@ %a@]@]"
          print_loc loc
          name
          print_ty ty
    | Bad_return (loc, got, exp) ->
        cctxt.warning
          "@[<v 2>%awrong stack type at end of body:@,\
           - @[<hov>expected return stack type:@ %a,@]@,\
           - @[<hov>actual stack type:@ %a.@]@]"
          print_loc loc
          (fun ppf -> print_stack_ty ppf) (Item_t (exp, Empty_t))
          (fun ppf -> print_stack_ty ppf) got
    | Bad_stack (loc, name, depth, sty) ->
        cctxt.warning
          "@[<hov 2>%awrong stack type for instruction %s:@ %a.@]"
          print_loc loc name (print_stack_ty ~depth) sty
    | Unmatched_branches (loc, sta, stb) ->
        cctxt.warning
          "@[<v 2>%atwo branches don't end with the same stack type:@,\
           - @[<hov>first stack type:@ %a,@]@,\
           - @[<hov>other stack type:@ %a.@]@]"
          print_loc loc
          (fun ppf -> print_stack_ty ppf) sta
          (fun ppf -> print_stack_ty ppf) stb
    | Transfer_in_lambda loc ->
        cctxt.warning
          "%aThe TRANSFER_TOKENS instruction cannot appear in a lambda."
          print_loc loc
    | Transfer_in_dip loc ->
        cctxt.warning
          "%aThe TRANSFER_TOKENS instruction cannot appear within a DIP."
          print_loc loc
    | Bad_stack_length ->
        cctxt.warning
          "Bad stack length."
    | Bad_stack_item lvl ->
        cctxt.warning
          "Bad stack item %d ."
          lvl
    | Invalid_constant (loc, got, exp) ->
        cctxt.warning
          "@[<hov 0>@[<hov 2>%avalue@ %a@]@ \
           @[<hov 2>is invalid for type@ %a.@]@]"
          print_loc loc
          (fun ppf -> print_expr no_locations ppf) got
          print_ty exp
    | Invalid_contract (loc, contract) ->
        cctxt.warning
          "%ainvalid contract %a."
          print_loc loc Contract.pp contract
    | Comparable_type_expected (loc, ty) ->
        cctxt.warning "%acomparable type expected."
          print_loc loc >>= fun () ->
        cctxt.warning "@[<hov 0>@[<hov 2>Type@ %a@]@ is not comparable.@]"
          print_ty ty
    | Inconsistent_types (tya, tyb) ->
        cctxt.warning
          "@[<hov 0>@[<hov 2>Type@ %a@]@ \
           @[<hov 2>is not compatible with type@ %a.@]@]"
          print_ty tya print_ty tyb
    | Reject _ -> cctxt.warning "Script reached FAIL instruction"
    | Overflow _ -> cctxt.warning "Unexpected arithmetic overflow"
    | err ->
        cctxt.warning "%a"
          Environment.Error_monad.pp_print_error [ err ] in
  let rec print_error_trace locations errs =
    let locations = match errs with
      | (Ill_typed_data (_, _, _)
        | Ill_formed_type (_, _)
        | Ill_typed_contract (_, _, _, _, _)
        | Runtime_contract_error (_, _, _, _, _)) :: rest ->
          let collected =
            collect_error_locations rest in
          let assoc, _ =
            List.fold_left
              (fun (acc, i) l ->
                 if List.mem_assoc l acc then
                   (acc, i)
                 else
                   ((l, i) :: acc, i + 1))
              ([], 1) collected in
          (fun l -> try Some (List.assoc l assoc) with Not_found -> None)
      | _ -> locations in
    match errs with
    | [] -> Lwt.return ()
    | err :: errs ->
        print_error locations err >>= fun () ->
        print_error_trace locations errs in
  Lwt_list.iter_s
    (function
      | Ecoproto_error errs ->
          print_error_trace no_locations errs
      | err -> cctxt.warning "%a" pp_print_error [ err ])
    errs

type 'a parsed =
  { ast : 'a ;
    source : string ;
    loc_table : (string * (int * Script_located_ir.location) list) list }

let parse_program source =
  try
    let fields = Michelson_parser.parse_toplevel (Michelson_parser.tokenize source) in
    let fields = List.map Script_located_ir.strip_locations fields in
    let rec get_field n = function
      | (Script.Prim (_, pn, [ ctns ], _), locs) :: _ when n = pn -> ctns, locs
      | _ :: rest -> get_field n rest
      | [] -> raise (Script_located_ir.Missing_program_field n) in
    let code, code_loc_table = get_field "code" fields in
    let arg_type, parameter_loc_table = get_field "parameter" fields in
    let ret_type, return_loc_table = get_field "return" fields in
    let storage_type, storage_loc_table = get_field "storage" fields in
    let ast = Script.{ code ; arg_type ; ret_type ; storage_type } in
    let loc_table =
      [ "code", code_loc_table ;
        "parameter", parameter_loc_table ;
        "return", return_loc_table ;
        "storage", storage_loc_table ] in
    return { ast ; source ; loc_table }
  with
  | exn -> report_parse_error "program" exn

let parse_data source =
  try
    let node = Michelson_parser.parse_expression (Michelson_parser.tokenize source) in
    let ast, loc_table = Script_located_ir.strip_locations node in
    let loc_table = [ "data", loc_table ] in
    return { ast ; source ; loc_table }
  with
  | exn -> report_parse_error "data" exn

let parse_data_type source =
  try
    let node = Michelson_parser.parse_expression (Michelson_parser.tokenize source) in
    let ast, loc_table = Script_located_ir.strip_locations node in
    let loc_table = [ "data", loc_table ] in
    return { ast ; source ; loc_table }
  with
  | exn -> report_parse_error "type" exn

let unexpand_macros type_map (program : Script.code) =
  let open Script in
  let rec first_prim_in_sequence = function
    | Int _ | String _ -> None
    | Prim (loc, _, _, _) -> Some loc
    | Seq (_, children, _) ->
        let rec loop = function
          | [] -> None
          | child :: children ->
              match first_prim_in_sequence child with
              | None -> loop children
              | Some loc -> Some loc in
        loop children in
  let rec last_prim_in_sequence = function
    | Int _ | String _ -> None
    | Prim (loc, _, _, _) -> Some loc
    | Seq (_, children, _) ->
        let rec reversed = function
          | [] -> None
          | child :: children ->
              match last_prim_in_sequence child with
              | None -> reversed children
              | Some loc -> Some loc in
        reversed (List.rev children) in
  let rec unexpand type_map original =
    match Michelson_macros.unexpand original with
    | Seq (loc, children, annot) ->
        let type_map, children =
          List.fold_left
            (fun (type_map, acc) node ->
               let type_map, node = unexpand type_map node in
               type_map, node :: acc)
            (type_map, []) children in
        type_map, Seq (loc, List.rev children, annot)
    | Prim (loc, name, children, annot) ->
        let type_map =
          match original with
          | Seq _ ->
              if List.mem_assoc loc type_map then
                type_map
              else
                begin match first_prim_in_sequence original, last_prim_in_sequence original with
                  | None, _ | _, None -> type_map
                  | Some floc, Some lloc ->
                      let fty, _ = List.assoc floc type_map in
                      let _, lty = List.assoc lloc type_map in
                      (loc, (fty, lty)) :: type_map
                end
          | _ -> type_map in
        let type_map, children =
          List.fold_left
            (fun (type_map, acc) node ->
               let type_map, node = unexpand type_map node in
               type_map, node :: acc)
            (type_map, []) children in
        type_map, Prim (loc, name, List.rev children, annot)
    | oth -> type_map, oth in
  let type_map, code = unexpand type_map program.code in
  type_map, { program with code }

module Program = Client_aliases.Alias (struct
    type t = Script.code parsed
    let encoding =
      let open Data_encoding in
      let loc_table_encoding =
        assoc (list (tup2 uint16 Script_located_ir.location_encoding)) in
      conv
        (fun { ast ; source ; loc_table } -> (ast, source, loc_table))
        (fun (ast, source, loc_table) -> { ast ; source ; loc_table })
        (obj3
           (req "ast" Script.code_encoding)
           (req "source" string)
           (req "loc_table" loc_table_encoding))
    let of_source _cctxt s = parse_program s
    let to_source _ { source } = return source
    let name = "program"
  end)

let group =
  { Cli_entries.name = "programs" ;
    title = "Commands for managing the record of known programs" }

let commands () =
  let open Cli_entries in
  let show_types_switch =
    switch
      ~parameter:"-details"
      ~doc:"Show the types of each instruction" in
  let emacs_mode_switch =
    switch
      ~parameter:"-emacs"
      ~doc:"Output in michelson-mode.el compatible format" in
  let trace_stack_switch =
    switch
      ~parameter:"-trace-stack"
      ~doc:"Show the stack after each step" in
  let amount_arg =
    Client_proto_args.tez_arg
      ~parameter:"-amount"
      ~doc:"The amount of the transfer in \xEA\x9C\xA9."
      ~default:"0.05" in
  [

    command ~group ~desc: "lists all known programs"
      no_options
      (fixed [ "list" ; "known" ; "programs" ])
      (fun () cctxt ->
         Program.load cctxt >>=? fun list ->
         Lwt_list.iter_s (fun (n, _) -> cctxt.message "%s" n) list >>= fun () ->
         return ()) ;

    command ~group ~desc: "remember a program under some name"
      no_options
      (prefixes [ "remember" ; "program" ]
       @@ Program.fresh_alias_param
       @@ Program.source_param
       @@ stop)
      (fun () name hash cctxt -> Program.add cctxt name hash) ;

    command ~group ~desc: "forget a remembered program"
      no_options
      (prefixes [ "forget" ; "program" ]
       @@ Program.alias_param
       @@ stop)
      (fun () (name, _) cctxt -> Program.del cctxt name) ;

    command ~group ~desc: "display a program"
      no_options
      (prefixes [ "show" ; "known" ; "program" ]
       @@ Program.alias_param
       @@ stop)
      (fun () (_, program) cctxt ->
         Program.to_source cctxt program >>=? fun source ->
         cctxt.message "%s\n" source >>= fun () ->
         return ()) ;

    command ~group ~desc: "ask the node to run a program"
      (args2 trace_stack_switch amount_arg)
      (prefixes [ "run" ; "program" ]
       @@ Program.source_param
       @@ prefixes [ "on" ; "storage" ]
       @@ Cli_entries.param ~name:"storage" ~desc:"the storage data"
         (fun _cctxt data -> parse_data data)
       @@ prefixes [ "and" ; "input" ]
       @@ Cli_entries.param ~name:"storage" ~desc:"the input data"
         (fun _cctxt data -> parse_data data)
       @@ stop)
      (fun (trace_stack, amount) program storage input cctxt ->
         let open Data_encoding in
         let print_errors errs =
           report_errors cctxt errs >>= fun () ->
           cctxt.error "error running program" >>= fun () ->
           return () in
         if trace_stack then
           Client_proto_rpcs.Helpers.trace_code cctxt.rpc_config
             cctxt.config.block program.ast (storage.ast, input.ast, amount) >>= function
           | Ok (storage, output, trace) ->
               cctxt.message
                 "@[<v 0>@[<v 2>storage@,%a@]@,\
                  @[<v 2>output@,%a@]@,@[<v 2>trace@,%a@]@]@."
                 (print_expr no_locations) storage
                 (print_expr no_locations) output
                 (Format.pp_print_list
                    (fun ppf (loc, gas, stack) ->
                       Format.fprintf ppf
                         "- @[<v 0>location: %d (remaining gas: %d)@,\
                          [ @[<v 0>%a ]@]@]"
                         loc gas
                         (Format.pp_print_list (print_expr no_locations))
                         stack))
                 trace >>= fun () ->
               return ()
           | Error errs -> print_errors errs
         else
           Client_proto_rpcs.Helpers.run_code cctxt.rpc_config
             cctxt.config.block program.ast (storage.ast, input.ast, amount) >>= function
           | Ok (storage, output) ->
               cctxt.message "@[<v 0>@[<v 2>storage@,%a@]@,@[<v 2>output@,%a@]@]@."
                 (print_expr no_locations) storage
                 (print_expr no_locations) output >>= fun () ->
               return ()
           | Error errs ->
               print_errors errs);

    command ~group ~desc: "ask the node to typecheck a program"
      (args2 show_types_switch emacs_mode_switch)
      (prefixes [ "typecheck" ; "program" ]
       @@ Program.source_param
       @@ stop)
      (fun (show_types, emacs_mode) program cctxt ->
         let open Data_encoding in
         Client_proto_rpcs.Helpers.typecheck_code
           cctxt.rpc_config cctxt.config.block program.ast >>= fun res ->
         if emacs_mode then
           let emacs_type_map type_map =
             (Utils.filter_map
                (fun (n, loc) ->
                   try
                     let bef, aft = List.assoc n type_map in
                     Some (loc, bef, aft)
                   with
                     Not_found -> None)
                (List.assoc "code" program.loc_table),
              []) in
           begin match res with
             | Ok type_map ->
                 Lwt.return (emacs_type_map type_map)
             | Error errs ->
                 let msg = Buffer.create 5000 in
                 let cctxt = Client_commands.make_context
                     (fun _ t -> Buffer.add_string msg t ; Buffer.add_char msg '\n' ; Lwt.return ()) in
                 match errs with
                 | Ecoproto_error (Script_ir_translator.Ill_formed_type
                                     (Some ("return" | "parameter" | "storage" as field), _) :: errs) :: _ ->
                     report_errors cctxt [ Ecoproto_error errs ] >>= fun () ->
                     Lwt.return ([], [ List.assoc 0 (List.assoc field program.loc_table), Buffer.contents msg ])
                 | Ecoproto_error (Script_ir_translator.Ill_typed_contract (_, _, _, _, type_map) :: errs) :: _ ->
                     (report_errors cctxt [ Ecoproto_error errs ]  >>= fun () ->
                      let (types, _) = emacs_type_map type_map in
                      let loc = match collect_error_locations errs with
                        | hd :: _ -> hd
                        | [] -> 0 in
                      Lwt.return (types, [ List.assoc loc (List.assoc "code" program.loc_table), Buffer.contents msg ]))
                 | _ -> Lwt.return ([], [])
           end >>= fun (types, errors) ->
           cctxt.message
             "((types . (%a)) (errors . (%a)))"
             (Format.pp_print_list
                (fun ppf ({ Script_located_ir.start = { point = s } ; stop = { point = e } },
                          bef, aft) ->
                  Format.fprintf ppf "(%d %d %a %a)" (s + 1) (e + 1)
                    print_emacs_stack bef print_emacs_stack aft))
             types
             (Format.pp_print_list
                (fun ppf ({ Script_located_ir.start = { point = s } ; stop = { point = e } },
                          err) ->
                  Format.fprintf ppf "(%d %d %S)" (s + 1) (e + 1) err))
             errors >>= fun () ->
           return ()
         else
           match res with
           | Ok type_map ->
               let type_map, program = unexpand_macros type_map program.ast in
               cctxt.message "Well typed" >>= fun () ->
               if show_types then
                 cctxt.message "%a" (print_program no_locations) (program, type_map) >>= fun () ->
                 return ()
               else return ()
           | Error errs ->
               report_errors cctxt errs >>= fun () ->
               cctxt.error "ill-typed program") ;

    command ~group ~desc: "ask the node to typecheck a data expression"
      no_options
      (prefixes [ "typecheck" ; "data" ]
       @@ Cli_entries.param ~name:"data" ~desc:"the data to typecheck"
         (fun _cctxt data -> parse_data data)
       @@ prefixes [ "against" ; "type" ]
       @@ Cli_entries.param ~name:"type" ~desc:"the expected type"
         (fun _cctxt data -> parse_data data)
       @@ stop)
      (fun () data exp_ty cctxt ->
         let open Data_encoding in
         Client_proto_rpcs.Helpers.typecheck_data cctxt.Client_commands.rpc_config
           cctxt.config.block (data.ast, exp_ty.ast) >>= function
         | Ok () ->
             cctxt.message "Well typed" >>= fun () ->
             return ()
         | Error errs ->
             report_errors cctxt errs >>= fun () ->
             cctxt.error "ill-typed data") ;

    command ~group
      ~desc: "ask the node to compute the hash of a data expression \
              using the same algorithm as script instruction H"
      no_options
      (prefixes [ "hash" ; "data" ]
       @@ Cli_entries.param ~name:"data" ~desc:"the data to hash"
         (fun _cctxt data -> parse_data data)
       @@ stop)
      (fun () data cctxt ->
         let open Data_encoding in
         Client_proto_rpcs.Helpers.hash_data cctxt.Client_commands.rpc_config
           cctxt.config.block (data.ast) >>= function
         | Ok hash ->
             cctxt.message "%S" hash >>= fun () ->
             return ()
         | Error errs ->
             cctxt.warning "%a" pp_print_error errs  >>= fun () ->
             cctxt.error "ill-formed data") ;

    command ~group
      ~desc: "ask the node to compute the hash of a data expression \
              using the same algorithm as script instruction H, sign it using \
              a given secret key, and display it using the format expected by \
              script instruction CHECK_SIGNATURE"
      no_options
      (prefixes [ "hash" ; "and" ; "sign" ; "data" ]
       @@ Cli_entries.param ~name:"data" ~desc:"the data to hash"
         (fun _cctxt data -> parse_data data)
       @@ prefixes [ "for" ]
       @@ Client_keys.Secret_key.alias_param
       @@ stop)
      (fun () data (_, key) cctxt ->
         let open Data_encoding in
         Client_proto_rpcs.Helpers.hash_data cctxt.rpc_config
           cctxt.config.block (data.ast) >>= function
         | Ok hash ->
             let signature = Ed25519.sign key (MBytes.of_string hash) in
             cctxt.message "Hash: %S@.Signature: %S"
               hash
               (signature |>
                Data_encoding.Binary.to_bytes Ed25519.Signature.encoding |>
                Hex_encode.hex_of_bytes) >>= fun () ->
             return ()
         | Error errs ->
             cctxt.warning "%a" pp_print_error errs >>= fun () ->
             cctxt.error "ill-formed data") ;

  ]
