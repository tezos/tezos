(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Micheline

let print_expr ppf expr =
  let rec print_expr ppf = function
    | Int (_, value) -> Format.fprintf ppf "%s" value
    | String (_, value) -> Micheline_printer.print_string ppf value
    | Seq (_, items, _) ->
        Format.fprintf ppf "(seq %a)"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space print_expr)
          items
    | Prim (_, name, [], _) ->
        Format.fprintf ppf "%s" name
    | Prim (_, name, items, _) ->
        Format.fprintf ppf "(%s %a)" name
          (Format.pp_print_list ~pp_sep:Format.pp_print_space print_expr) items in
  let root = root (Michelson_v1_primitives.strings_of_prims expr) in
  Format.fprintf ppf "@[<h>%a@]" print_expr root

open Micheline_parser
open Script_ir_translator

let print_type_map ppf (parsed, type_map) =
  let rec print_expr_types ppf = function
    | Seq (loc, [], _)
    | Prim (loc, _, [], _)
    | Int (loc, _)
    | String (loc, _) ->
        print_item ppf loc
    | Seq (loc, items, _)
    | Prim (loc, _, items, _) ->
        print_item ppf loc ;
        List.iter (print_expr_types ppf) items
  and print_stack ppf items =
    Format.fprintf ppf "(%a)"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space print_expr)
      items
  and print_item ppf loc = try
      let { start = { point = s } ; stop = { point = e } }, locs =
        List.assoc loc parsed.Michelson_v1_parser.expansion_table in
      let locs = List.sort compare locs in
      let (bef, aft) = List.assoc (List.hd locs) type_map in
      Format.fprintf ppf "(@[<h>%d %d %a %a@])@,"
        s e
        print_stack bef
        print_stack aft
    with Not_found -> () in
  Format.fprintf ppf "(@[<v 0>%a@])"
    print_expr_types (root parsed.unexpanded)

let first_error_location errs =
  let rec find = function
    | [] -> 0
    | Ill_formed_type (_, _, loc) :: _
    | Invalid_arity (loc, _, _, _) :: _
    | Invalid_namespace (loc, _, _, _) :: _
    | Invalid_primitive (loc, _, _) :: _
    | Invalid_kind (loc, _, _) :: _
    | Fail_not_in_tail_position loc :: _
    | Undefined_binop (loc, _, _, _) :: _
    | Undefined_unop (loc, _, _) :: _
    | Bad_return (loc, _, _) :: _
    | Bad_stack (loc, _, _, _) :: _
    | Unmatched_branches (loc, _, _) :: _
    | Transfer_in_lambda loc :: _
    | Transfer_in_dip loc :: _
    | Invalid_constant (loc, _, _) :: _
    | Invalid_contract (loc, _) :: _
    | Comparable_type_expected (loc, _) :: _ -> loc
    | _ :: rest -> find rest in
  find errs

let report_errors ppf (parsed, errs) =
  Format.fprintf ppf "(@[<v 0>%a@])"
    (Format.pp_print_list
       (fun ppf err ->
          let errs, loc =
            match err with
            | Environment.Ecoproto_error (top :: errs) ->
                [ Environment.Ecoproto_error (top :: errs) ],
                begin match top with
                  | Ill_typed_contract (expr, _)
                  | Ill_typed_data (_, expr, _) ->
                      if expr = parsed.Michelson_v1_parser.expanded then
                        first_error_location (top :: errs)
                      else 0
                  | _ -> 0
                end
            | err -> [ err ], 0 in
          let message =
            Format.asprintf "%a"
              (Michelson_v1_error_reporter.report_errors
                 ~details: false ~show_source: false ~parsed)
              errs in
          let { start = { point = s } ; stop = { point = e } } =
            let oloc = List.assoc loc parsed.unexpansion_table in
            fst (List.assoc oloc parsed.expansion_table) in
          Format.fprintf ppf "(%d %d %S)" (s + 1) (e + 1) message))
    errs
