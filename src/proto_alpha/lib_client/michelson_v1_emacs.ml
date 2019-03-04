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

open Proto_alpha
open Tezos_micheline
open Micheline

let print_expr ppf expr =
  let print_annot ppf = function
    | [] -> ()
    | annots -> Format.fprintf ppf " %s" (String.concat " " annots) in
  let rec print_expr ppf = function
    | Int (_, value) -> Format.fprintf ppf "%s" (Z.to_string value)
    | String (_, value) -> Micheline_printer.print_string ppf value
    | Bytes (_, value) -> Format.fprintf ppf "0x%a" MBytes.pp_hex value
    | Seq (_, items) ->
        Format.fprintf ppf "(seq %a)"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space print_expr)
          items
    | Prim (_, name, [], []) ->
        Format.fprintf ppf "%s" name
    | Prim (_, name, items, annot) ->
        Format.fprintf ppf "(%s%a%s%a)"
          name
          print_annot annot
          (if items = [] then "" else " ")
          (Format.pp_print_list ~pp_sep:Format.pp_print_space print_expr) items in
  let root = root (Michelson_v1_primitives.strings_of_prims expr) in
  Format.fprintf ppf "@[<h>%a@]" print_expr root

let print_var_annots ppf =
  List.iter (Format.fprintf ppf "%s ")

let print_annot_expr ppf (expr, annot) =
  Format.fprintf ppf "(%a%a)"
    print_var_annots annot
    print_expr expr

open Micheline_parser
open Script_tc_errors

let print_type_map ppf (parsed, type_map) =
  let rec print_expr_types ppf = function
    | Seq (loc, [])
    | Prim (loc, _, [], _)
    | Int (loc, _)
    | Bytes (loc, _)
    | String (loc, _) ->
        print_item ppf loc
    | Seq (loc, items)
    | Prim (loc, _, items, _) ->
        print_item ppf loc ;
        List.iter (print_expr_types ppf) items
  and print_stack ppf items =
    Format.fprintf ppf "(%a)"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space print_annot_expr)
      items
  and print_item ppf loc = try
      let { start = { point = s ; _ } ; stop = { point = e ; _ } }, locs =
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
    | (Inconsistent_type_annotations (loc, _, _)
      | Unexpected_annotation loc
      | Ill_formed_type (_, _, loc)
      | Invalid_arity (loc, _, _, _)
      | Invalid_namespace (loc, _, _, _)
      | Invalid_primitive (loc, _, _)
      | Invalid_kind (loc, _, _)
      | Fail_not_in_tail_position loc
      | Undefined_binop (loc, _, _, _)
      | Undefined_unop (loc, _, _)
      | Bad_return (loc, _, _)
      | Bad_stack (loc, _, _, _)
      | Unmatched_branches (loc, _, _)
      | Invalid_constant (loc, _, _)
      | Invalid_contract (loc, _)
      | Comparable_type_expected (loc, _)
      | Michelson_v1_primitives.Invalid_primitive_name (_, loc)) :: _ -> loc
    | _ :: rest -> find rest in
  find errs

let report_errors ppf (parsed, errs) =
  let eco, out =
    List.fold_left
      (fun (eco, out) -> function
         | Alpha_environment.Ecoproto_error err -> (err :: eco, out)
         | err -> (eco, err :: out))
      ([], []) errs in
  let eco, out = List.rev eco, List.rev out in
  Format.fprintf ppf "(@[<v 0>%a@,%a@])"
    (fun ppf errs ->
       let find_location loc =
         let oloc = List.assoc loc parsed.Michelson_v1_parser.unexpansion_table in
         fst (List.assoc oloc parsed.expansion_table) in
       match errs with
       | top :: errs ->
           let errs, loc =
             List.map
               (fun e -> Alpha_environment.Ecoproto_error e)
               (top :: errs),
             match top with
             | Ill_typed_contract (expr, _)
             | Ill_typed_data (_, expr, _) ->
                 if expr = parsed.expanded then
                   find_location
                     (first_error_location
                        (top :: errs))
                 else find_location 0
             | Michelson_v1_primitives.Invalid_primitive_name (expr, loc) ->
                 if Micheline.strip_locations (Michelson_v1_macros.unexpand_rec (Micheline.root expr)) =
                    parsed.Michelson_v1_parser.unexpanded then
                   find_location loc
                 else
                   find_location 0
             | _ -> find_location 0
           in
           let message =
             Format.asprintf "%a"
               (Michelson_v1_error_reporter.report_errors
                  ~details:false ~show_source:false ~parsed)
               errs in
           let { start = { point = s ; _ } ; stop = { point = e ; _ } } = loc in
           Format.fprintf ppf "(%d %d %S)" (s + 1) (e + 1) message
       | [] -> ())
    eco
    (Format.pp_print_list
       (fun ppf err ->
          let find_location loc =
            let oloc = List.assoc loc parsed.Michelson_v1_parser.unexpansion_table in
            fst (List.assoc oloc parsed.expansion_table) in
          let loc =
            match err with
            | Invalid_utf8_sequence (point, _)
            | Unexpected_character (point, _)
            | Undefined_escape_sequence (point, _)
            | Missing_break_after_number point ->
                { start = point ; stop = point }
            | Unterminated_string loc
            | Unterminated_integer loc
            | Unterminated_comment loc
            | Odd_lengthed_bytes loc
            | Unclosed { loc ; _ }
            | Unexpected { loc ; _ }
            | Extra { loc ; _ } -> loc
            | Misaligned node -> location node
            | _ -> find_location 0 in
          let message =
            Format.asprintf "%a"
              (Michelson_v1_error_reporter.report_errors
                 ~details:false ~show_source:false ~parsed)
              [ err ] in
          let { start = { point = s ; _ } ; stop = { point = e ; _ } } = loc in
          Format.fprintf ppf "(%d %d %S)" (s + 1) (e + 1) message))
    out
