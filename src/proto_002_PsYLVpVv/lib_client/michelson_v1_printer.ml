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
open Alpha_context
open Tezos_micheline
open Micheline
open Micheline_printer

let anon = { comment = None }

let print_expr ppf expr =
  expr
  |> Michelson_v1_primitives.strings_of_prims
  |> Micheline.inject_locations (fun _ -> anon)
  |> print_expr ppf

let print_expr_unwrapped ppf expr =
  expr
  |> Michelson_v1_primitives.strings_of_prims
  |> Micheline.inject_locations (fun _ -> anon)
  |> print_expr_unwrapped ppf

let print_var_annots ppf =
  List.iter (Format.fprintf ppf "%s ")

let print_annot_expr_unwrapped ppf (expr, annot) =
  Format.fprintf ppf "%a%a"
    print_var_annots annot
    print_expr_unwrapped expr

let print_stack ppf = function
  | [] -> Format.fprintf ppf "[]"
  | more ->
      Format.fprintf ppf "@[<hov 0>[ %a ]@]"
        (Format.pp_print_list
           ~pp_sep: (fun ppf () -> Format.fprintf ppf "@ : ")
           print_annot_expr_unwrapped)
        more

let print_execution_trace ppf trace =
  Format.pp_print_list
    (fun ppf (loc, gas, stack) ->
       Format.fprintf ppf
         "- @[<v 0>location: %d (remaining gas: %a)@,\
          [ @[<v 0>%a ]@]@]"
         loc Gas.pp gas
         (Format.pp_print_list
            (fun ppf (e, annot) ->
               Format.fprintf ppf
                 "@[<v 0>%a  \t%s@]"
                 print_expr e
                 (match annot with None -> "" | Some a -> a)
            ))
         stack)
    ppf
    trace

let inject_types type_map parsed =
  let rec inject_expr = function
    | Seq (loc, items) ->
        Seq (inject_loc `before loc, List.map inject_expr items)
    | Prim (loc, name, items, annot) ->
        Prim (inject_loc `after loc, name, List.map inject_expr items, annot)
    | Int (loc, value) ->
        Int (inject_loc `after loc, value)
    | String (loc, value) ->
        String (inject_loc `after loc, value)
    | Bytes (loc, value) ->
        Bytes (inject_loc `after loc, value)
  and inject_loc which loc = try
      let stack =
        let locs =
          List.assoc loc parsed.Michelson_v1_parser.expansion_table
          |> snd
          |> List.sort compare in
        let (bef, aft) =
          List.assoc (List.hd locs) type_map in
        match which with
        | `before -> bef
        | `after -> aft in
      { comment = Some (Format.asprintf "%a" print_stack stack) }
    with Not_found -> { comment = None } in
  inject_expr (root parsed.unexpanded)

let unparse ?type_map parse expanded =
  let source =
    match type_map with
    | Some type_map ->
        let unexpanded, unexpansion_table =
          expanded
          |> Michelson_v1_primitives.strings_of_prims
          |> root |> Michelson_v1_macros.unexpand_rec |> Micheline.extract_locations in
        let rec inject_expr = function
          | Seq (loc, items) ->
              Seq (inject_loc `before loc, List.map inject_expr items)
          | Prim (loc, name, items, annot) ->
              Prim (inject_loc `after loc, name, List.map inject_expr items, annot)
          | Int (loc, value) ->
              Int (inject_loc `after loc, value)
          | String (loc, value) ->
              String (inject_loc `after loc, value)
          | Bytes (loc, value) ->
              Bytes (inject_loc `after loc, value)
        and inject_loc which loc = try
            let stack =
              let (bef, aft) =
                List.assoc (List.assoc loc unexpansion_table) type_map in
              match which with
              | `before -> bef
              | `after -> aft in
            { comment = Some (Format.asprintf "%a" print_stack stack) }
          with Not_found -> { comment = None } in
        unexpanded |> root |> inject_expr
        |> Format.asprintf "%a" Micheline_printer.print_expr
    | None ->
        expanded |> Michelson_v1_primitives.strings_of_prims
        |> root |> Michelson_v1_macros.unexpand_rec |> Micheline.strip_locations
        |> Micheline_printer.printable (fun n -> n)
        |> Format.asprintf "%a" Micheline_printer.print_expr in
  match parse source with
  | res, [] -> res
  | _, _ :: _ -> Pervasives.failwith "Michelson_v1_printer.unparse"

let unparse_toplevel ?type_map = unparse ?type_map Michelson_v1_parser.parse_toplevel
let unparse_expression = unparse Michelson_v1_parser.parse_expression

let unparse_invalid expanded =
  let source =
    expanded
    |> root |> Michelson_v1_macros.unexpand_rec |> Micheline.strip_locations
    |> Micheline_printer.printable (fun n -> n)
    |> Format.asprintf "%a" Micheline_printer.print_expr_unwrapped in
  fst (Michelson_v1_parser.parse_toplevel source)
