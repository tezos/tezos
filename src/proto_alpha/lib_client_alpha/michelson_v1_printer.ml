(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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

let print_stack ppf = function
  | [] -> Format.fprintf ppf "[]"
  | more ->
      Format.fprintf ppf "@[<hov 0>[ %a ]@]"
        (Format.pp_print_list
           ~pp_sep: (fun ppf () -> Format.fprintf ppf "@ : ")
           print_expr_unwrapped)
        more

let inject_types type_map parsed =
  let rec inject_expr = function
    | Seq (loc, items, annot) ->
        Seq (inject_loc `before loc, List.map inject_expr items, annot)
    | Prim (loc, name, items, annot) ->
        Prim (inject_loc `after loc, name, List.map inject_expr items, annot)
    | Int (loc, value) ->
        Int (inject_loc `after loc, value)
    | String (loc, value) ->
        String (inject_loc `after loc, value)
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
  let rec unexpand expr =
    match Michelson_macros.unexpand expr with
    | Seq (loc, items, annot) ->
        Seq (loc, List.map unexpand items, annot)
    | Prim (loc, name, args, annot) ->
        Prim (loc, name, List.map unexpand args, annot)
    | Int _ | String _ as atom -> atom in
  let source =
    match type_map with
    | Some type_map ->
        let unexpanded, unexpansion_table =
          expanded
          |> Michelson_v1_primitives.strings_of_prims
          |> root |> unexpand |> Micheline.extract_locations in
        let rec inject_expr = function
          | Seq (loc, items, annot) ->
              Seq (inject_loc `before loc, List.map inject_expr items, annot)
          | Prim (loc, name, items, annot) ->
              Prim (inject_loc `after loc, name, List.map inject_expr items, annot)
          | Int (loc, value) ->
              Int (inject_loc `after loc, value)
          | String (loc, value) ->
              String (inject_loc `after loc, value)
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
        |> root |> unexpand |> Micheline.strip_locations
        |> Micheline_printer.printable (fun n -> n)
        |> Format.asprintf "%a" Micheline_printer.print_expr in
  match parse source with
  | res, [] -> res
  | _, _ :: _ -> Pervasives.failwith "Michelson_v1_printer.unexpand"

let unparse_toplevel ?type_map = unparse ?type_map Michelson_v1_parser.parse_toplevel
let unparse_expression = unparse Michelson_v1_parser.parse_expression
