(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Micheline_parser
open Micheline

type parsed =
  { source : string ;
    unexpanded : string canonical ;
    expanded : Michelson_v1_primitives.prim canonical ;
    expansion_table : (int * (Micheline_parser.location * int list)) list ;
    unexpansion_table : (int * int) list }

let expand_all source ast =
  let unexpanded, loc_table =
    extract_locations ast in
  let rec expand expr =
    match Michelson_macros.expand expr with
    | Seq (loc, items, annot) ->
        Seq (loc, List.map expand items, annot)
    | Prim (loc, name, args, annot) ->
        Prim (loc, name, List.map expand args, annot)
    | Int _ | String _ as atom -> atom in
  let expanded, unexpansion_table =
    extract_locations (expand (root unexpanded)) in
  let expansion_table =
    let sorted =
      List.sort (fun (_, a) (_, b) -> compare a b) unexpansion_table in
    let grouped =
      let rec group = function
        | acc, [] -> acc
        | [], (u, e) :: r ->
            group ([ (e, [ u ]) ], r)
        | ((pe, us) :: racc as acc), (u, e) :: r ->
            if e = pe then
              group (((e, u :: us) :: racc), r)
            else
              group (((e, [ u ]) :: acc), r) in
      group ([], sorted) in
    List.map2
      (fun (l, ploc) (l', elocs) ->
         assert (l = l') ;
         (l, (ploc, elocs)))
      (List.sort compare loc_table)
      (List.sort compare grouped) in
  Environment.wrap_error (Michelson_v1_primitives.prims_of_strings expanded) >>? fun expanded ->
  ok { source ; unexpanded ; expanded ; expansion_table ; unexpansion_table }

let parse_toplevel ?check source =
  Micheline_parser.tokenize source >>? fun tokens ->
  Micheline_parser.parse_toplevel ?check tokens >>? fun asts ->
  let ast = match asts with
    | [ ast ] -> ast
    | asts ->
        let start = min_point asts and stop = max_point asts in
        Seq (Michelson_macros.{ start ; stop }, asts, None) in
  expand_all source ast

let parse_expression ?check source =
  Micheline_parser.tokenize source >>? fun tokens ->
  Micheline_parser.parse_expression ?check tokens >>? fun ast ->
  expand_all source ast
