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
open Micheline_parser
open Micheline

type parsed =
  { source : string ;
    unexpanded : string canonical ;
    expanded : Michelson_v1_primitives.prim canonical ;
    expansion_table : (int * (Micheline_parser.location * int list)) list ;
    unexpansion_table : (int * int) list }

(* Unexpanded toplevel expression should be a sequence *)
let expand_all source ast errors =
  let unexpanded, loc_table =
    extract_locations ast in
  let expanded, expansion_errors =
    Michelson_v1_macros.expand_rec (root unexpanded) in
  let expanded, unexpansion_table =
    extract_locations expanded in
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
  match Alpha_environment.wrap_error (Michelson_v1_primitives.prims_of_strings expanded) with
  | Ok expanded ->
      { source ; unexpanded ; expanded ;
        expansion_table ; unexpansion_table },
      errors @ expansion_errors
  | Error errs ->
      { source ; unexpanded ;
        expanded = Micheline.strip_locations (Seq ((), [])) ;
        expansion_table ; unexpansion_table },
      errors @ expansion_errors @ errs

let parse_toplevel ?check source =
  let tokens, lexing_errors = Micheline_parser.tokenize source in
  let asts, parsing_errors = Micheline_parser.parse_toplevel ?check tokens in
  let ast =
    let start = min_point asts and stop = max_point asts in
    Seq ({ start ; stop }, asts) in
  expand_all source ast (lexing_errors @ parsing_errors)

let parse_expression ?check source =
  let tokens, lexing_errors = Micheline_parser.tokenize source in
  let ast, parsing_errors = Micheline_parser.parse_expression ?check tokens in
  expand_all source ast (lexing_errors @ parsing_errors)

let expand_all ~source ~original =
  expand_all source original []
