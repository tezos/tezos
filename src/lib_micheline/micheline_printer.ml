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

open Micheline

type location = { comment : string option }

type node = (location, string) Micheline.node

let printable
    ?(comment = (fun _ -> None))
    map_prim expr =
  let map_loc loc =
    { comment = comment loc } in
  map_node map_loc map_prim (root expr)

let print_comment ppf text =
  Format.fprintf ppf "/* @[<h>%a@] */" Format.pp_print_text text

let print_string ppf text =
  Format.fprintf ppf "\"" ;
  String.iter (function
      | '"' -> Format.fprintf ppf "\\\""
      | '\n' -> Format.fprintf ppf "\\n"
      | '\r' -> Format.fprintf ppf "\\r"
      | '\b' -> Format.fprintf ppf "\\b"
      | '\t' -> Format.fprintf ppf "\\t"
      | '\\' -> Format.fprintf ppf "\\\\"
      | c -> Format.fprintf ppf "%c" c)
    text ;
  Format.fprintf ppf "\""

let print_annotations =
  Format.pp_print_list ~pp_sep:Format.pp_print_space Format.pp_print_string

let preformat root =
  let preformat_loc = function
    | { comment = None } ->
        (false, 0)
    | { comment = Some text } ->
        (String.contains text '\n', String.length text + 1) in
  let preformat_annots = function
    | [] -> 0
    | annots -> String.length (String.concat " " annots) + 2 in
  let rec preformat_expr = function
    | Int (loc, value) ->
        let cml, csz = preformat_loc loc in
        Int ((cml, String.length (Z.to_string value) + csz, loc), value)
    | String (loc, value) ->
        let cml, csz = preformat_loc loc in
        String ((cml, String.length value + csz, loc), value)
    | Bytes (loc, value) ->
        let cml, csz = preformat_loc loc in
        Bytes ((cml, MBytes.length value * 2 + 2 + csz, loc), value)
    | Prim (loc, name, items, annots) ->
        let cml, csz = preformat_loc loc in
        let asz = preformat_annots annots in
        let items = List.map preformat_expr items in
        let ml, sz =
          List.fold_left
            (fun (tml, tsz) e ->
               let (ml, sz, _) = location e in
               (tml || ml, tsz + 1 + sz))
            (cml, String.length name + csz + asz)
            items in
        Prim ((ml, sz, loc), name, items, annots)
    | Seq (loc, items) ->
        let cml, csz = preformat_loc loc in
        let items = List.map preformat_expr items in
        let ml, sz =
          List.fold_left
            (fun (tml, tsz) e ->
               let (ml, sz, _) = location e in
               (tml || ml, tsz + 3 + sz))
            (cml, 4 + csz)
            items in
        Seq ((ml, sz, loc), items) in
  preformat_expr root

let rec print_expr_unwrapped ppf = function
  | Prim ((ml, s, { comment }), name, args, annot) ->
      let name = match annot with
        | [] -> name
        | annots ->
            Format.asprintf "%s @[<h>%a@]" name print_annotations annots in
      if not ml && s < 80 then begin
        if args = [] then
          Format.fprintf ppf "%s" name
        else
          Format.fprintf ppf "@[<h>%s %a@]" name (Format.pp_print_list ~pp_sep:Format.pp_print_space print_expr) args ;
        begin match comment with
          | None -> ()
          | Some text -> Format.fprintf ppf "@ /* %s */" text
        end ;
      end else begin
        if args = [] then
          Format.fprintf ppf "%s" name
        else if String.length name <= 4 then
          Format.fprintf ppf "%s @[<v 0>%a@]" name (Format.pp_print_list print_expr) args
        else
          Format.fprintf ppf "@[<v 2>%s@,%a@]" name (Format.pp_print_list print_expr) args ;
        begin match comment with
          | None -> ()
          | Some comment -> Format.fprintf ppf "@ %a" print_comment comment
        end
      end
  | Int ((_, _, { comment }), value) ->
      begin match comment with
        | None -> Format.fprintf ppf "%s" (Z.to_string value)
        | Some comment -> Format.fprintf ppf "%s@ %a" (Z.to_string value) print_comment comment
      end
  | String ((_, _, { comment }), value) ->
      begin match comment with
        | None -> print_string ppf value
        | Some comment -> Format.fprintf ppf "%a@ %a" print_string value print_comment comment
      end
  | Bytes ((_, _, { comment }), value) ->
      begin match comment with
        | None -> Format.fprintf ppf "0x%a" MBytes.pp_hex value
        | Some comment -> Format.fprintf ppf "0x%a@ %a" MBytes.pp_hex value print_comment comment
      end
  | Seq ((_, _, { comment = None }), []) ->
      Format.fprintf ppf "{}"
  | Seq ((ml, s, { comment }), items) ->
      if not ml && s < 80 then
        Format.fprintf ppf "{ @[<h 0>"
      else
        Format.fprintf ppf "{ @[<v 0>" ;
      begin match comment, items with
        | None, _ -> ()
        | Some comment, [] -> Format.fprintf ppf "%a" print_comment comment
        | Some comment, _ -> Format.fprintf ppf "%a@ " print_comment comment
      end ;
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf " ;@ ")
        print_expr_unwrapped
        ppf items ;
      Format.fprintf ppf "@] }"

and print_expr ppf = function
  | Prim (_, _, _ :: _, _)
  | Prim (_, _, [], _ :: _) as expr ->
      Format.fprintf ppf "(%a)" print_expr_unwrapped expr
  | expr -> print_expr_unwrapped ppf expr

let with_unbounded_formatter ppf f x =
  let buf = Buffer.create 10000 in
  let sppf = Format.formatter_of_buffer buf in
  Format.pp_set_margin sppf 199999 ;
  Format.pp_set_max_indent sppf 99999 ;
  Format.pp_set_max_boxes sppf 99999 ;
  f sppf x ;
  Format.fprintf sppf "%!" ;
  let lines = String.split_on_char '\n' (Buffer.contents buf) in
  Format.pp_print_list ~pp_sep:Format.pp_force_newline Format.pp_print_string ppf lines

let print_expr_unwrapped ppf expr =
  with_unbounded_formatter ppf print_expr_unwrapped (preformat expr)

let print_expr ppf expr =
  with_unbounded_formatter ppf print_expr (preformat expr)
