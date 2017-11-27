(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
      | '"' | 'r' | 'n' | 't' | 'b' | '\\' as c ->
          Format.fprintf ppf "%c" c
      | '\x20'..'\x7E' as c ->
          Format.fprintf ppf "%c" c
      | c ->
          Format.fprintf ppf "\\x%02X" (Char.code c))
    text ;
  Format.fprintf ppf "\""

let preformat root =
  let preformat_loc = function
    | { comment = None } ->
        (false, 0)
    | { comment = Some text } ->
        (String.contains text '\n', String.length text + 1) in
  let preformat_annot = function
    | None -> 0
    | Some annot -> String.length annot + 2 in
  let rec preformat_expr = function
    | Int (loc, value) ->
        let cml, csz = preformat_loc loc in
        Int ((cml, String.length value + csz, loc), value)
    | String (loc, value) ->
        let cml, csz = preformat_loc loc in
        String ((cml, String.length value + csz, loc), value)
    | Prim (loc, name, items, annot) ->
        let cml, csz = preformat_loc loc in
        let asz = preformat_annot annot in
        let items = List.map preformat_expr items in
        let ml, sz =
          List.fold_left
            (fun (tml, tsz) e ->
               let (ml, sz, _) = location e in
               (tml || ml, tsz + 1 + sz))
            (cml, String.length name + csz + asz)
            items in
        Prim ((ml, sz, loc), name, items, annot)
    | Seq (loc, items, annot) ->
        let cml, csz = preformat_loc loc in
        let asz = preformat_annot annot in
        let items = List.map preformat_expr items in
        let ml, sz =
          List.fold_left
            (fun (tml, tsz) e ->
               let (ml, sz, _) = location e in
               (tml || ml, tsz + 3 + sz))
            (cml, 4 + csz + asz)
            items in
        Seq ((ml, sz, loc), items, annot) in
  preformat_expr root

let rec print_expr_unwrapped ppf = function
  | Prim ((ml, s, { comment }), name, args, annot) ->
      let name = match annot with
        | None -> name
        | Some annot -> Format.asprintf "%s %s" name annot in
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
        | None -> Format.fprintf ppf "%s" value
        | Some comment -> Format.fprintf ppf "%s@ %a" value print_comment comment
      end
  | String ((_, _, { comment }), value) ->
      begin match comment with
        | None -> print_string ppf value
        | Some comment -> Format.fprintf ppf "%a@ %a" print_string value print_comment comment
      end
  | Seq ((_, _, { comment = None }), [], None) ->
      Format.fprintf ppf "{}"
  | Seq ((ml, s, { comment }), items, annot) ->
      if not ml && s < 80 then
        Format.fprintf ppf "{ @[<h 0>"
      else
        Format.fprintf ppf "{ @[<v 0>" ;
      begin match annot, comment, items with
        | None, _, _ -> ()
        | Some annot, None, [] -> Format.fprintf ppf "%s" annot
        | Some annot, _, _ -> Format.fprintf ppf "%s@ " annot
      end ;
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
  | Prim (_, _, [], Some _) as expr ->
      Format.fprintf ppf "(%a)" print_expr_unwrapped expr
  | expr -> print_expr_unwrapped ppf expr

let print_expr_unwrapped ppf expr =
  print_expr_unwrapped ppf (preformat expr)

let print_expr ppf expr =
  print_expr ppf (preformat expr)
