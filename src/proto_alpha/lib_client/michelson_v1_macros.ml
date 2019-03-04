(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Tezos_micheline
open Micheline

module IntMap = Map.Make (Compare.Int)

type 'l node = ('l, string) Micheline.node

type error += Unexpected_macro_annotation of string
type error += Sequence_expected of string
type error += Invalid_arity of string * int * int

let rec check_letters str i j f =
  i > j || f (String.get str i) && check_letters str (i + 1) j f

let expand_caddadr original =
  match original with
  | Prim (loc, str, args, annot) ->
      let len = String.length str in
      if len > 3
      && String.get str 0 = 'C'
      && String.get str (len - 1) = 'R'
      && check_letters str 1 (len - 2)
           (function 'A' | 'D' -> true | _ -> false) then
        begin match args with
          | [] -> ok ()
          | _ :: _ -> error (Invalid_arity (str, List.length args, 0))
        end >>? fun () ->
        let rec parse i annot acc =
          if i = 0 then
            Seq (loc, acc)
          else
            let annot = if i = len - 2 then annot else [] in
            match String.get str i with
            | 'A' -> parse (i - 1) [] (Prim (loc, "CAR", [], annot) :: acc)
            | 'D' -> parse (i - 1) [] (Prim (loc, "CDR", [], annot) :: acc)
            | _ -> assert false in
        ok (Some (parse (len - 2) annot []))
      else
        ok None
  | _ -> ok None

let extract_field_annots annot =
  List.partition (fun a ->
      match a.[0] with
      | '%' -> true
      | _ -> false
      | exception Invalid_argument _ -> false
    ) annot

let expand_set_caddadr original =
  match original with
  | Prim (loc, str, args, annot) ->
      let len = String.length str in
      if len >= 7
      && String.sub str 0 5 = "SET_C"
      && String.get str (len - 1) = 'R'
      && check_letters str 5 (len - 2)
           (function 'A' | 'D' -> true | _ -> false) then
        begin match args with
          | [] -> ok ()
          | _ :: _ -> error (Invalid_arity (str, List.length args, 0))
        end >>? fun () ->
        begin match extract_field_annots annot with
          | [], annot -> ok (None, annot)
          | [f], annot -> ok (Some f, annot)
          | _, _ -> error (Unexpected_macro_annotation str)
        end >>? fun (field_annot, annot) ->
        let rec parse i acc =
          if i = 4 then
            acc
          else
            let annot = if i = 5 then annot else [] in
            match String.get str i with
            | 'A' ->
                let acc =
                  Seq (loc,
                       [ Prim (loc, "DUP", [], []) ;
                         Prim (loc, "DIP",
                               [ Seq (loc,
                                      [ Prim (loc, "CAR", [], [ "@%%" ]) ;
                                        acc ]) ], []) ;
                         Prim (loc, "CDR", [], [ "@%%" ]) ;
                         Prim (loc, "SWAP", [], []) ;
                         Prim (loc, "PAIR", [], "%@" :: "%@" :: annot) ]) in
                parse (i - 1) acc
            | 'D' ->
                let acc =
                  Seq (loc,
                       [ Prim (loc, "DUP", [], []) ;
                         Prim (loc, "DIP",
                               [ Seq (loc,
                                      [ Prim (loc, "CDR", [], [ "@%%" ]) ;
                                        acc ]) ], []) ;
                         Prim (loc, "CAR", [], [ "@%%" ]) ;
                         Prim (loc, "PAIR", [], "%@" :: "%@" :: annot) ]) in
                parse (i - 1) acc
            | _ -> assert false in
        match String.get str (len - 2) with
        | 'A' ->
            let access_check = match field_annot with
              | None -> []
              | Some f -> [ Prim (loc, "DUP", [], []) ;
                            Prim (loc, "CAR", [], [ f ]) ;
                            Prim (loc, "DROP", [], []) ;
                          ] in
            let encoding = [ Prim (loc, "CDR", [], [ "@%%" ]) ;
                             Prim (loc, "SWAP", [], []) ] in
            let pair = [ Prim (loc, "PAIR", [],
                               [ Option.unopt field_annot ~default:"%" ; "%@" ]) ] in
            let init = Seq (loc, access_check @ encoding @ pair) in
            ok (Some (parse (len - 3) init))
        | 'D' ->
            let access_check = match field_annot with
              | None -> []
              | Some f -> [ Prim (loc, "DUP", [], []) ;
                            Prim (loc, "CDR", [], [ f ]) ;
                            Prim (loc, "DROP", [], []) ;
                          ] in
            let encoding = [ Prim (loc, "CAR", [], [ "@%%" ]) ] in
            let pair = [ Prim (loc, "PAIR", [],
                               [ "%@" ; Option.unopt field_annot ~default:"%" ]) ] in
            let init = Seq (loc, access_check @ encoding @ pair) in
            ok (Some (parse (len - 3) init))
        | _ -> assert false
      else
        ok None
  | _ -> ok None

let expand_map_caddadr original =
  match original with
  | Prim (loc, str, args, annot) ->
      let len = String.length str in
      if len >= 7
      && String.sub str 0 5 = "MAP_C"
      && String.get str (len - 1) = 'R'
      && check_letters str 5 (len - 2)
           (function 'A' | 'D' -> true | _ -> false) then
        begin match args with
          | [ Seq _ as code ] -> ok code
          | [ _ ] -> error (Sequence_expected str)
          | [] | _ :: _ :: _ -> error (Invalid_arity (str, List.length args, 1))
        end >>? fun code ->
        begin match extract_field_annots annot with
          | [], annot -> ok (None, annot)
          | [f], annot -> ok (Some f, annot)
          | _, _ -> error (Unexpected_macro_annotation str)
        end >>? fun (field_annot, annot) ->
        let rec parse i acc =
          if i = 4 then
            acc
          else
            let annot = if i = 5 then annot else [] in
            match String.get str i with
            | 'A' ->
                let acc =
                  Seq (loc,
                       [ Prim (loc, "DUP", [], []) ;
                         Prim (loc, "DIP",
                               [ Seq (loc,
                                      [ Prim (loc, "CAR", [], [ "@%%" ]) ;
                                        acc ]) ], []) ;
                         Prim (loc, "CDR", [], [ "@%%" ]) ;
                         Prim (loc, "SWAP", [], []) ;
                         Prim (loc, "PAIR", [], "%@" :: "%@" :: annot) ]) in
                parse (i - 1) acc
            | 'D' ->
                let acc =
                  Seq (loc,
                       [ Prim (loc, "DUP", [], []) ;
                         Prim (loc, "DIP",
                               [ Seq (loc,
                                      [ Prim (loc, "CDR", [], [ "@%%" ]) ;
                                        acc ]) ], []) ;
                         Prim (loc, "CAR", [], [ "@%%" ]) ;
                         Prim (loc, "PAIR", [], "%@" :: "%@" :: annot) ]) in
                parse (i - 1) acc
            | _ -> assert false in
        let cr_annot = match field_annot with
          | None -> []
          | Some f -> [ "@" ^ String.sub f 1 (String.length f - 1) ] in
        match String.get str (len - 2) with
        | 'A' ->
            let init =
              Seq (loc,
                   [ Prim (loc, "DUP", [], []) ;
                     Prim (loc, "CDR", [], [ "@%%" ]) ;
                     Prim (loc, "DIP",
                           [ Seq (loc, [ Prim (loc, "CAR", [], cr_annot) ; code ]) ], []) ;
                     Prim (loc, "SWAP", [], []) ;
                     Prim (loc, "PAIR", [],
                           [ Option.unopt field_annot ~default:"%" ; "%@"]) ]) in
            ok (Some (parse (len - 3) init))
        | 'D' ->
            let init =
              Seq (loc,
                   [ Prim (loc, "DUP", [], []) ;
                     Prim (loc, "CDR", [], cr_annot) ;
                     code ;
                     Prim (loc, "SWAP", [], []) ;
                     Prim (loc, "CAR", [], [ "@%%" ]) ;
                     Prim (loc, "PAIR", [],
                           [ "%@" ; Option.unopt field_annot ~default:"%" ]) ]) in
            ok (Some (parse (len - 3) init))
        | _ -> assert false
      else
        ok None
  | _ -> ok None

exception Not_a_roman

let decimal_of_roman roman =
  (* http://rosettacode.org/wiki/Roman_numerals/Decode#OCaml *)
  let arabic = ref 0 in
  let lastval = ref 0 in
  for i = (String.length roman) - 1 downto 0 do
    let n =
      match roman.[i] with
      | 'M' -> 1000
      | 'D' -> 500
      | 'C' -> 100
      | 'L' -> 50
      | 'X' -> 10
      | 'V' -> 5
      | 'I' -> 1
      | _ -> raise_notrace Not_a_roman
    in
    if Compare.Int.(n < !lastval)
    then arabic := !arabic - n
    else arabic := !arabic + n;
    lastval := n
  done;
  !arabic

let expand_dxiiivp original =
  match original with
  | Prim (loc, str, args, annot) ->
      let len = String.length str in
      if len > 3
      && String.get str 0 = 'D'
      && String.get str (len - 1) = 'P' then
        try
          let depth = decimal_of_roman (String.sub str 1 (len - 2)) in
          let rec make i acc =
            if i = 0 then
              acc
            else
              make (i - 1)
                (Seq (loc, [ Prim (loc, "DIP", [ acc ], annot) ])) in
          match args with
          | [ Seq (_, _) as arg ] -> ok @@ Some (make depth arg)
          | [ _ ] -> error (Sequence_expected str)
          | [] | _ :: _ :: _ -> error (Invalid_arity (str, List.length args, 1))
        with Not_a_roman -> ok None
      else ok None
  | _ -> ok None

exception Not_a_pair

let rec dip ~loc depth instr =
  if depth <= 0
  then instr
  else dip ~loc (depth - 1) (Prim (loc, "DIP", [ Seq (loc, [ instr ]) ], []))

type pair_item =
  | A
  | I
  | P of int * pair_item * pair_item

let parse_pair_substr str ~len start =
  let rec parse ?left i =
    if i = len - 1 then
      raise_notrace Not_a_pair
    else if String.get str i = 'P' then
      let next_i, l = parse ~left:true (i + 1) in
      let next_i, r = parse ~left:false next_i in
      next_i, P (i, l, r)
    else if String.get str i = 'A' && left = Some true then
      i + 1, A
    else if String.get str i = 'I' && left <> Some true then
      i + 1, I
    else
      raise_notrace Not_a_pair in
  let last, ast = parse start in
  if last <> len - 1 then
    raise_notrace Not_a_pair
  else
    ast

let unparse_pair_item ast =
  let rec unparse ast acc = match ast with
    | P (_, l, r) -> unparse r (unparse l ("P" :: acc))
    | A -> "A" :: acc
    | I -> "I" :: acc in
  List.rev ("R" :: unparse ast []) |> String.concat ""

let pappaiir_annots_pos ast annot =
  let rec find_annots_pos p_pos ast annots acc =
    match ast, annots with
    | _, [] -> annots, acc
    | P (i, left, right), _ ->
        let annots, acc = find_annots_pos i left annots acc in
        find_annots_pos i right annots acc
    | A, a :: annots ->
        let pos = match IntMap.find_opt p_pos acc with
          | None -> [ a ], []
          | Some (_, cdr) -> [ a ], cdr in
        annots, IntMap.add p_pos pos acc
    | I, a :: annots ->
        let pos = match IntMap.find_opt p_pos acc with
          | None -> [], [ a ]
          | Some (car, _) -> car, [ a ] in
        annots, IntMap.add p_pos pos acc in
  snd (find_annots_pos 0 ast annot IntMap.empty)

let expand_pappaiir original =
  match original with
  | Prim (loc, str, args, annot) ->
      let len = String.length str in
      if len > 4
      && String.get str 0 = 'P'
      && String.get str (len - 1) = 'R'
      && check_letters str 1 (len - 2)
           (function 'P' | 'A' | 'I' -> true | _ -> false) then
        try
          let field_annots, annot = extract_field_annots annot in
          let ast = parse_pair_substr str ~len 0 in
          let field_annots_pos = pappaiir_annots_pos ast field_annots in
          let rec parse p (depth, acc) =
            match p with
            | P (i, left, right) ->
                let annot =
                  match i, IntMap.find_opt i field_annots_pos with
                  | 0, None -> annot
                  | _, None -> []
                  | 0, Some ([], cdr_annot) -> "%" :: cdr_annot @ annot
                  | _, Some ([], cdr_annot) -> "%" :: cdr_annot
                  | 0, Some (car_annot, cdr_annot) -> car_annot @ cdr_annot @ annot
                  | _, Some (car_annot, cdr_annot) -> car_annot @ cdr_annot
                in
                let acc = dip ~loc depth (Prim (loc, "PAIR", [], annot)) :: acc in
                (depth, acc)
                |> parse left
                |> parse right
            | A | I -> (depth + 1, acc)
          in
          let _, expanded = parse ast (0, []) in
          begin match args with
            | [] -> ok ()
            | _ :: _ -> error (Invalid_arity (str, List.length args, 0))
          end >>? fun () ->
          ok (Some (Seq (loc, expanded)))
        with Not_a_pair -> ok None
      else
        ok None
  | _ -> ok None

let expand_unpappaiir original =
  match original with
  | Prim (loc, str, args, annot) ->
      let len = String.length str in
      if len >= 6
      && String.sub str 0 3 = "UNP"
      && String.get str (len - 1) = 'R'
      && check_letters str 3 (len - 2)
           (function 'P' | 'A' | 'I' -> true | _ -> false) then
        try
          let unpair car_annot cdr_annot =
            Seq (loc, [ Prim (loc, "DUP", [], []) ;
                        Prim (loc, "CAR", [], car_annot) ;
                        dip ~loc 1 (Prim (loc, "CDR", [], cdr_annot)) ;
                      ]) in
          let ast = parse_pair_substr str ~len 2 in
          let annots_pos = pappaiir_annots_pos ast annot in
          let rec parse p (depth, acc) =
            match p with
            | P (i, left, right) ->
                let car_annot, cdr_annot =
                  match IntMap.find_opt i annots_pos with
                  | None -> [], []
                  | Some (car_annot, cdr_annot) -> car_annot, cdr_annot in
                let acc = dip ~loc depth (unpair car_annot cdr_annot) :: acc in
                (depth, acc)
                |> parse left
                |> parse right
            | A | I -> (depth + 1, acc) in
          let _, rev_expanded = parse ast (0, []) in
          let expanded = Seq (loc, List.rev rev_expanded) in
          begin match args with
            | [] -> ok ()
            | _ :: _ -> error (Invalid_arity (str, List.length args, 0))
          end >>? fun () ->
          ok (Some expanded)
        with Not_a_pair -> ok None
      else
        ok None
  | _ -> ok None

exception Not_a_dup

let expand_duuuuup original =
  match original with
  | Prim (loc, str, args, annot) ->
      let len = String.length str in
      if len > 3
      && String.get str 0 = 'D'
      && String.get str (len - 1) = 'P'
      && check_letters str 1 (len - 2) ((=) 'U') then
        begin match args with
          | [] -> ok ()
          | _ :: _ -> error (Invalid_arity (str, List.length args, 0))
        end >>? fun () ->
        try
          let rec parse i acc =
            if i = 1 then acc
            else if String.get str i = 'U' then
              parse (i - 1)
                (Seq (loc, [ Prim (loc, "DIP", [ acc ], []) ;
                             Prim (loc, "SWAP", [], []) ]))
            else
              raise_notrace Not_a_dup in
          ok (Some (parse (len - 2) (Seq (loc, [ Prim (loc, "DUP", [], annot) ]))))
        with Not_a_dup -> ok None
      else
        ok None
  | _ -> ok None

let expand_compare original =
  let cmp loc is annot =
    let is =
      match List.rev_map (fun i -> Prim (loc, i, [], [])) is with
      | Prim (loc, i, args, _) :: r -> List.rev (Prim (loc, i, args, annot) :: r)
      | is -> List.rev is
    in
    ok (Some (Seq (loc, is))) in
  let ifcmp loc is l r annot =
    let is =
      List.map (fun i -> Prim (loc, i, [], [])) is @
      [ Prim (loc, "IF", [ l ; r ], annot) ] in
    ok (Some (Seq (loc, is))) in
  match original with
  | Prim (loc, "CMPEQ", [], annot) ->
      cmp loc [ "COMPARE" ; "EQ" ] annot
  | Prim (loc, "CMPNEQ", [], annot) ->
      cmp loc [ "COMPARE" ; "NEQ" ] annot
  | Prim (loc, "CMPLT", [], annot) ->
      cmp loc [ "COMPARE" ; "LT" ] annot
  | Prim (loc, "CMPGT", [], annot) ->
      cmp loc [ "COMPARE" ; "GT" ] annot
  | Prim (loc, "CMPLE", [], annot) ->
      cmp loc [ "COMPARE" ; "LE" ] annot
  | Prim (loc, "CMPGE", [], annot) ->
      cmp loc [ "COMPARE" ; "GE" ] annot
  | Prim (_, ("CMPEQ" |  "CMPNEQ" |  "CMPLT"
             |  "CMPGT" |  "CMPLE" | "CMPGE" as str), args, []) ->
      error (Invalid_arity (str, List.length args, 0))
  | Prim (loc, "IFCMPEQ", [ l ; r ], annot) ->
      ifcmp loc [ "COMPARE" ; "EQ" ] l r annot
  | Prim (loc, "IFCMPNEQ", [ l ; r ], annot) ->
      ifcmp loc [ "COMPARE" ; "NEQ" ] l r annot
  | Prim (loc, "IFCMPLT", [ l ; r ], annot) ->
      ifcmp loc [ "COMPARE" ; "LT" ] l r annot
  | Prim (loc, "IFCMPGT", [ l ; r ], annot) ->
      ifcmp loc [ "COMPARE" ; "GT" ] l r annot
  | Prim (loc, "IFCMPLE", [ l ; r ], annot) ->
      ifcmp loc [ "COMPARE" ; "LE" ] l r annot
  | Prim (loc, "IFCMPGE", [ l ; r ], annot) ->
      ifcmp loc [ "COMPARE" ; "GE" ] l r annot
  | Prim (loc, "IFEQ", [ l ; r ], annot) ->
      ifcmp loc [ "EQ" ] l r annot
  | Prim (loc, "IFNEQ", [ l ; r ], annot) ->
      ifcmp loc [ "NEQ" ] l r annot
  | Prim (loc, "IFLT", [ l ; r ], annot) ->
      ifcmp loc [ "LT" ] l r annot
  | Prim (loc, "IFGT", [ l ; r ], annot) ->
      ifcmp loc [ "GT" ] l r annot
  | Prim (loc, "IFLE", [ l ; r ], annot) ->
      ifcmp loc [ "LE" ] l r annot
  | Prim (loc, "IFGE", [ l ; r ], annot) ->
      ifcmp loc [ "GE" ] l r annot
  | Prim (_, ("IFCMPEQ" | "IFCMPNEQ" | "IFCMPLT"
             | "IFCMPGT" | "IFCMPLE" | "IFCMPGE"
             | "IFEQ" | "IFNEQ" | "IFLT"
             | "IFGT" | "IFLE" | "IFGE" as str), args, []) ->
      error (Invalid_arity (str, List.length args, 2))
  | Prim (_, ("IFCMPEQ" | "IFCMPNEQ" | "IFCMPLT"
             | "IFCMPGT" | "IFCMPLE" | "IFCMPGE"
             | "IFEQ" | "IFNEQ" | "IFLT"
             | "IFGT" | "IFLE" | "IFGE" as str), [], _ :: _) ->
      error (Unexpected_macro_annotation str)
  | _ -> ok None

let expand_asserts original =
  let may_rename loc = function
    | [] -> Seq (loc, [])
    | annot -> Seq (loc, [ Prim (loc, "RENAME", [], annot) ])
  in
  let fail_false ?(annot=[]) loc =
    [may_rename loc annot; Seq (loc, [ Prim (loc, "FAIL", [], []) ])]
  in
  let fail_true ?(annot=[]) loc =
    [Seq (loc, [ Prim (loc, "FAIL", [], []) ]); may_rename loc annot]
  in
  match original with
  | Prim (loc, "ASSERT", [], []) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF", fail_false loc, []) ]))
  | Prim (loc, "ASSERT_NONE", [], []) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_NONE", fail_false loc, []) ]))
  | Prim (loc, "ASSERT_SOME", [], annot) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_NONE", fail_true ~annot loc, []) ]))
  | Prim (loc, "ASSERT_LEFT", [], annot) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_LEFT", fail_false ~annot loc, []) ]))
  | Prim (loc, "ASSERT_RIGHT", [], annot) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_LEFT", fail_true ~annot loc, []) ]))
  | Prim (_, ("ASSERT" | "ASSERT_NONE" | "ASSERT_SOME"
             | "ASSERT_LEFT" | "ASSERT_RIGHT" as str), args, []) ->
      error (Invalid_arity (str, List.length args, 0))
  | Prim (_, ( "ASSERT" | "ASSERT_NONE" as str), [], _ :: _) ->
      error (Unexpected_macro_annotation str)
  | Prim (loc, s, args, annot)
    when String.(length s >  7 && equal (sub s 0 7) "ASSERT_") ->
      begin match args with
        | [] -> ok ()
        | _ :: _ -> error (Invalid_arity (s, List.length args, 0))
      end >>? fun () ->
      begin match annot with
        | _ :: _ -> (error (Unexpected_macro_annotation s))
        | [] -> ok ()
      end >>? fun () ->
      begin
        let remaining = String.(sub s 7 (length s - 7)) in
        let remaining_prim = Prim (loc, remaining, [], []) in
        match remaining with
        | "EQ" | "NEQ" | "LT" | "LE" | "GE" | "GT" ->
            ok @@ Some (Seq (loc, [ remaining_prim ;
                                    Prim (loc, "IF", fail_false loc, []) ]))
        | _ ->
            begin
              expand_compare remaining_prim >|? function
              | None -> None
              | Some seq ->
                  Some (Seq (loc, [ seq ;
                                    Prim (loc, "IF", fail_false loc, []) ]))
            end
      end
  | _ -> ok None


let expand_if_some = function
  | Prim (loc, "IF_SOME", [ right ; left ], annot) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_NONE", [ left ; right ], annot) ]))
  | Prim (_, "IF_SOME", args, _annot) ->
      error (Invalid_arity ("IF_SOME", List.length args, 2))
  | _ -> ok @@ None

let expand_if_right = function
  | Prim (loc, "IF_RIGHT", [ right ; left ], annot) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_LEFT", [ left ; right ], annot) ]))
  | Prim (_, "IF_RIGHT", args, _annot) ->
      error (Invalid_arity ("IF_RIGHT", List.length args, 2))
  | _ -> ok @@ None

let expand_fail = function
  | Prim (loc, "FAIL", [], []) ->
      ok @@ Some (Seq (loc, [
          Prim (loc, "UNIT", [], []) ;
          Prim (loc, "FAILWITH", [], []) ;
        ]))
  | _ -> ok @@ None

let expand original =
  let rec try_expansions = function
    | [] -> ok @@ original
    | expander :: expanders ->
        expander original >>? function
        | None -> try_expansions expanders
        | Some rewritten -> ok rewritten in
  try_expansions
    [ expand_caddadr ;
      expand_set_caddadr ;
      expand_map_caddadr ;
      expand_dxiiivp ;
      (* expand_paaiair ; *)
      expand_pappaiir ;
      (* expand_unpaaiair ; *)
      expand_unpappaiir ;
      expand_duuuuup ;
      expand_compare ;
      expand_asserts ;
      expand_if_some ;
      expand_if_right ;
      expand_fail ;
    ]

let expand_rec expr =
  let rec error_map (expanded, errors) f = function
    | [] -> (List.rev expanded, List.rev errors)
    | hd :: tl ->
        let (new_expanded, new_errors) = f hd in
        error_map
          (new_expanded :: expanded, List.rev_append new_errors errors)
          f tl in
  let error_map = error_map ([], []) in
  let rec expand_rec expr =
    match expand expr with
    | Ok expanded ->
        begin
          match expanded with
          | Seq (loc, items) ->
              let items, errors = error_map expand_rec items in
              (Seq (loc, items), errors)
          | Prim (loc, name, args, annot) ->
              let args, errors = error_map expand_rec args in
              (Prim (loc, name, args, annot), errors)
          | Int _ | String _ | Bytes _ as atom -> (atom, []) end
    | Error errors -> (expr, errors) in
  expand_rec expr

let unexpand_caddadr expanded =
  let rec rsteps acc = function
    | [] -> Some acc
    | Prim (_, "CAR" , [], []) :: rest ->
        rsteps ("A" :: acc) rest
    | Prim (_, "CDR" , [], []) :: rest ->
        rsteps ("D" :: acc) rest
    | _ -> None in
  match expanded with
  | Seq (loc, (Prim (_, "CAR" , [], []) :: _ as nodes))
  | Seq (loc, (Prim (_, "CDR" , [], []) :: _ as nodes)) ->
      begin match rsteps [] nodes with
        | Some steps ->
            let name = String.concat "" ("C" :: List.rev ("R" :: steps)) in
            Some (Prim (loc, name, [], []))
        | None -> None
      end
  | _ -> None

let unexpand_set_caddadr expanded =
  let rec steps acc annots = function
    | Seq (loc,
           [ Prim (_, "CDR", [], _) ;
             Prim (_, "SWAP", [], _) ;
             Prim (_, "PAIR", [], _) ]) ->
        Some (loc, "A" :: acc, annots)
    | Seq (loc,
           [ Prim (_, "DUP", [], []) ;
             Prim (_, "CAR", [], [ field_annot ]) ;
             Prim (_, "DROP", [], []) ;
             Prim (_, "CDR", [], _) ;
             Prim (_, "SWAP", [], []) ;
             Prim (_, "PAIR", [], _) ]) ->
        Some (loc, "A" :: acc, field_annot :: annots)
    | Seq (loc,
           [ Prim (_, "CAR", [], _) ;
             Prim (_, "PAIR", [], _) ]) ->
        Some (loc, "D" :: acc, annots)
    | Seq (loc,
           [ Prim (_, "DUP", [], []) ;
             Prim (_, "CDR", [], [ field_annot ]) ;
             Prim (_, "DROP", [], []) ;
             Prim (_, "CAR", [], _) ;
             Prim (_, "PAIR", [], _) ]) ->
        Some (loc, "D" :: acc, field_annot :: annots)
    | Seq (_,
           [ Prim (_, "DUP", [], []) ;
             Prim (_, "DIP",
                   [ Seq (_,
                          [ Prim (_, "CAR", [], _) ;
                            sub ]) ], []) ;
             Prim (_, "CDR", [], _) ;
             Prim (_, "SWAP", [], []) ;
             Prim (_, "PAIR", [], pair_annots) ]) ->
        let _, pair_annots = extract_field_annots pair_annots in
        steps ("A" :: acc) (List.rev_append pair_annots annots) sub
    | Seq (_,
           [ Prim (_, "DUP", [], []) ;
             Prim (_, "DIP",
                   [ Seq (_,
                          [ Prim (_, "CDR", [], _) ;
                            sub ]) ], []) ;
             Prim (_, "CAR", [], _) ;
             Prim (_, "PAIR", [], pair_annots) ]) ->
        let _, pair_annots = extract_field_annots pair_annots in
        steps ("D" :: acc) (List.rev_append pair_annots annots) sub
    | _ -> None in
  match steps [] [] expanded with
  | Some (loc, steps, annots) ->
      let name = String.concat "" ("SET_C" :: List.rev ("R" :: steps)) in
      Some (Prim (loc, name, [], List.rev annots))
  | None -> None

let unexpand_map_caddadr expanded =
  let rec steps acc annots = function
    | Seq (loc,
           [ Prim (_, "DUP", [], []) ;
             Prim (_, "CDR", [], _) ;
             Prim (_, "SWAP", [], []) ;
             Prim (_, "DIP",
                   [ Seq (_,
                          [ Prim (_, "CAR", [], []) ;
                            code ]) ], []) ;
             Prim (_, "PAIR", [], _) ]) ->
        Some (loc, "A" :: acc, annots, code)
    | Seq (loc,
           [ Prim (_, "DUP", [], []) ;
             Prim (_, "CDR", [], _) ;
             Prim (_, "SWAP", [], []) ;
             Prim (_, "DIP",
                   [ Seq (_,
                          [ Prim (_, "CAR", [], [ field_annot ]) ;
                            code ]) ], []) ;
             Prim (_, "PAIR", [], _) ]) ->
        Some (loc, "A" :: acc, field_annot :: annots, code)
    | Seq (loc,
           [ Prim (_, "DUP", [], []) ;
             Prim (_, "CDR", [], []) ;
             code ;
             Prim (_, "SWAP", [], []) ;
             Prim (_, "CAR", [], _) ;
             Prim (_, "PAIR", [], _) ]) ->
        Some (loc, "D" :: acc, annots, code)
    | Seq (loc,
           [ Prim (_, "DUP", [], []) ;
             Prim (_, "CDR", [], [ field_annot ]) ;
             code ;
             Prim (_, "SWAP", [], []) ;
             Prim (_, "CAR", [], _) ;
             Prim (_, "PAIR", [], _) ]) ->
        Some (loc, "D" :: acc, field_annot :: annots, code)
    | Seq (_,
           [ Prim (_, "DUP", [], []) ;
             Prim (_, "DIP",
                   [ Seq (_,
                          [ Prim (_, "CAR", [], _) ;
                            sub ]) ], []) ;
             Prim (_, "CDR", [], _) ;
             Prim (_, "SWAP", [], []) ;
             Prim (_, "PAIR", [], pair_annots) ]) ->
        let _, pair_annots = extract_field_annots pair_annots in
        steps ("A" :: acc) (List.rev_append pair_annots annots) sub
    | Seq (_,
           [ Prim (_, "DUP", [], []) ;
             Prim (_, "DIP",
                   [ Seq (_,
                          [ Prim (_, "CDR", [], []) ;
                            sub ]) ], []) ;
             Prim (_, "CAR", [], []) ;
             Prim (_, "PAIR", [], pair_annots) ]) ->
        let _, pair_annots = extract_field_annots pair_annots in
        steps ("D" :: acc) (List.rev_append pair_annots annots) sub
    | _ -> None in
  match steps [] [] expanded with
  | Some (loc, steps, annots, code) ->
      let name = String.concat "" ("MAP_C" :: List.rev ("R" :: steps)) in
      Some (Prim (loc, name, [ code ], List.rev annots))
  | None -> None

let roman_of_decimal decimal =
  (* http://rosettacode.org/wiki/Roman_numerals/Encode#OCaml *)
  let digit x y z = function
    | 1 -> [ x ]
    | 2 -> [ x ; x ]
    | 3 -> [ x ; x ; x ]
    | 4 -> [ x ; y ]
    | 5 -> [ y ]
    | 6 -> [ y ; x ]
    | 7 -> [ y ; x ; x ]
    | 8 -> [ y ; x ; x ; x ]
    | 9 -> [ x ; z ]
    | _ -> assert false in
  let rec to_roman x =
    if x = 0 then []
    else if x < 0 then
      invalid_arg "Negative roman numeral"
    else if x >= 1000 then
      "M" :: to_roman (x - 1000)
    else if x >= 100 then
      digit "C" "D" "M" (x / 100) @ to_roman (x mod 100)
    else if x >= 10 then
      digit "X" "L" "C" (x / 10) @ to_roman (x mod 10)
    else
      digit "I" "V" "X" x in
  String.concat "" (to_roman decimal)

let dxiiivp_roman_of_decimal decimal =
  let roman = roman_of_decimal decimal in
  if String.length roman = 1 then
    (* too short for D*P, fall back to IIIII... *)
    String.concat "" (List.init decimal (fun _ -> "I"))
  else
    roman

let unexpand_dxiiivp expanded =
  match expanded with
  | Seq (loc,
         [ Prim (_, "DIP",
                 [ Seq (_, [ Prim (_, "DIP", [ _ ], []) ]) as sub ],
                 []) ]) ->
      let rec count acc = function
        | Seq (_, [ Prim (_, "DIP", [ sub ], []) ]) -> count (acc + 1) sub
        | sub -> (acc, sub) in
      let depth, sub = count 1 sub in
      let name = "D" ^ dxiiivp_roman_of_decimal depth ^ "P" in
      Some (Prim (loc, name, [ sub ], []))
  | _ -> None

let unexpand_duuuuup expanded =
  let rec help expanded =
    match expanded with
    | Seq (loc, [ Prim (_, "DUP", [], []) ]) -> Some (loc, 1)
    | Seq (_, [ Prim (_, "DIP", [expanded'], []);
                Prim (_, "SWAP", [], []) ]) ->
        begin
          match help expanded' with
          | None -> None
          | Some (loc, n) -> Some (loc, n + 1)
        end
    | _ -> None
  in let rec dupn = function
      | 0 -> "P"
      | n -> "U" ^ (dupn (n - 1)) in
  match help expanded with
  | None -> None
  | Some (loc, n) -> Some (Prim (loc, "D" ^ (dupn n), [], []))

let rec normalize_pair_item ?(right=false) = function
  | P (i, a, b) -> P (i, normalize_pair_item a, normalize_pair_item ~right:true b)
  | A when right -> I
  | A -> A
  | I -> I

let unexpand_pappaiir expanded =
  match expanded with
  | Seq (_,  [ Prim (_, "PAIR", [], []) ]) -> Some expanded
  | Seq (loc, (_ :: _ as nodes)) ->
      let rec exec stack nodes = match nodes, stack with
        | [], _ -> stack
        | Prim (_, "DIP", [ Seq (_, sub) ], []) :: rest, a :: rstack ->
            exec (a :: exec rstack sub) rest
        | Prim (_, "DIP", [ Seq (_, sub) ], []) :: rest, [] ->
            exec (A :: exec [] sub) rest
        | Prim (_, "PAIR", [], []) :: rest, a :: b :: rstack ->
            exec (P (0, a, b) :: rstack) rest
        | Prim (_, "PAIR", [], []) :: rest, [ a ]  ->
            exec [ P (0, a, I) ] rest
        | Prim (_, "PAIR", [], []) :: rest, []  ->
            exec [ P (0, A, I) ] rest
        | _ -> raise_notrace Not_a_pair in
      begin match exec [] nodes with
        | [] -> None
        | res :: _ ->
            let res = normalize_pair_item res in
            let name = unparse_pair_item res in
            Some (Prim (loc, name, [], []))
        | exception Not_a_pair -> None
      end
  | _ -> None

let unexpand_unpappaiir expanded =
  match expanded with
  | Seq (loc, (_ :: _ as nodes)) ->
      let rec exec stack nodes = match nodes, stack with
        | [], _ -> stack
        | Prim (_, "DIP", [ Seq (_, sub) ], []) :: rest, a :: rstack ->
            exec (a :: exec rstack sub) rest
        | Prim (_, "DIP", [ Seq (_, sub) ], []) :: rest, [] ->
            exec (A :: exec [] sub) rest
        | Seq (_, [ Prim (_, "DUP", [], []) ;
                    Prim (_, "CAR", [], []) ;
                    Prim (_, "DIP",
                          [ Seq (_, [ Prim (_, "CDR", [], []) ]) ],
                          []) ]) :: rest,
          a :: b :: rstack ->
            exec (P (0, a, b) :: rstack) rest
        | Seq (_, [ Prim (_, "DUP", [], []) ;
                    Prim (_, "CAR", [], []) ;
                    Prim (_, "DIP",
                          [ Seq (_, [ Prim (_, "CDR", [], []) ]) ],
                          []) ]) :: rest,
          [ a ]  ->
            exec [ P (0, a, I) ] rest
        | Seq (_, [ Prim (_, "DUP", [], []) ;
                    Prim (_, "CAR", [], []) ;
                    Prim (_, "DIP",
                          [ Seq (_, [ Prim (_, "CDR", [], []) ]) ],
                          []) ]) :: rest,
          []  ->
            exec [ P (0, A, I) ] rest
        | _ -> raise_notrace Not_a_pair in
      begin match exec [] (List.rev nodes) with
        | [] -> None
        | res :: _ ->
            let res = normalize_pair_item res in
            let name = "UN" ^ unparse_pair_item res in
            Some (Prim (loc, name, [], []))
        | exception Not_a_pair -> None
      end
  | _ -> None


let unexpand_compare expanded =
  match expanded with
  | Seq (loc, [ Prim (_, "COMPARE", [], _) ;
                Prim (_, "EQ", [], annot) ]) ->
      Some (Prim (loc, "CMPEQ", [], annot))
  | Seq (loc, [ Prim (_, "COMPARE", [], _) ;
                Prim (_, "NEQ", [], annot) ]) ->
      Some (Prim (loc, "CMPNEQ", [], annot))
  | Seq (loc, [ Prim (_, "COMPARE", [], _) ;
                Prim (_, "LT", [], annot) ]) ->
      Some (Prim (loc, "CMPLT", [], annot))
  | Seq (loc, [ Prim (_, "COMPARE", [], _) ;
                Prim (_, "GT", [], annot) ]) ->
      Some (Prim (loc, "CMPGT", [], annot))
  | Seq (loc, [ Prim (_, "COMPARE", [], _) ;
                Prim (_, "LE", [], annot) ]) ->
      Some (Prim (loc, "CMPLE", [], annot))
  | Seq (loc, [ Prim (_, "COMPARE", [], _) ;
                Prim (_, "GE", [], annot) ]) ->
      Some (Prim (loc, "CMPGE", [], annot))
  | Seq (loc, [ Prim (_, "COMPARE", [], _) ;
                Prim (_, "EQ", [], _) ;
                Prim (_, "IF", args, annot) ]) ->
      Some (Prim (loc, "IFCMPEQ", args, annot))
  | Seq (loc, [ Prim (_, "COMPARE", [], _) ;
                Prim (_, "NEQ", [], _) ;
                Prim (_, "IF", args, annot) ]) ->
      Some (Prim (loc, "IFCMPNEQ", args, annot))
  | Seq (loc, [ Prim (_, "COMPARE", [], _) ;
                Prim (_, "LT", [], _) ;
                Prim (_, "IF", args, annot) ]) ->
      Some (Prim (loc, "IFCMPLT", args, annot))
  | Seq (loc, [ Prim (_, "COMPARE", [], _) ;
                Prim (_, "GT", [], _) ;
                Prim (_, "IF", args, annot) ]) ->
      Some (Prim (loc, "IFCMPGT", args, annot))
  | Seq (loc, [ Prim (_, "COMPARE", [], _) ;
                Prim (_, "LE", [], _) ;
                Prim (_, "IF", args, annot) ]) ->
      Some (Prim (loc, "IFCMPLE", args, annot))
  | Seq (loc, [ Prim (_, "COMPARE", [], _) ;
                Prim (_, "GE", [], _) ;
                Prim (_, "IF", args, annot) ]) ->
      Some (Prim (loc, "IFCMPGE", args, annot))
  | Seq (loc, [ Prim (_, "EQ", [], _) ;
                Prim (_, "IF", args, annot) ]) ->
      Some (Prim (loc, "IFEQ", args, annot))
  | Seq (loc, [ Prim (_, "NEQ", [], _) ;
                Prim (_, "IF", args, annot) ]) ->
      Some (Prim (loc, "IFNEQ", args, annot))
  | Seq (loc, [ Prim (_, "LT", [], _) ;
                Prim (_, "IF", args, annot) ]) ->
      Some (Prim (loc, "IFLT", args, annot))
  | Seq (loc, [ Prim (_, "GT", [], _) ;
                Prim (_, "IF", args, annot) ]) ->
      Some (Prim (loc, "IFGT", args, annot))
  | Seq (loc, [ Prim (_, "LE", [], _) ;
                Prim (_, "IF", args, annot) ]) ->
      Some (Prim (loc, "IFLE", args, annot))
  | Seq (loc, [ Prim (_, "GE", [], _) ;
                Prim (_, "IF", args, annot) ]) ->
      Some (Prim (loc, "IFGE", args, annot))
  | _ -> None

let unexpand_asserts expanded =
  match expanded with
  | Seq (loc, [ Prim (_, "IF", [ Seq (_, []) ;
                                 Seq (_, [
                                     Seq (_, [
                                         Prim (_, "UNIT", [], []) ;
                                         Prim (_, "FAILWITH", [], []) ]) ]) ],
                      []) ]) ->
      Some (Prim (loc, "ASSERT", [], []))
  | Seq (loc, [ Seq (_, [ Prim(_, "COMPARE", [], []) ; Prim (_, comparison, [], []) ]) ;
                Prim (_, "IF", [ Seq (_, []) ;
                                 Seq (_, [
                                     Seq (_, [
                                         Prim (_, "UNIT", [], []) ;
                                         Prim (_, "FAILWITH", [], []) ]) ]) ],
                      []) ]) ->
      Some (Prim (loc, "ASSERT_CMP" ^ comparison, [], []))
  | Seq (loc, [ Prim (_, comparison, [], []) ;
                Prim (_, "IF", [ Seq (_, []) ;
                                 Seq (_, [
                                     Seq (_, [
                                         Prim (_, "UNIT", [], []) ;
                                         Prim (_, "FAILWITH", [], []) ]) ]) ],
                      []) ]) ->
      Some (Prim (loc, "ASSERT_" ^ comparison, [], []))
  | Seq (loc, [ Prim (_, "IF_NONE", [ Seq (_, [ Prim (_, "RENAME", [], annot) ]) ;
                                      Seq (_, [
                                          Seq (_, [
                                              Prim (_, "UNIT", [], []) ;
                                              Prim (_, "FAILWITH", [], []) ]) ]) ],
                      []) ]) ->
      Some (Prim (loc, "ASSERT_NONE", [], annot))
  | Seq (loc, [ Prim (_, "IF_NONE", [ Seq (_, []) ;
                                      Seq (_, [
                                          Seq (_, [
                                              Prim (_, "UNIT", [], []) ;
                                              Prim (_, "FAILWITH", [], []) ]) ]) ],
                      []) ]) ->
      Some (Prim (loc, "ASSERT_NONE", [], []))
  | Seq (loc, [ Prim (_, "IF_NONE", [ Seq (_, [ Seq (_, [ Prim (_, "UNIT", [], []) ;
                                                          Prim (_, "FAILWITH", [], []) ]) ]) ;
                                      Seq (_, [])],
                      []) ]) ->
      Some (Prim (loc, "ASSERT_SOME", [], []))
  | Seq (loc, [ Prim (_, "IF_NONE", [ Seq (_, [ Seq (_, [ Prim (_, "UNIT", [], []) ;
                                                          Prim (_, "FAILWITH", [], []) ]) ]) ;
                                      Seq (_, [ Prim (_, "RENAME", [], annot) ])],
                      []) ]) ->
      Some (Prim (loc, "ASSERT_SOME", [], annot))
  | Seq (loc, [ Prim (_, "IF_LEFT", [ Seq (_, []) ;
                                      Seq (_, [
                                          Seq (_, [
                                              Prim (_, "UNIT", [], []) ;
                                              Prim (_, "FAILWITH", [], []) ]) ]) ],
                      []) ]) ->
      Some (Prim (loc, "ASSERT_LEFT", [], []))
  | Seq (loc, [ Prim (_, "IF_LEFT", [ Seq (_, [ Prim (_, "RENAME", [], annot) ]) ;
                                      Seq (_, [
                                          Seq (_, [
                                              Prim (_, "UNIT", [], []) ;
                                              Prim (_, "FAILWITH", [], []) ]) ]) ],
                      []) ]) ->
      Some (Prim (loc, "ASSERT_LEFT", [], annot))
  | Seq (loc, [ Prim (_, "IF_LEFT", [ Seq (_, [ Seq (_, [ Prim (_, "UNIT", [], []) ;
                                                          Prim (_, "FAILWITH", [], []) ]) ]) ;
                                      Seq (_, []) ],
                      []) ]) ->
      Some (Prim (loc, "ASSERT_RIGHT", [], []))
  | Seq (loc, [ Prim (_, "IF_LEFT", [ Seq (_, [ Seq (_, [ Prim (_, "UNIT", [], []) ;
                                                          Prim (_, "FAILWITH", [], []) ]) ]) ;
                                      Seq (_, [ Prim (_, "RENAME", [], annot) ]) ],
                      []) ]) ->
      Some (Prim (loc, "ASSERT_RIGHT", [], annot))
  | _ -> None


let unexpand_if_some = function
  | Seq (loc, [ Prim (_, "IF_NONE", [ left ; right ], annot) ]) ->
      Some (Prim (loc, "IF_SOME", [ right ; left ], annot))
  | _ -> None

let unexpand_if_right = function
  | Seq (loc, [ Prim (_, "IF_LEFT", [ left ; right ], annot) ]) ->
      Some (Prim (loc, "IF_RIGHT", [ right ; left ], annot))
  | _ -> None

let unexpand_fail = function
  | Seq (loc, [
      Prim (_, "UNIT", [], []) ;
      Prim (_, "FAILWITH", [], []) ;
    ]) ->
      Some (Prim (loc, "FAIL", [], []))
  | _ -> None

let unexpand original =
  let try_unexpansions unexpanders =
    match
      List.fold_left
        (fun acc f ->
           match acc with
           | None -> f original
           | Some rewritten -> Some rewritten)
        None unexpanders with
    | None -> original
    | Some rewritten -> rewritten in
  try_unexpansions
    [ unexpand_asserts ;
      unexpand_caddadr ;
      unexpand_set_caddadr ;
      unexpand_map_caddadr ;
      unexpand_dxiiivp ;
      unexpand_pappaiir ;
      unexpand_unpappaiir ;
      unexpand_duuuuup ;
      unexpand_compare ;
      unexpand_if_some ;
      unexpand_if_right ;
      unexpand_fail ]

(*
   If an argument of Prim is a sequence, we do not want to unexpand
   its root in case the source already contains an expanded macro. In
   which case unexpansion would remove surrounding braces and generate
   ill-formed code.

   For example, DIIP { DIP { DUP }; SWAP } is not unexpandable but
   DIIP {{ DIP { DUP }; SWAP }} (note the double braces) is unexpanded
   to DIIP { DUUP }.

   unexpand_rec_but_root is the same as unexpand_rec but does not try
   to unexpand at root *)

let rec unexpand_rec expr =
  unexpand_rec_but_root (unexpand expr)

and unexpand_rec_but_root = function
  | Seq (loc, items) ->
      Seq (loc, List.map unexpand_rec items)
  | Prim (loc, name, args, annot) ->
      Prim (loc, name, List.map unexpand_rec_but_root args, annot)
  | Int _ | String _ | Bytes _ as atom -> atom

let () =
  let open Data_encoding in
  let open Proto_alpha in
  register_error_kind
    `Permanent
    ~id:"michelson.macros.unexpected_annotation"
    ~title:"Unexpected annotation"
    ~description:"A macro had an annotation, but no annotation was permitted on this macro."
    ~pp:(fun ppf ->
        Format.fprintf ppf
          "Unexpected annotation on macro %s.")
    (obj1
       (req "macro_name" string))
    (function
      | Unexpected_macro_annotation str -> Some str
      | _ -> None)
    (fun s -> Unexpected_macro_annotation s) ;
  register_error_kind
    `Permanent
    ~id:"michelson.macros.sequence_expected"
    ~title:"Macro expects a sequence"
    ~description:"An macro expects a sequence, but a sequence was not provided"
    ~pp:(fun ppf name ->
        Format.fprintf ppf
          "Macro %s expects a sequence, but did not receive one." name)
    (obj1
       (req "macro_name" string))
    (function
      | Sequence_expected name -> Some name
      | _ -> None)
    (fun name -> Sequence_expected name) ;
  register_error_kind
    `Permanent
    ~id:"michelson.macros.bas_arity"
    ~title:"Wrong number of arguments to macro"
    ~description:"A wrong number of arguments was provided to a macro"
    ~pp:(fun ppf (name, got, exp) ->
        Format.fprintf ppf
          "Macro %s expects %d arguments, was given %d." name got exp)
    (obj3
       (req "macro_name" string)
       (req "given_number_of_arguments" uint16)
       (req "expected_number_of_arguments" uint16))
    (function
      | Invalid_arity (name, got, exp) -> Some (name, got, exp)
      | _ -> None)
    (fun (name, got, exp) -> Invalid_arity (name, got, exp))
