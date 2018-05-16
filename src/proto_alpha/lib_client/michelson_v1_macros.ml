(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_micheline
open Micheline

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
            let annot = if i = (String.length str - 2) then annot else [] in
            match String.get str i with
            | 'A' -> parse (i - 1) [] (Prim (loc, "CAR", [], annot) :: acc)
            | 'D' -> parse (i - 1) [] (Prim (loc, "CDR", [], annot) :: acc)
            | _ -> assert false in
        ok (Some (parse (len - 2) annot []))
      else
        ok None
  | _ -> ok None

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
        let rec parse i acc =
          if i = 4 then
            acc
          else
            match String.get str i with
            | 'A' ->
                let acc =
                  Seq (loc,
                       [ Prim (loc, "DUP", [], []) ;
                         Prim (loc, "DIP",
                               [ Seq (loc,
                                      [ Prim (loc, "CAR", [], []) ;
                                        acc ]) ], []) ;
                         Prim (loc, "CDR", [], []) ;
                         Prim (loc, "SWAP", [], []) ;
                         Prim (loc, "PAIR", [], []) ]) in
                parse (i - 1) acc
            | 'D' ->
                let acc =
                  Seq (loc,
                       [ Prim (loc, "DUP", [], []) ;
                         Prim (loc, "DIP",
                               [ Seq (loc,
                                      [ Prim (loc, "CDR", [], []) ;
                                        acc ]) ], []) ;
                         Prim (loc, "CAR", [], []) ;
                         Prim (loc, "PAIR", [], []) ]) in
                parse (i - 1) acc
            | _ -> assert false in
        match String.get str (len - 2) with
        | 'A' ->
            let init =
              Seq (loc,
                   [ Prim (loc, "CDR", [], []) ;
                     Prim (loc, "SWAP", [], annot) ;
                     Prim (loc, "PAIR", [], []) ]) in
            ok (Some (parse (len - 3) init))
        | 'D' ->
            let init =
              Seq (loc,
                   (Prim (loc, "CAR", [], [])) ::
                   (let pair = Prim (loc, "PAIR", [], []) in
                    match annot with
                    | [] -> [ pair ]
                    | _ -> [ Prim (loc, "SWAP", [], annot) ;
                             Prim (loc, "SWAP", [], []) ;
                             pair])) in
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
        begin match annot with
          | _ :: _ -> (error (Unexpected_macro_annotation str))
          | [] -> ok ()
        end >>? fun () ->
        begin match args with
          | [ Seq _ as code ] -> ok code
          | [ _ ] -> error (Sequence_expected str)
          | [] | _ :: _ :: _ -> error (Invalid_arity (str, List.length args, 1))
        end >>? fun code ->
        let rec parse i acc =
          if i = 4 then
            acc
          else
            match String.get str i with
            | 'A' ->
                let acc =
                  Seq (loc,
                       [ Prim (loc, "DUP", [], []) ;
                         Prim (loc, "DIP",
                               [ Seq (loc,
                                      [ Prim (loc, "CAR", [], []) ;
                                        acc ]) ], []) ;
                         Prim (loc, "CDR", [], []) ;
                         Prim (loc, "SWAP", [], []) ;
                         Prim (loc, "PAIR", [], []) ]) in
                parse (i - 1) acc
            | 'D' ->
                let acc =
                  Seq (loc,
                       [ Prim (loc, "DUP", [], []) ;
                         Prim (loc, "DIP",
                               [ Seq (loc,
                                      [ Prim (loc, "CDR", [], []) ;
                                        acc ]) ], []) ;
                         Prim (loc, "CAR", [], []) ;
                         Prim (loc, "PAIR", [], []) ]) in
                parse (i - 1) acc
            | _ -> assert false in
        match String.get str (len - 2) with
        | 'A' ->
            let init =
              Seq (loc,
                   [ Prim (loc, "DUP", [], []) ;
                     Prim (loc, "CDR", [], []) ;
                     Prim (loc, "DIP",
                           [ Seq (loc, [ Prim (loc, "CAR", [], []) ; code ]) ], []) ;
                     Prim (loc, "SWAP", [], []) ;
                     Prim (loc, "PAIR", [], []) ]) in
            ok (Some (parse (len - 3) init))
        | 'D' ->
            let init =
              Seq (loc,
                   [ Prim (loc, "DUP", [], []) ;
                     Prim (loc, "CDR", [], []) ;
                     code ;
                     Prim (loc, "SWAP", [], []) ;
                     Prim (loc, "CAR", [], []) ;
                     Prim (loc, "PAIR", [], []) ]) in
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

let expand_paaiair original =
  match original with
  | Prim (loc, str, args, annot) ->
      let len = String.length str in
      if len > 4
      && String.get str 0 = 'P'
      && String.get str (len - 1) = 'R'
      && check_letters str 1 (len - 2)
           (function 'A' | 'I' -> true | _ -> false) then
        try
          let rec parse i acc =
            if i = 0 then
              acc
            else if String.get str i = 'I'
                 && String.get str (i - 1) = 'A' then
              parse (i - 2) (Prim (loc, "PAIR", [], if i = (len - 2) then annot else []) :: acc)
            else if String.get str i = 'A' then
              match acc with
              | [] ->
                  raise_notrace Not_a_pair
              | acc :: accs ->
                  parse (i - 1)
                    (Prim (loc, "DIP", [ Seq (loc, [ acc ]) ], [])
                     :: accs)
            else
              raise_notrace Not_a_pair in
          let expanded = parse (len - 2) [] in
          begin match args with
            | [] -> ok ()
            | _ :: _ -> error (Invalid_arity (str, List.length args, 0))
          end >>? fun () ->
          ok (Some (Seq (loc, expanded)))
        with Not_a_pair -> ok None
      else
        ok None
  | _ -> ok None

let expand_unpaaiair original =
  match original with
  | Prim (loc, str, args, annot) ->
      let len = String.length str in
      if len >= 6
      && String.sub str 0 3 = "UNP"
      && String.get str (len - 1) = 'R'
      && check_letters str 3 (len - 2)
           (function 'A' | 'I' -> true | _ -> false) then
        try
          let rec parse i remaining_annots acc =
            if i = 2 then
              match acc with
              | [ Seq _ as acc ] -> acc
              | _ -> Seq (loc, List.rev acc)
            else if String.get str i = 'I'
                 && String.get str (i - 1) = 'A' then
              let car_annot, cdr_annot, remaining_annots =
                match remaining_annots with
                | [] -> [], [], []
                | a :: b :: r when i = 4 -> [ a ], [ b ], r
                | a :: r -> [ a ], [], r in
              parse (i - 2) remaining_annots
                (Seq (loc, [ Prim (loc, "DUP", [], []) ;
                             Prim (loc, "CAR", [], car_annot) ;
                             Prim (loc, "DIP",
                                   [ Seq (loc,
                                          [ Prim (loc, "CDR", [], cdr_annot) ]) ], []) ])
                 :: acc)
            else if String.get str i = 'A' then
              match acc with
              | [] ->
                  raise_notrace Not_a_pair
              | (Seq _ as acc) :: accs ->
                  parse (i - 1) remaining_annots
                    (Prim (loc, "DIP", [ acc ], []) :: accs)
              | acc :: accs ->
                  parse (i - 1) remaining_annots
                    (Prim (loc, "DIP",
                           [ Seq (loc, [ acc ]) ],
                           []) :: accs)
            else
              raise_notrace Not_a_pair in
          let expanded = parse (len - 2) annot [] in
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
  let cmp loc is =
    let is =
      List.map (fun i -> Prim (loc, i, [], [])) is in
    ok (Some (Seq (loc, is))) in
  let ifcmp loc is l r =
    let is =
      List.map (fun i -> Prim (loc, i, [], [])) is @
      [ Prim (loc, "IF", [ l ; r ], []) ] in
    ok (Some (Seq (loc, is))) in
  match original with
  | Prim (loc, "CMPEQ", [], []) ->
      cmp loc [ "COMPARE" ; "EQ" ]
  | Prim (loc, "CMPNEQ", [], []) ->
      cmp loc [ "COMPARE" ; "NEQ" ]
  | Prim (loc, "CMPLT", [], []) ->
      cmp loc [ "COMPARE" ; "LT" ]
  | Prim (loc, "CMPGT", [], []) ->
      cmp loc [ "COMPARE" ; "GT" ]
  | Prim (loc, "CMPLE", [], []) ->
      cmp loc [ "COMPARE" ; "LE" ]
  | Prim (loc, "CMPGE", [], []) ->
      cmp loc [ "COMPARE" ; "GE" ]
  | Prim (_, ("CMPEQ" |  "CMPNEQ" |  "CMPLT"
             |  "CMPGT" |  "CMPLE" | "CMPGE" as str), args, []) ->
      error (Invalid_arity (str, List.length args, 0))
  | Prim (loc, "IFCMPEQ", [ l ; r ], []) ->
      ifcmp loc [ "COMPARE" ; "EQ" ] l r
  | Prim (loc, "IFCMPNEQ", [ l ; r ], []) ->
      ifcmp loc [ "COMPARE" ; "NEQ" ] l r
  | Prim (loc, "IFCMPLT", [ l ; r ], []) ->
      ifcmp loc [ "COMPARE" ; "LT" ] l r
  | Prim (loc, "IFCMPGT", [ l ; r ], []) ->
      ifcmp loc [ "COMPARE" ; "GT" ] l r
  | Prim (loc, "IFCMPLE", [ l ; r ], []) ->
      ifcmp loc [ "COMPARE" ; "LE" ] l r
  | Prim (loc, "IFCMPGE", [ l ; r ], []) ->
      ifcmp loc [ "COMPARE" ; "GE" ] l r
  | Prim (loc, "IFEQ", [ l ; r ], []) ->
      ifcmp loc [ "EQ" ] l r
  | Prim (loc, "IFNEQ", [ l ; r ], []) ->
      ifcmp loc [ "NEQ" ] l r
  | Prim (loc, "IFLT", [ l ; r ], []) ->
      ifcmp loc [ "LT" ] l r
  | Prim (loc, "IFGT", [ l ; r ], []) ->
      ifcmp loc [ "GT" ] l r
  | Prim (loc, "IFLE", [ l ; r ], []) ->
      ifcmp loc [ "LE" ] l r
  | Prim (loc, "IFGE", [ l ; r ], []) ->
      ifcmp loc [ "GE" ] l r
  | Prim (_, ("IFCMPEQ" | "IFCMPNEQ" | "IFCMPLT"
             | "IFCMPGT" | "IFCMPLE" | "IFCMPGE"
             | "IFEQ" | "IFNEQ" | "IFLT"
             | "IFGT" | "IFLE" | "IFGE" as str), args, []) ->
      error (Invalid_arity (str, List.length args, 2))
  | Prim (_, ("IFCMPEQ" | "IFCMPNEQ" | "IFCMPLT"
             | "IFCMPGT" | "IFCMPLE" | "IFCMPGE"
             | "IFEQ" | "IFNEQ" | "IFLT"
             | "IFGT" | "IFLE" | "IFGE"
             | "CMPEQ" |  "CMPNEQ" |  "CMPLT"
             | "CMPGT" |  "CMPLE" | "CMPGE" as str), [], _ :: _) ->
      error (Unexpected_macro_annotation str)
  | _ -> ok None

let expand_asserts original =
  let fail_false loc =
    [ Seq(loc, []) ; Seq(loc, [ Prim (loc, "FAIL", [], []) ]) ] in
  let fail_true loc =
    [ Seq(loc, [ Prim (loc, "FAIL", [], []) ]) ; Seq(loc, []) ] in
  match original with
  | Prim (loc, "ASSERT", [], []) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF", fail_false loc, []) ]))
  | Prim (loc, "ASSERT_NONE", [], []) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_NONE", fail_false loc, []) ]))
  | Prim (loc, "ASSERT_SOME", [], []) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_NONE", fail_true loc, []) ]))
  | Prim (loc, "ASSERT_LEFT", [], []) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_LEFT", fail_false loc, []) ]))
  | Prim (loc, "ASSERT_RIGHT", [], []) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_LEFT", fail_true loc, []) ]))
  | Prim (_, ("ASSERT" | "ASSERT_NONE" | "ASSERT_SOME"
             | "ASSERT_LEFT" | "ASSERT_RIGHT" as str), args, []) ->
      error (Invalid_arity (str, List.length args, 0))
  | Prim (_, ("ASSERT" | "ASSERT_NONE" | "ASSERT_SOME"
             | "ASSERT_LEFT" | "ASSERT_RIGHT" as str), [], _ :: _) ->
      error (Unexpected_macro_annotation str)
  | Prim (loc, s, args, annot)
    when String.(length s >  7 && equal (sub s 0 7) "ASSERT_") ->
      begin match args with
        | [] -> ok ()
        | _ :: _ -> error (Invalid_arity (s, List.length args, 0))
      end >>? fun () ->
      begin match annot with
        | _ :: _ -> (error (Unexpected_macro_annotation s))
        | [] -> ok () end >>? fun () ->
      begin
        let remaining = String.(sub s 7 ((length s) - 7)) in
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
  | Prim (loc, "IF_SOME", [ right ; left ], []) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_NONE", [ left ; right ], []) ]))
  | Prim (_, "IF_SOME", args, []) ->
      error (Invalid_arity ("IF_SOME", List.length args, 2))
  | Prim (_, "IF_SOME", [], _ :: _) ->
      error (Unexpected_macro_annotation "IF_SOME")
  | _ -> ok @@ None

let expand_if_right = function
  | Prim (loc, "IF_RIGHT", [ right ; left ], []) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_LEFT", [ left ; right ], []) ]))
  | Prim (_, "IF_RIGHT", args, []) ->
      error (Invalid_arity ("IF_RIGHT", List.length args, 2))
  | Prim (_, "IF_RIGHT", [], _ :: _) ->
      error (Unexpected_macro_annotation "IF_RIGHT")
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
      expand_paaiair ;
      expand_unpaaiair ;
      expand_duuuuup ;
      expand_compare ;
      expand_asserts ;
      expand_if_some ;
      expand_if_right ]

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
          | Int _ | String _ as atom -> (atom, []) end
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
  let rec steps acc = function
    | Seq (loc,
           [ Prim (_, "CDR", [], []) ;
             Prim (_, "SWAP", [], []) ;
             Prim (_, "PAIR", [], []) ]) ->
        Some (loc, "A" :: acc)
    | Seq (loc,
           [ Prim (_, "CAR", [], []) ;
             Prim (_, "PAIR", [], []) ]) ->
        Some (loc, "D" :: acc)
    | Seq (_,
           [ Prim (_, "DUP", [], []) ;
             Prim (_, "DIP",
                   [ Seq (_,
                          [ Prim (_, "CAR", [], []) ;
                            sub ]) ], []) ;
             Prim (_, "CDR", [], []) ;
             Prim (_, "SWAP", [], []) ;
             Prim (_, "PAIR", [], []) ]) ->
        steps ("A" :: acc) sub
    | Seq (_,
           [ Prim (_, "DUP", [], []) ;
             Prim (_, "DIP",
                   [ Seq (_,
                          [ Prim (_, "CDR", [], []) ;
                            sub ]) ], []) ;
             Prim (_, "CAR", [], []) ;
             Prim (_, "PAIR", [], []) ]) ->
        steps ("D" :: acc) sub
    | _ -> None in
  match steps [] expanded with
  | Some (loc, steps) ->
      let name = String.concat "" ("SET_C" :: List.rev ("R" :: steps)) in
      Some (Prim (loc, name, [], []))
  | None -> None

let unexpand_map_caddadr expanded =
  let rec steps acc = function
    | Seq (loc,
           [ Prim (_, "DUP", [], []) ;
             Prim (_, "CDR", [], []) ;
             Prim (_, "SWAP", [], []) ;
             Prim (_, "CAR", [], []) ;
             code ;
             Prim (_, "PAIR", [], []) ]) ->
        Some (loc, "A" :: acc, code)
    | Seq (loc,
           [ Prim (_, "DUP", [], []) ;
             Prim (_, "CDR", [], []) ;
             code ;
             Prim (_, "SWAP", [], []) ;
             Prim (_, "CAR", [], []) ;
             Prim (_, "PAIR", [], []) ]) ->
        Some (loc, "D" :: acc, code)
    | Seq (_,
           [ Prim (_, "DUP", [], []) ;
             Prim (_, "DIP",
                   [ Seq (_,
                          [ Prim (_, "CAR", [], []) ;
                            sub ]) ], []) ;
             Prim (_, "CDR", [], []) ;
             Prim (_, "SWAP", [], []) ;
             Prim (_, "PAIR", [], []) ]) ->
        steps ("A" :: acc) sub
    | Seq (_,
           [ Prim (_, "DUP", [], []) ;
             Prim (_, "DIP",
                   [ Seq (_,
                          [ Prim (_, "CDR", [], []) ;
                            sub ]) ], []) ;
             Prim (_, "CAR", [], []) ;
             Prim (_, "PAIR", [], []) ]) ->
        steps ("D" :: acc) sub
    | _ -> None in
  match steps [] expanded with
  | Some (loc, steps, code) ->
      let name = String.concat "" ("MAP_C" :: List.rev ("R" :: steps)) in
      Some (Prim (loc, name, [ code ], []))
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
      let name = "D" ^ roman_of_decimal depth ^ "P" in
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

let unexpand_paaiair expanded =
  match expanded with
  | Seq (_,  [ Prim (_, "PAIR", [], []) ]) -> Some expanded
  | Seq (loc, (_ :: _ as nodes)) ->
      let rec destruct acc = function
        | [] -> Some acc
        | Prim (_, "DIP", [ Seq (_, [ sub ]) ], []) :: rest ->
            destruct ("A" :: acc) (sub :: rest)
        | Prim (_, "PAIR", [], []) :: rest ->
            destruct ("AI" :: acc) rest
        | _ -> None in
      begin match destruct [] nodes with
        | None -> None
        | Some seq ->
            let name = String.concat "" ("P" :: List.rev ("R" :: seq)) in
            Some (Prim (loc, name, [], []))
      end
  | _ -> None

let unexpand_unpaaiair expanded =
  match expanded with
  | Seq (loc, (_ :: _ as nodes)) ->
      let rec destruct sacc acc = function
        | [] -> Some acc
        | Prim (_, "DIP", [ Seq (_, [ sub ]) ], []) :: rest
        | Prim (_, "DIP", [ Seq (_, _) as sub ], []) :: rest ->
            destruct ("A" :: sacc) acc (sub :: rest)
        | Seq (_, [ Prim (_, "DUP", [], []) ;
                    Prim (_, "CAR", [], []) ;
                    Prim (_, "DIP",
                          [ Seq (_, [ Prim (_, "CDR", [], []) ]) ],
                          []) ]) :: rest ->
            destruct [] (List.rev ("AI" :: sacc) :: acc) rest
        | _ -> None in
      begin match destruct [] [ [ "R" ] ] nodes with
        | None -> None
        | Some seq ->
            let name = String.concat "" ("UNP" :: List.flatten seq) in
            Some (Prim (loc, name, [], []))
      end
  | _ -> None

let unexpand_compare expanded =
  match expanded with
  | Seq (loc, [ Prim (_, "COMPARE", [], []) ;
                Prim (_, "EQ", [], []) ]) ->
      Some (Prim (loc, "CMPEQ", [], []))
  | Seq (loc, [ Prim (_, "COMPARE", [], []) ;
                Prim (_, "NEQ", [], []) ]) ->
      Some (Prim (loc, "CMPNEQ", [], []))
  | Seq (loc, [ Prim (_, "COMPARE", [], []) ;
                Prim (_, "LT", [], []) ]) ->
      Some (Prim (loc, "CMPLT", [], []))
  | Seq (loc, [ Prim (_, "COMPARE", [], []) ;
                Prim (_, "GT", [], []) ]) ->
      Some (Prim (loc, "CMPGT", [], []))
  | Seq (loc, [ Prim (_, "COMPARE", [], []) ;
                Prim (_, "LE", [], []) ]) ->
      Some (Prim (loc, "CMPLE", [], []))
  | Seq (loc, [ Prim (_, "COMPARE", [], []) ;
                Prim (_, "GE", [], []) ]) ->
      Some (Prim (loc, "CMPGE", [], []))
  | Seq (loc, [ Prim (_, "COMPARE", [], []) ;
                Prim (_, "EQ", [], []) ;
                Prim (_, "IF", args, []) ]) ->
      Some (Prim (loc, "IFCMPEQ", args, []))
  | Seq (loc, [ Prim (_, "COMPARE", [], []) ;
                Prim (_, "NEQ", [], []) ;
                Prim (_, "IF", args, []) ]) ->
      Some (Prim (loc, "IFCMPNEQ", args, []))
  | Seq (loc, [ Prim (_, "COMPARE", [], []) ;
                Prim (_, "LT", [], []) ;
                Prim (_, "IF", args, []) ]) ->
      Some (Prim (loc, "IFCMPLT", args, []))
  | Seq (loc, [ Prim (_, "COMPARE", [], []) ;
                Prim (_, "GT", [], []) ;
                Prim (_, "IF", args, []) ]) ->
      Some (Prim (loc, "IFCMPGT", args, []))
  | Seq (loc, [ Prim (_, "COMPARE", [], []) ;
                Prim (_, "LE", [], []) ;
                Prim (_, "IF", args, []) ]) ->
      Some (Prim (loc, "IFCMPLE", args, []))
  | Seq (loc, [ Prim (_, "COMPARE", [], []) ;
                Prim (_, "GE", [], []) ;
                Prim (_, "IF", args, []) ]) ->
      Some (Prim (loc, "IFCMPGE", args, []))
  | Seq (loc, [ Prim (_, "EQ", [], []) ;
                Prim (_, "IF", args, []) ]) ->
      Some (Prim (loc, "IFEQ", args, []))
  | Seq (loc, [ Prim (_, "NEQ", [], []) ;
                Prim (_, "IF", args, []) ]) ->
      Some (Prim (loc, "IFNEQ", args, []))
  | Seq (loc, [ Prim (_, "LT", [], []) ;
                Prim (_, "IF", args, []) ]) ->
      Some (Prim (loc, "IFLT", args, []))
  | Seq (loc, [ Prim (_, "GT", [], []) ;
                Prim (_, "IF", args, []) ]) ->
      Some (Prim (loc, "IFGT", args, []))
  | Seq (loc, [ Prim (_, "LE", [], []) ;
                Prim (_, "IF", args, []) ]) ->
      Some (Prim (loc, "IFLE", args, []))
  | Seq (loc, [ Prim (_, "GE", [], []) ;
                Prim (_, "IF", args, []) ]) ->
      Some (Prim (loc, "IFGE", args, []))
  | _ -> None

let unexpand_asserts expanded =
  match expanded with
  | Seq (loc, [ Prim (_, "IF", [ Seq (_, []) ;
                                 Seq (_, [ Prim(_, "FAIL", [], []) ]) ],
                      []) ]) ->
      Some (Prim (loc, "ASSERT", [], []))
  | Seq (loc, [ Seq (_, [ Prim(_, "COMPARE", [], []) ; Prim (_, comparison, [], []) ]) ;
                Prim (_, "IF", [ Seq (_, []) ;
                                 Seq (_, [ Prim (_, "FAIL", [], []) ]) ],
                      []) ]) ->
      Some (Prim (loc, "ASSERT_CMP" ^ comparison, [], []))
  | Seq (loc, [ Prim (_, comparison, [], []) ;
                Prim (_, "IF", [ Seq (_, []) ;
                                 Seq (_, [ Prim (_, "FAIL", [], []) ]) ],
                      []) ]) ->
      Some (Prim (loc, "ASSERT_" ^ comparison, [], []))
  | Seq (loc, [ Prim (_, "IF_NONE", [ Seq (_, []) ;
                                      Seq (_, [ Prim (_, "FAIL", [], []) ]) ],
                      []) ]) ->
      Some (Prim (loc, "ASSERT_NONE", [], []))
  | Seq (loc, [ Prim (_, "IF_NONE", [ Seq (_, [ Prim (_, "FAIL", [], []) ]) ;
                                      Seq (_, [])],
                      []) ]) ->
      Some (Prim (loc, "ASSERT_SOME", [], []))
  | Seq (loc, [ Prim (_, "IF_LEFT", [ Seq (_, []) ;
                                      Seq (_, [ Prim (_, "FAIL", [], []) ]) ],
                      []) ]) ->
      Some (Prim (loc, "ASSERT_LEFT", [], []))
  | Seq (loc, [ Prim (_, "IF_LEFT", [ Seq (_, [ Prim (_, "FAIL", [], []) ]) ;
                                      Seq (_, []) ],
                      []) ]) ->
      Some (Prim (loc, "ASSERT_RIGHT", [], []))
  | _ -> None


let unexpand_if_some = function
  | Seq (loc, [ Prim (_, "IF_NONE", [ left ; right ], []) ]) ->
      Some (Prim (loc, "IF_SOME", [ right ; left ], []))
  | _ -> None

let unexpand_if_right = function
  | Seq (loc, [ Prim (_, "IF_LEFT", [ left ; right ], []) ]) ->
      Some (Prim (loc, "IF_RIGHT", [ right ; left ], []))
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
      unexpand_paaiair ;
      unexpand_unpaaiair ;
      unexpand_duuuuup ;
      unexpand_compare ;
      unexpand_if_some ;
      unexpand_if_right ]

let rec unexpand_rec expr =
  match unexpand expr with
  | Seq (loc, items) ->
      Seq (loc, List.map unexpand_rec items)
  | Prim (loc, name, args, annot) ->
      Prim (loc, name, List.map unexpand_rec args, annot)
  | Int _ | String _ as atom -> atom

let () =
  let open Data_encoding in
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
