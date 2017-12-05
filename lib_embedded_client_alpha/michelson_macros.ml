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
        let rec parse i ?annot acc =
          if i = 0 then
            Seq (loc, acc, None)
          else
            let annot = if i = (String.length str - 2) then annot else None in
            match String.get str i with
            | 'A' -> parse (i - 1) (Prim (loc, "CAR", [], annot) :: acc)
            | 'D' -> parse (i - 1) (Prim (loc, "CDR", [], annot) :: acc)
            | _ -> assert false in
        ok (Some (parse (len - 2) ?annot []))
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
                       [ Prim (loc, "DUP", [], None) ;
                         Prim (loc, "DIP",
                               [ Seq (loc,
                                      [ Prim (loc, "CAR", [], None) ;
                                        acc ], None) ], None) ;
                         Prim (loc, "CDR", [], None) ;
                         Prim (loc, "SWAP", [], None) ;
                         Prim (loc, "PAIR", [], None) ], None) in
                parse (i - 1) acc
            | 'D' ->
                let acc =
                  Seq (loc,
                       [ Prim (loc, "DUP", [], None) ;
                         Prim (loc, "DIP",
                               [ Seq (loc,
                                      [ Prim (loc, "CDR", [], None) ;
                                        acc ], None) ], None) ;
                         Prim (loc, "CAR", [], None) ;
                         Prim (loc, "PAIR", [], None) ], None) in
                parse (i - 1) acc
            | _ -> assert false in
        match String.get str (len - 2) with
        | 'A' ->
            let init =
              Seq (loc,
                   [ Prim (loc, "CDR", [], None) ;
                     Prim (loc, "SWAP", [], annot) ;
                     Prim (loc, "PAIR", [], None) ], None) in
            ok (Some (parse (len - 3) init))
        | 'D' ->
            let init =
              Seq (loc,
                   (Prim (loc, "CAR", [], None)) ::
                   (let pair = Prim (loc, "PAIR", [], None) in
                    match annot with
                    | None -> [ pair ]
                    | Some _ -> [ Prim (loc, "SWAP", [], annot) ;
                                  Prim (loc, "SWAP", [], None) ;
                                  pair]), None) in
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
          | Some _ -> (error (Unexpected_macro_annotation str))
          | None -> ok ()
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
                       [ Prim (loc, "DUP", [], None) ;
                         Prim (loc, "DIP",
                               [ Seq (loc,
                                      [ Prim (loc, "CAR", [], None) ;
                                        acc ], None) ], None) ;
                         Prim (loc, "CDR", [], None) ;
                         Prim (loc, "SWAP", [], None) ;
                         Prim (loc, "PAIR", [], None) ], None) in
                parse (i - 1) acc
            | 'D' ->
                let acc =
                  Seq (loc,
                       [ Prim (loc, "DUP", [], None) ;
                         Prim (loc, "DIP",
                               [ Seq (loc,
                                      [ Prim (loc, "CDR", [], None) ;
                                        acc ], None) ], None) ;
                         Prim (loc, "CAR", [], None) ;
                         Prim (loc, "PAIR", [], None) ], None) in
                parse (i - 1) acc
            | _ -> assert false in
        match String.get str (len - 2) with
        | 'A' ->
            let init =
              Seq (loc,
                   [ Prim (loc, "DUP", [], None) ;
                     Prim (loc, "CDR", [], None) ;
                     Prim (loc, "DIP",
                           [ Seq (loc, [ Prim (loc, "CAR", [], None) ; code ], None) ], None) ;
                     Prim (loc, "SWAP", [], None) ;
                     Prim (loc, "PAIR", [], None) ], None) in
            ok (Some (parse (len - 3) init))
        | 'D' ->
            let init =
              Seq (loc,
                   [ Prim (loc, "DUP", [], None) ;
                     Prim (loc, "CDR", [], None) ;
                     code ;
                     Prim (loc, "SWAP", [], None) ;
                     Prim (loc, "CAR", [], None) ;
                     Prim (loc, "PAIR", [], None) ], None) in
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
                (Seq (loc, [ Prim (loc, "DIP", [ acc ], annot) ], None)) in
          match args with
          | [ Seq (_, _, _) as arg ] -> ok @@ Some (make depth arg)
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
              parse (i - 2) (Prim (loc, "PAIR", [], if i = (len - 2) then annot else None) :: acc)
            else if String.get str i = 'A' then
              match acc with
              | [] ->
                  raise_notrace Not_a_pair
              | acc :: accs ->
                  parse (i - 1)
                    (Prim (loc, "DIP", [ Seq (loc, [ acc ], None) ], None)
                     :: accs)
            else
              raise_notrace Not_a_pair in
          let expanded = parse (len - 2) [] in
          begin match args with
            | [] -> ok ()
            | _ :: _ -> error (Invalid_arity (str, List.length args, 0))
          end >>? fun () ->
          ok (Some (Seq (loc, expanded, None)))
        with Not_a_pair -> ok None
      else
        ok None
  | _ -> ok None

let expand_unpaaiair original =
  match original with
  | Prim (loc, str, args, None) ->
      let len = String.length str in
      if len >= 6
      && String.sub str 0 3 = "UNP"
      && String.get str (len - 1) = 'R'
      && check_letters str 3 (len - 2)
           (function 'A' | 'I' -> true | _ -> false) then
        try
          let rec parse i acc =
            if i = 2 then
              match acc with
              | [ Seq _ as acc ] -> acc
              | _ -> Seq (loc, List.rev acc, None)
            else if String.get str i = 'I'
                 && String.get str (i - 1) = 'A' then
              parse (i - 2)
                (Seq (loc, [ Prim (loc, "DUP", [], None) ;
                             Prim (loc, "CAR", [], None) ;
                             Prim (loc, "DIP",
                                   [ Seq (loc,
                                          [ Prim (loc, "CDR", [], None) ],
                                          None) ], None) ], None)
                 :: acc)
            else if String.get str i = 'A' then
              match acc with
              | [] ->
                  raise_notrace Not_a_pair
              | (Seq _ as acc) :: accs ->
                  parse (i - 1)
                    (Prim (loc, "DIP", [ acc ], None) :: accs)
              | acc :: accs ->
                  parse (i - 1)
                    (Prim (loc, "DIP",
                           [ Seq (loc, [ acc ], None) ],
                           None) :: accs)
            else
              raise_notrace Not_a_pair in
          let expanded = parse (len - 2) [] in
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
                (Seq (loc, [ Prim (loc, "DIP", [ acc ], None) ;
                             Prim (loc, "SWAP", [], None) ], None))
            else
              raise_notrace Not_a_dup in
          ok (Some (parse (len - 2) (Seq (loc, [ Prim (loc, "DUP", [], annot) ], None))))
        with Not_a_dup -> ok None
      else
        ok None
  | _ -> ok None

let expand_compare original =
  let cmp loc is =
    let is =
      List.map (fun i -> Prim (loc, i, [], None)) is in
    ok (Some (Seq (loc, is, None))) in
  let ifcmp loc is l r =
    let is =
      List.map (fun i -> Prim (loc, i, [], None)) is @
      [ Prim (loc, "IF", [ l ; r ], None) ] in
    ok (Some (Seq (loc, is, None))) in
  match original with
  | Prim (loc, "CMPEQ", [], None) ->
      cmp loc [ "COMPARE" ; "EQ" ]
  | Prim (loc, "CMPNEQ", [], None) ->
      cmp loc [ "COMPARE" ; "NEQ" ]
  | Prim (loc, "CMPLT", [], None) ->
      cmp loc [ "COMPARE" ; "LT" ]
  | Prim (loc, "CMPGT", [], None) ->
      cmp loc [ "COMPARE" ; "GT" ]
  | Prim (loc, "CMPLE", [], None) ->
      cmp loc [ "COMPARE" ; "LE" ]
  | Prim (loc, "CMPGE", [], None) ->
      cmp loc [ "COMPARE" ; "GE" ]
  | Prim (_, ("CMPEQ" |  "CMPNEQ" |  "CMPLT"
               |  "CMPGT" |  "CMPLE" | "CMPGE" as str), args, None) ->
      error (Invalid_arity (str, List.length args, 0))
  | Prim (loc, "IFCMPEQ", [ l ; r ], None) ->
      ifcmp loc [ "COMPARE" ; "EQ" ] l r
  | Prim (loc, "IFCMPNEQ", [ l ; r ], None) ->
      ifcmp loc [ "COMPARE" ; "NEQ" ] l r
  | Prim (loc, "IFCMPLT", [ l ; r ], None) ->
      ifcmp loc [ "COMPARE" ; "LT" ] l r
  | Prim (loc, "IFCMPGT", [ l ; r ], None) ->
      ifcmp loc [ "COMPARE" ; "GT" ] l r
  | Prim (loc, "IFCMPLE", [ l ; r ], None) ->
      ifcmp loc [ "COMPARE" ; "LE" ] l r
  | Prim (loc, "IFCMPGE", [ l ; r ], None) ->
      ifcmp loc [ "COMPARE" ; "GE" ] l r
  | Prim (loc, "IFEQ", [ l ; r ], None) ->
      ifcmp loc [ "EQ" ] l r
  | Prim (loc, "IFNEQ", [ l ; r ], None) ->
      ifcmp loc [ "NEQ" ] l r
  | Prim (loc, "IFLT", [ l ; r ], None) ->
      ifcmp loc [ "LT" ] l r
  | Prim (loc, "IFGT", [ l ; r ], None) ->
      ifcmp loc [ "GT" ] l r
  | Prim (loc, "IFLE", [ l ; r ], None) ->
      ifcmp loc [ "LE" ] l r
  | Prim (loc, "IFGE", [ l ; r ], None) ->
      ifcmp loc [ "GE" ] l r
  | Prim (_, ("IFCMPEQ" | "IFCMPNEQ" | "IFCMPLT"
               | "IFCMPGT" | "IFCMPLE" | "IFCMPGE"
               | "IFEQ" | "IFNEQ" | "IFLT"
               | "IFGT" | "IFLE" | "IFGE" as str), args, None) ->
      error (Invalid_arity (str, List.length args, 2))
  | Prim (_, ("IFCMPEQ" | "IFCMPNEQ" | "IFCMPLT"
               | "IFCMPGT" | "IFCMPLE" | "IFCMPGE"
               | "IFEQ" | "IFNEQ" | "IFLT"
               | "IFGT" | "IFLE" | "IFGE"
               | "CMPEQ" |  "CMPNEQ" |  "CMPLT"
               | "CMPGT" |  "CMPLE" | "CMPGE" as str), [], Some _) ->
      error (Unexpected_macro_annotation str)
  | _ -> ok None

let expand_asserts original =
  let fail_false loc =
    [ Seq(loc, [], None) ; Seq(loc, [ Prim (loc, "FAIL", [], None) ], None) ] in
  let fail_true loc =
    [ Seq(loc, [ Prim (loc, "FAIL", [], None) ], None) ; Seq(loc, [], None) ] in
  match original with
  | Prim (loc, "ASSERT", [], None) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF", fail_false loc, None) ], None))
  | Prim (loc, "ASSERT_NONE", [], None) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_NONE", fail_false loc, None) ], None))
  | Prim (loc, "ASSERT_SOME", [], None) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_NONE", fail_true loc, None) ], None))
  | Prim (loc, "ASSERT_LEFT", [], None) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_LEFT", fail_false loc, None) ], None))
  | Prim (loc, "ASSERT_RIGHT", [], None) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_LEFT", fail_true loc, None) ], None))
  | Prim (_, ("ASSERT" | "ASSERT_NONE" | "ASSERT_SOME"
             | "ASSERT_LEFT" | "ASSERT_RIGHT" as str), args, None) ->
      error (Invalid_arity (str, List.length args, 0))
  | Prim (_, ("ASSERT" | "ASSERT_NONE" | "ASSERT_SOME"
             | "ASSERT_LEFT" | "ASSERT_RIGHT" as str), [], Some _) ->
      error (Unexpected_macro_annotation str)
  | Prim (loc, s, args, annot)
    when String.(length s >  7 && equal (sub s 0 7) "ASSERT_") ->
      begin match args with
        | [] -> ok ()
        | _ :: _ -> error (Invalid_arity (s, List.length args, 0))
      end >>? fun () ->
      begin match annot with
        | Some _ -> (error (Unexpected_macro_annotation s))
        | None -> ok () end >>? fun () ->
      begin
        let remaining = String.(sub s 7 ((length s) - 7)) in
        let remaining_prim = Prim(loc, remaining, [], None) in
        match remaining with
        | "EQ" | "NEQ" | "LT" | "LE" | "GE" | "GT" ->
            ok @@ Some (Seq (loc, [ remaining_prim ;
                                    Prim (loc, "IF", fail_false loc, None) ], None))
        | _ ->
            begin
              expand_compare remaining_prim >|? function
              | None -> None
              | Some seq ->
                  Some (Seq (loc, [ seq ;
                                    Prim (loc, "IF", fail_false loc, None) ], None))
            end
      end
  | _ -> ok None


let expand_if_some = function
  | Prim (loc, "IF_SOME", [ right ; left ], None) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_NONE", [ left ; right ], None) ], None))
  | Prim (_, "IF_SOME", args, None) ->
      error (Invalid_arity ("IF_SOME", List.length args, 2))
  | Prim (_, "IF_SOME", [], Some _) ->
      error (Unexpected_macro_annotation "IF_SOME")
  | _ -> ok @@ None

let expand_if_right = function
  | Prim (loc, "IF_RIGHT", [ right ; left ], None) ->
      ok @@ Some (Seq (loc, [ Prim (loc, "IF_LEFT", [ left ; right ], None) ], None))
  | Prim (_, "IF_RIGHT", args, None) ->
      error (Invalid_arity ("IF_RIGHT", List.length args, 2))
  | Prim (_, "IF_RIGHT", [], Some _) ->
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

let unexpand_caddadr expanded =
  let rec rsteps acc = function
    | [] -> Some acc
    | Prim (_, "CAR" , [], None) :: rest ->
        rsteps ("A" :: acc) rest
    | Prim (_, "CDR" , [], None) :: rest ->
        rsteps ("D" :: acc) rest
    | _ -> None in
  match expanded with
  | Seq (loc, (Prim (_, "CAR" , [], None) :: _ as nodes), None)
  | Seq (loc, (Prim (_, "CDR" , [], None) :: _ as nodes), None) ->
      begin match rsteps [] nodes with
        | Some steps ->
            let name = String.concat "" ("C" :: List.rev ("R" :: steps)) in
            Some (Prim (loc, name, [], None))
        | None -> None
      end
  | _ -> None

let unexpand_set_caddadr expanded =
  let rec steps acc = function
    | Seq (loc,
           [ Prim (_, "CDR", [], None) ;
             Prim (_, "SWAP", [], None) ;
             Prim (_, "PAIR", [], None) ], None) ->
        Some (loc, "A" :: acc)
    | Seq (loc,
           [ Prim (_, "CAR", [], None) ;
             Prim (_, "PAIR", [], None) ], None) ->
        Some (loc, "D" :: acc)
    | Seq (_,
           [ Prim (_, "DUP", [], None) ;
             Prim (_, "DIP",
                   [ Seq (_,
                          [ Prim (_, "CAR", [], None) ;
                            sub ], None) ], None) ;
             Prim (_, "CDR", [], None) ;
             Prim (_, "SWAP", [], None) ;
             Prim (_, "PAIR", [], None) ], None) ->
        steps ("A" :: acc) sub
    | Seq (_,
           [ Prim (_, "DUP", [], None) ;
             Prim (_, "DIP",
                   [ Seq (_,
                          [ Prim (_, "CDR", [], None) ;
                            sub ], None) ], None) ;
             Prim (_, "CAR", [], None) ;
             Prim (_, "PAIR", [], None) ], None) ->
        steps ("D" :: acc) sub
    | _ -> None in
  match steps [] expanded with
  | Some (loc, steps) ->
      let name = String.concat "" ("SET_C" :: List.rev ("R" :: steps)) in
      Some (Prim (loc, name, [], None))
  | None -> None

let unexpand_map_caddadr expanded =
  let rec steps acc = function
    | Seq (loc,
           [ Prim (_, "DUP", [], None) ;
             Prim (_, "CDR", [], None) ;
             Prim (_, "SWAP", [], None) ;
             Prim (_, "CAR", [], None) ;
             code ;
             Prim (_, "PAIR", [], None) ], None) ->
        Some (loc, "A" :: acc, code)
    | Seq (loc,
           [ Prim (_, "DUP", [], None) ;
             Prim (_, "CDR", [], None) ;
             code ;
             Prim (_, "SWAP", [], None) ;
             Prim (_, "CAR", [], None) ;
             Prim (_, "PAIR", [], None) ], None) ->
        Some (loc, "D" :: acc, code)
    | Seq (_,
           [ Prim (_, "DUP", [], None) ;
             Prim (_, "DIP",
                   [ Seq (_,
                          [ Prim (_, "CAR", [], None) ;
                            sub ], None) ], None) ;
             Prim (_, "CDR", [], None) ;
             Prim (_, "SWAP", [], None) ;
             Prim (_, "PAIR", [], None) ], None) ->
        steps ("A" :: acc) sub
    | Seq (_,
           [ Prim (_, "DUP", [], None) ;
             Prim (_, "DIP",
                   [ Seq (_,
                          [ Prim (_, "CDR", [], None) ;
                            sub ], None) ], None) ;
             Prim (_, "CAR", [], None) ;
             Prim (_, "PAIR", [], None) ], None) ->
        steps ("D" :: acc) sub
    | _ -> None in
  match steps [] expanded with
  | Some (loc, steps, code) ->
      let name = String.concat "" ("MAP_C" :: List.rev ("R" :: steps)) in
      Some (Prim (loc, name, [ code ], None))
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
                 [ Seq (_, [ Prim (_, "DIP", [ _ ], None) ], None) as sub ],
                 None) ],
         None) ->
      let rec count acc = function
        | Seq (_, [ Prim (_, "DIP", [ sub ], None) ], None) -> count (acc + 1) sub
        | sub -> (acc, sub) in
      let depth, sub = count 1 sub in
      let name = "D" ^ roman_of_decimal depth ^ "P" in
      Some (Prim (loc, name, [ sub ], None))
  | _ -> None

let unexpand_duuuuup expanded =
  let rec help expanded =
    match expanded with
    | Seq (loc, [ Prim (_, "DUP", [], None) ], None) -> Some (loc, 1)
    | Seq (_, [ Prim (_, "DIP", [expanded'], None);
                Prim (_, "SWAP", [], None) ], None) ->
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
  | Some (loc, n) -> Some (Prim (loc, "D" ^ (dupn n), [], None))

let unexpand_paaiair expanded =
  match expanded with
  | Seq (_,  [ Prim (_, "PAIR", [], None) ], None) -> Some expanded
  | Seq (loc, (_ :: _ as nodes), None) ->
      let rec destruct acc = function
        | [] -> Some acc
        | Prim (_, "DIP", [ Seq (_, [ sub ], None) ], None) :: rest ->
            destruct ("A" :: acc) (sub :: rest)
        | Prim (_, "PAIR", [], None) :: rest ->
            destruct ("AI" :: acc) rest
        | _ -> None in
      begin match destruct [] nodes with
        | None -> None
        | Some seq ->
            let name = String.concat "" ("P" :: List.rev ("R" :: seq)) in
            Some (Prim (loc, name, [], None))
      end
  | _ -> None

let unexpand_unpaaiair expanded =
  match expanded with
  | Seq (loc, (_ :: _ as nodes), None) ->
      let rec destruct sacc acc = function
        | [] -> Some acc
        | Prim (_, "DIP", [ Seq (_, [ sub ], None) ], None) :: rest
        | Prim (_, "DIP", [ Seq (_, _, _) as sub ], None) :: rest ->
            destruct ("A" :: sacc) acc (sub :: rest)
        | Seq (_, [ Prim (_, "DUP", [], None) ;
                    Prim (_, "CAR", [], None) ;
                    Prim (_, "DIP",
                          [ Seq (_,
                                 [ Prim (_, "CDR", [], None) ], None) ],
                          None) ], None) :: rest ->
            destruct [] (List.rev ("AI" :: sacc) :: acc) rest
        | _ -> None in
      begin match destruct [] [ [ "R" ] ] nodes with
        | None -> None
        | Some seq ->
            let name = String.concat "" ("UNP" :: List.flatten seq) in
            Some (Prim (loc, name, [], None))
      end
  | _ -> None

let unexpand_compare expanded =
  match expanded with
  | Seq (loc, [ Prim (_, "COMPARE", [], None) ;
                Prim (_, "EQ", [], None) ], None) ->
      Some (Prim (loc, "CMPEQ", [], None))
  | Seq (loc, [ Prim (_, "COMPARE", [], None) ;
                Prim (_, "NEQ", [], None) ], None) ->
      Some (Prim (loc, "CMPNEQ", [], None))
  | Seq (loc, [ Prim (_, "COMPARE", [], None) ;
                Prim (_, "LT", [], None) ], None) ->
      Some (Prim (loc, "CMPLT", [], None))
  | Seq (loc, [ Prim (_, "COMPARE", [], None) ;
                Prim (_, "GT", [], None) ], None) ->
      Some (Prim (loc, "CMPGT", [], None))
  | Seq (loc, [ Prim (_, "COMPARE", [], None) ;
                Prim (_, "LE", [], None) ], None) ->
      Some (Prim (loc, "CMPLE", [], None))
  | Seq (loc, [ Prim (_, "COMPARE", [], None) ;
                Prim (_, "GE", [], None) ], None) ->
      Some (Prim (loc, "CMPGE", [], None))
  | Seq (loc, [ Prim (_, "COMPARE", [], None) ;
                Prim (_, "EQ", [], None) ;
                Prim (_, "IF", args, None) ], None) ->
      Some (Prim (loc, "IFCMPEQ", args, None))
  | Seq (loc, [ Prim (_, "COMPARE", [], None) ;
                Prim (_, "NEQ", [], None) ;
                Prim (_, "IF", args, None) ], None) ->
      Some (Prim (loc, "IFCMPNEQ", args, None))
  | Seq (loc, [ Prim (_, "COMPARE", [], None) ;
                Prim (_, "LT", [], None) ;
                Prim (_, "IF", args, None) ], None) ->
      Some (Prim (loc, "IFCMPLT", args, None))
  | Seq (loc, [ Prim (_, "COMPARE", [], None) ;
                Prim (_, "GT", [], None) ;
                Prim (_, "IF", args, None) ], None) ->
      Some (Prim (loc, "IFCMPGT", args, None))
  | Seq (loc, [ Prim (_, "COMPARE", [], None) ;
                Prim (_, "LE", [], None) ;
                Prim (_, "IF", args, None) ], None) ->
      Some (Prim (loc, "IFCMPLE", args, None))
  | Seq (loc, [ Prim (_, "COMPARE", [], None) ;
                Prim (_, "GE", [], None) ;
                Prim (_, "IF", args, None) ], None) ->
      Some (Prim (loc, "IFCMPGE", args, None))
  | Seq (loc, [ Prim (_, "EQ", [], None) ;
                Prim (_, "IF", args, None) ], None) ->
      Some (Prim (loc, "IFEQ", args, None))
  | Seq (loc, [ Prim (_, "NEQ", [], None) ;
                Prim (_, "IF", args, None) ], None) ->
      Some (Prim (loc, "IFNEQ", args, None))
  | Seq (loc, [ Prim (_, "LT", [], None) ;
                Prim (_, "IF", args, None) ], None) ->
      Some (Prim (loc, "IFLT", args, None))
  | Seq (loc, [ Prim (_, "GT", [], None) ;
                Prim (_, "IF", args, None) ], None) ->
      Some (Prim (loc, "IFGT", args, None))
  | Seq (loc, [ Prim (_, "LE", [], None) ;
                Prim (_, "IF", args, None) ], None) ->
      Some (Prim (loc, "IFLE", args, None))
  | Seq (loc, [ Prim (_, "GE", [], None) ;
                Prim (_, "IF", args, None) ], None) ->
      Some (Prim (loc, "IFGE", args, None))
  | _ -> None

let unexpand_asserts expanded =
  match expanded with
  | Seq (loc, [ Prim (_, "IF", [ Seq (_, [ ], None) ;
                                 Seq (_, [ Prim(_, "FAIL", [], None) ], None) ],
                      None) ], None) ->
      Some (Prim (loc, "ASSERT", [], None))
  | Seq (loc, [ Seq (_, [ Prim(_, "COMPARE", [], None) ; Prim(_, comparison, [], None) ], None) ;
                Prim (_, "IF", [ Seq (_, [ ], None) ;
                                 Seq (_, [ Prim(_, "FAIL", [], None) ], None) ],
                      None) ], None) ->
      Some (Prim (loc, "ASSERT_CMP" ^ comparison, [], None))
  | Seq (loc, [ Prim (_, comparison, [], None) ;
                Prim (_, "IF", [ Seq (_, [ ], None) ;
                                 Seq (_, [ Prim(_, "FAIL", [], None) ], None) ],
                      None) ], None) ->
      Some (Prim (loc, "ASSERT_" ^ comparison, [], None))
  | Seq (loc, [ Prim (_, "IF_NONE", [ Seq (_, [ ], None) ;
                                      Seq (_, [ Prim(_, "FAIL", [], None) ], None) ],
                      None) ], None) ->
      Some (Prim (loc, "ASSERT_NONE", [], None))
  | Seq (loc, [ Prim (_, "IF_NONE", [ Seq (_, [ Prim(_, "FAIL", [], None) ], None) ;
                                      Seq (_, [ ], None)],
                      None) ], None) ->
      Some (Prim (loc, "ASSERT_SOME", [], None))
  | Seq (loc, [ Prim (_, "IF_LEFT", [ Seq (_, [ ], None) ;
                                      Seq (_, [ Prim(_, "FAIL", [], None) ], None) ],
                      None) ], None) ->
      Some (Prim (loc, "ASSERT_LEFT", [], None))
  | Seq (loc, [ Prim (_, "IF_LEFT", [ Seq (_, [ Prim(_, "FAIL", [], None) ], None) ;
                                      Seq (_, [ ], None) ],
                      None) ], None) ->
      Some (Prim (loc, "ASSERT_RIGHT", [], None))
  | _ -> None


let unexpand_if_some = function
  | Seq (loc, [ Prim (_, "IF_NONE", [ left ; right ], None) ], None) ->
      Some (Prim (loc, "IF_SOME", [ right ; left ], None))
  | _ -> None

let unexpand_if_right = function
  | Seq (loc, [ Prim (_, "IF_LEFT", [ left ; right ], None) ], None) ->
      Some (Prim (loc, "IF_RIGHT", [ right ; left ], None))
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
