(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Script_located_ir

let expand_caddadr original =
  match original with
  | Prim (loc, str, [], annot) ->
      let len = String.length str in
      if len > 3
      && String.get str 0 = 'C'
      && String.get str (len - 1) = 'R' then
        let rec parse i ?annot acc =
          if i = 0 then
            Some (Seq (loc, acc, None))
          else
            match String.get str i with
            | 'A' -> parse (i - 1) (Prim (loc, "CAR", [], annot) :: acc)
            | 'D' -> parse (i - 1) (Prim (loc, "CDR", [], annot) :: acc)
            | _ -> None in
        parse (len - 2) ?annot []
      else
        None
  | _ -> None

let expand_set_caddadr original =
  match original with
  | Prim (loc, str, [], None) ->
      let len = String.length str in
      if len >= 7
      && String.sub str 0 5 = "SET_C"
      && String.get str (len - 1) = 'R' then
        let rec parse i acc =
          if i = 4 then
            Some acc
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
            | _ -> None in
        match String.get str (len - 2) with
        | 'A' ->
            let init =
              Seq (loc,
                   [ Prim (loc, "CDR", [], None) ;
                     Prim (loc, "SWAP", [], None) ;
                     Prim (loc, "PAIR", [], None) ], None) in
            parse (len - 3) init
        | 'D' ->
            let init =
              Seq (loc,
                   [ Prim (loc, "CAR", [], None) ;
                     Prim (loc, "PAIR", [], None) ], None) in
            parse (len - 3) init
        | _ -> None
      else
        None
  | _ -> None

let expand_map_caddadr original =
  match original with
  | Prim (loc, str, [ Seq _ as code ], None) ->
      let len = String.length str in
      if len >= 7
      && String.sub str 0 5 = "MAP_C"
      && String.get str (len - 1) = 'R' then
        let rec parse i acc =
          if i = 4 then
            Some acc
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
            | _ -> None in
        match String.get str (len - 2) with
        | 'A' ->
            let init =
              Seq (loc,
                   [ Prim (loc, "DUP", [], None) ;
                     Prim (loc, "CDR", [], None) ;
                     Prim (loc, "SWAP", [], None) ;
                     Prim (loc, "CAR", [], None) ;
                     code ;
                     Prim (loc, "PAIR", [], None) ], None) in
            parse (len - 3) init
        | 'D' ->
            let init =
              Seq (loc,
                   [ Prim (loc, "DUP", [], None) ;
                     Prim (loc, "CDR", [], None) ;
                     code ;
                     Prim (loc, "SWAP", [], None) ;
                     Prim (loc, "CAR", [], None) ;
                     Prim (loc, "PAIR", [], None) ], None) in
            parse (len - 3) init
        | _ -> None
      else
        None
  | _ -> None

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
      | _ -> raise Not_a_roman
    in
    if Compare.Int.(n < !lastval)
    then arabic := !arabic - n
    else arabic := !arabic + n;
    lastval := n
  done;
  !arabic

let expand_dxiiivp original =
  match original with
  | Prim (loc, str, [ arg ], None) ->
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
                (Seq (loc, [ Prim (loc, "DIP", [ acc ], None) ], None)) in
          Some (make depth arg)
        with Not_a_roman -> None
      else None
  | _ -> None

exception Not_a_pair

let expand_paaiair original =
  match original with
  | Prim (loc, str, [], None) ->
      let len = String.length str in
      if len > 4
      && String.get str 0 = 'P'
      && String.get str (len - 1) = 'R' then
        try
          let rec parse i acc =
            if i = 0 then
              acc
            else if String.get str i = 'I'
                 && String.get str (i - 1) = 'A' then
              parse (i - 2) (Prim (loc, "PAIR", [], None) :: acc)
            else if String.get str i = 'A' then
              match acc with
              | [] ->
                  raise Not_a_pair
              | acc :: accs ->
                  parse (i - 1)
                    (Prim (loc, "DIP", [ Seq (loc, [ acc ], None) ], None)
                     :: accs)
            else
              raise Not_a_pair in
          Some (Seq (loc, parse (len - 2) [], None))
        with Not_a_pair -> None
      else
        None
  | _ -> None

let expand_unpaaiair original =
  match original with
  | Prim (loc, str, [], None) ->
      let len = String.length str in
      if len >= 6
      && String.sub str 0 3 = "UNP"
      && String.get str (len - 1) = 'R' then
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
                  raise Not_a_pair
              | (Seq _ as acc) :: accs ->
                  parse (i - 1)
                    (Prim (loc, "DIP", [ acc ], None) :: accs)
              | acc :: accs ->
                  parse (i - 1)
                    (Prim (loc, "DIP",
                           [ Seq (loc, [ acc ], None) ],
                           None) :: accs)
            else
              raise Not_a_pair in
          Some (parse (len - 2) [])
        with Not_a_pair -> None
      else
        None
  | _ -> None

exception Not_a_dup

let expand_duuuuup original =
  match original with
  | Prim (loc, str, [], None) ->
      let len = String.length str in
      if len > 3
      && String.get str 0 = 'D'
      && String.get str 1 = 'U'
      && String.get str (len - 1) = 'P' then
        try
          let rec parse i acc =
            if i = 1 then acc
            else if String.get str i = 'U' then
              parse (i - 1)
                (Seq (loc, [ Prim (loc, "DIP", [ acc ], None) ;
                             Prim (loc, "SWAP", [], None) ], None))
            else
              raise Not_a_dup in
          Some (parse (len - 2) (Seq (loc, [ Prim (loc, "DUP", [], None) ], None)))
        with Not_a_dup -> None
      else
        None
  | _ -> None

let expand_compare original =
  match original with
  | Prim (loc, "CMPEQ", [], None) ->
      Some (Seq (loc, [ Prim (loc, "COMPARE", [], None) ;
                        Prim (loc, "EQ", [], None) ], None))
  | Prim (loc, "CMPNEQ", [], None) ->
      Some (Seq (loc, [ Prim (loc, "COMPARE", [], None) ;
                        Prim (loc, "NEQ", [], None) ], None))
  | Prim (loc, "CMPLT", [], None) ->
      Some (Seq (loc, [ Prim (loc, "COMPARE", [], None) ;
                        Prim (loc, "LT", [], None) ], None))
  | Prim (loc, "CMPGT", [], None) ->
      Some (Seq (loc, [ Prim (loc, "COMPARE", [], None) ;
                        Prim (loc, "GT", [], None) ], None))
  | Prim (loc, "CMPLE", [], None) ->
      Some (Seq (loc, [ Prim (loc, "COMPARE", [], None) ;
                        Prim (loc, "LE", [], None) ], None))
  | Prim (loc, "CMPGE", [], None) ->
      Some (Seq (loc, [ Prim (loc, "COMPARE", [], None) ;
                        Prim (loc, "GE", [], None) ], None))
  | Prim (loc, "IFCMPEQ", args, None) ->
      Some (Seq (loc, [ Prim (loc, "COMPARE", [], None) ;
                        Prim (loc, "EQ", [], None) ;
                        Prim (loc, "IF", args, None) ], None))
  | Prim (loc, "IFCMPNEQ", args, None) ->
      Some (Seq (loc, [ Prim (loc, "COMPARE", [], None) ;
                        Prim (loc, "NEQ", [], None) ;
                        Prim (loc, "IF", args, None) ], None))
  | Prim (loc, "IFCMPLT", args, None) ->
      Some (Seq (loc, [ Prim (loc, "COMPARE", [], None) ;
                        Prim (loc, "LT", [], None) ;
                        Prim (loc, "IF", args, None) ], None))
  | Prim (loc, "IFCMPGT", args, None) ->
      Some (Seq (loc, [ Prim (loc, "COMPARE", [], None) ;
                        Prim (loc, "GT", [], None) ;
                        Prim (loc, "IF", args, None) ], None))
  | Prim (loc, "IFCMPLE", args, None) ->
      Some (Seq (loc, [ Prim (loc, "COMPARE", [], None) ;
                        Prim (loc, "LE", [], None) ;
                        Prim (loc, "IF", args, None) ], None))
  | Prim (loc, "IFCMPGE", args, None) ->
      Some (Seq (loc, [ Prim (loc, "COMPARE", [], None) ;
                        Prim (loc, "GE", [], None) ;
                        Prim (loc, "IF", args, None) ], None))
  | Prim (loc, "IFEQ", args, None) ->
      Some (Seq (loc, [ Prim (loc, "EQ", [], None) ;
                        Prim (loc, "IF", args, None) ], None))
  | Prim (loc, "IFNEQ", args, None) ->
      Some (Seq (loc, [ Prim (loc, "NEQ", [], None) ;
                        Prim (loc, "IF", args, None) ], None))
  | Prim (loc, "IFLT", args, None) ->
      Some (Seq (loc, [ Prim (loc, "LT", [], None) ;
                        Prim (loc, "IF", args, None) ], None))
  | Prim (loc, "IFGT", args, None) ->
      Some (Seq (loc, [ Prim (loc, "GT", [], None) ;
                        Prim (loc, "IF", args, None) ], None))
  | Prim (loc, "IFLE", args, None) ->
      Some (Seq (loc, [ Prim (loc, "LE", [], None) ;
                        Prim (loc, "IF", args, None) ], None))
  | Prim (loc, "IFGE", args, None) ->
      Some (Seq (loc, [ Prim (loc, "GE", [], None) ;
                        Prim (loc, "IF", args, None) ], None))
  | _ -> None;;


let expand_asserts original =
  let fail_false loc =
    [ Seq(loc, [], None) ; Seq(loc, [ Prim (loc, "FAIL", [], None) ], None) ] in
  let fail_true loc =
    [ Seq(loc, [ Prim (loc, "FAIL", [], None) ], None) ; Seq(loc, [], None) ] in
  match original with
  | Prim (loc, "ASSERT", [], None) ->
      Some (Seq (loc, [ Prim (loc, "IF", fail_false loc, None) ], None))
  | Prim (loc, "ASSERT_NONE", [], None) ->
      Some (Seq (loc, [ Prim (loc, "IF_NONE", fail_false loc, None) ], None))
  | Prim (loc, "ASSERT_SOME", [], None) ->
      Some (Seq (loc, [ Prim (loc, "IF_NONE", fail_true loc, None) ], None))
  | Prim (loc, "ASSERT_LEFT", [], None) ->
      Some (Seq (loc, [ Prim (loc, "IF_LEFT", fail_false loc, None) ], None))
  | Prim (loc, "ASSERT_RIGHT", [], None) ->
      Some (Seq (loc, [ Prim (loc, "IF_LEFT", fail_true loc, None) ], None))
  | Prim (loc, s, [], None)
    when String.(length s >  7 && equal (sub s 0 7) "ASSERT_") ->
      begin
        let remaining = String.(sub s 7 ((length s) - 7)) in
        let remaining_prim = Prim(loc, remaining, [], None) in
        match remaining with
        | "EQ" | "NEQ" | "LT" | "LE" | "GE" | "GT" ->
            Some (Seq (loc, [ remaining_prim ;
                              Prim (loc, "IF", fail_false loc, None) ], None))
        | _ ->
            begin
              match expand_compare remaining_prim with
            | None -> None
            | Some seq ->
                Some (Seq (loc, [ seq ;
                                  Prim (loc, "IF", fail_false loc, None) ], None))
          end
      end
  | _ -> None


let expand_if_some = function
  | Prim (loc, "IF_SOME", [ right ; left ], None) ->
      Some (Seq (loc, [ Prim (loc, "IF_NONE", [ left ; right ], None) ], None))
  | _ -> None

let expand_if_right = function
  | Prim (loc, "IF_RIGHT", [ right ; left ], None) ->
      Some (Seq (loc, [ Prim (loc, "IF_LEFT", [ left ; right ], None) ], None))
  | _ -> None

let expand original =
  let try_expansions expanders =
    match
      List.fold_left
        (fun acc f ->
           match acc with
           | None -> f original
           | Some rewritten -> Some rewritten)
        None expanders with
    | None -> original
    | Some rewritten -> rewritten in
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

open Script

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
  let try_expansions unexpanders =
    match
      List.fold_left
        (fun acc f ->
           match acc with
           | None -> f original
           | Some rewritten -> Some rewritten)
        None unexpanders with
    | None -> original
    | Some rewritten -> rewritten in
  try_expansions
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
