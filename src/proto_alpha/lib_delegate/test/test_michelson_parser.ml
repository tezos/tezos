(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
module Helpers = Proto_alpha_helpers
module Assert = Helpers.Assert

let known_ok_tez_litterals =
  [ 0L, "0" ;
    10L, "0.00001" ;
    100L, "0.0001" ;
    1_000L, "0.001" ;
    10_000L, "0.01" ;
    100_000L, "0.1" ;
    1_000_000L, "1" ;
    10_000_000L, "10" ;
    100_000_000L, "100" ;
    1_000_000_000L, "1000" ;
    10_000_000_000L, "10000" ;
    100_000_000_000L, "100000" ;
    1_000_000_000_000L, "1000000" ;
    1_000_000_000_001L, "1000000.000001" ;
    1_000_000_000_010L, "1000000.00001" ;
    1_000_000_000_100L, "1000000.0001" ;
    1_000_000_001_000L, "1000000.001" ;
    1_000_000_010_000L, "1000000.01" ;
    1_000_000_100_000L, "1000000.1" ;
    123_123_123_123_123_123L, "123123123123.123123" ;
    999_999_999_999_999_999L, "999999999999.999999" ]

let known_bad_tez_litterals =
  [ "10000." ;
    "100,." ;
    "100," ;
    "1,0000" ;
    "0.0000,1" ;
    "0.00,1" ;
    "0,1" ;
    "HAHA" ;
    "0.000,000,1" ;
    "0.0000000" ;
    "9,999,999,999,999.999,999"]

let test_known_tez_litterals () =
  List.iter
    (fun (v, s) ->
       let vv = Tez_repr.of_mutez v in
       let vs = Tez_repr.of_string s in
       let vs' = Tez_repr.of_string (String.concat "" (String.split_on_char ',' s)) in
       let vv = match vv with None -> Assert.fail_msg "could not unopt %Ld" v | Some vv -> vv in
       let vs = match vs with None -> Assert.fail_msg "could not unopt %s" s | Some vs -> vs in
       let vs' = match vs' with None -> Assert.fail_msg "could not unopt %s" s | Some vs' -> vs' in
       Assert.equal ~prn:Tez_repr.to_string vv vs ;
       Assert.equal ~prn:Tez_repr.to_string vv vs' ;
       Assert.equal ~prn:(fun s -> s) (Tez_repr.to_string vv) s)
    known_ok_tez_litterals ;
  List.iter
    (fun s ->
       let vs = Tez_repr.of_string s in
       Assert.is_none ~msg:("Unexpected successful parsing of " ^ s) vs)
    known_bad_tez_litterals ;
  return_unit

let test_random_tez_litterals () =
  for _ = 0 to 100_000 do
    let v = Random.int64 12L in
    let vv = Tez_repr.of_mutez v in
    let vv = match vv with None -> Assert.fail_msg "could not unopt %Ld" v | Some vv -> vv in
    let s = Tez_repr.to_string vv in
    let vs = Tez_repr.of_string s in
    let s' = String.concat "" (String.split_on_char ',' s) in
    let vs' = Tez_repr.of_string s' in
    Assert.is_some ~msg:("Could not parse " ^ s ^ " back") vs ;
    Assert.is_some ~msg:("Could not parse " ^ s ^ " back") vs' ;
    begin match vs with
      | None -> assert false
      | Some vs ->
          let rev = Tez_repr.to_int64 vs in
          Assert.equal ~prn:Int64.to_string ~msg:(Tez_repr.to_string vv) v rev
    end ;
    begin match vs' with
      | None -> assert false
      | Some vs' ->
          let rev = Tez_repr.to_int64 vs' in
          Assert.equal ~prn:Int64.to_string ~msg:(Tez_repr.to_string vv) v rev
    end
  done ;
  return_unit

open Tezos_micheline
open Micheline

let zero_loc = Micheline_parser.location_zero

let prn expr =
  expr |>
  Micheline_printer.printable (fun s -> s) |>
  Format.asprintf "%a" Micheline_printer.print_expr

let assert_expands original expanded =
  let { Michelson_v1_parser.expanded = expansion }, errors =
    let source = prn (Micheline.strip_locations original) in
    Michelson_v1_parser.expand_all ~source ~original in
  let expanded = Micheline.strip_locations expanded in
  let expansion = Michelson_v1_primitives.strings_of_prims expansion in
  match errors with
  | [] ->
      Assert.equal ~prn expansion expanded ;
      ok ()
  | errors -> Error errors

let left_branch = Seq(zero_loc, [ Prim(zero_loc, "SWAP", [], []) ])
let right_branch = Seq(zero_loc, [])

let test_expansion () =
  assert_expands (Prim (zero_loc, "CAAR", [], []))
    (Seq (zero_loc,
          [(Prim (zero_loc, "CAR", [], []));
           (Prim (zero_loc, "CAR", [], [])) ])) >>? fun () ->
  assert_expands (Prim (zero_loc, "CAAR", [], [ "annot" ]))
    (Seq (zero_loc,
          [(Prim (zero_loc, "CAR", [], []));
           (Prim (zero_loc, "CAR", [], [ "annot" ])) ])) >>? fun () ->
  let car = Prim (zero_loc, "CAR", [], [ "annot" ]) in
  assert_expands car car >>? fun () ->
  let arg = [ Seq (zero_loc, [ car ]) ] in
  assert_expands
    (Prim (zero_loc, "DIP", arg, [ "new_annot" ]))
    (Prim (zero_loc, "DIP", arg, [ "new_annot" ])) >>? fun () ->
  assert_expands
    (Prim (zero_loc, "DIIP", arg, []))
    (Seq (zero_loc,
          [ Prim (zero_loc, "DIP",
                  [ (Seq (zero_loc,
                          [ Prim (zero_loc, "DIP", arg, []) ])) ],
                  []) ])) >>? fun () ->
  assert_expands
    (Prim (zero_loc, "DIIIP", arg, []))
    (Seq (zero_loc,
          [ Prim (zero_loc, "DIP",
                  [ (Seq (zero_loc,
                          [ Prim (zero_loc,
                                  "DIP",
                                  [ (Seq (zero_loc,
                                          [ Prim (zero_loc, "DIP", arg, []) ])) ],
                                  []) ])) ],
                  []) ])) >>? fun () ->
  assert_expands
    (Prim (zero_loc, "DUUP", [], []))
    (Seq (zero_loc,
          [ Prim (zero_loc, "DIP", [ Seq (zero_loc, [ Prim (zero_loc, "DUP", [], []) ]) ], []) ;
            Prim (zero_loc, "SWAP", [], []) ])) >>? fun () ->
  assert_expands
    (Prim (zero_loc, "DUUUP", [], []))
    (Seq (zero_loc,
          [ Prim (zero_loc, "DIP",
                  [ Seq (zero_loc, [
                        Prim (zero_loc, "DIP", [
                            Seq (zero_loc, [ Prim (zero_loc, "DUP", [], []) ])],
                              []);
                        Prim (zero_loc, "SWAP", [], []) ]) ],
                  []) ;
            Prim (zero_loc, "SWAP", [], []) ])) >>? fun () ->
  let assert_compare_macro prim_name compare_name =
    assert_expands
      (Prim (zero_loc, prim_name, [], []))
      (Seq (zero_loc,
            [ Prim (zero_loc, "COMPARE", [], []) ;
              Prim (zero_loc, compare_name, [], []) ])) in
  let assert_compare_if_macro prim_name compare_name =
    assert_expands
      (Prim (zero_loc, prim_name,
             [ left_branch ; right_branch ],
             []))
      (Seq (zero_loc, [ Prim(zero_loc, "COMPARE", [], []);
                        Prim(zero_loc, compare_name, [], []);
                        Prim (zero_loc, "IF", [ left_branch ; right_branch ], []) ])) in
  assert_compare_macro "CMPEQ" "EQ" >>? fun () ->
  assert_compare_macro "CMPNEQ" "NEQ" >>? fun () ->
  assert_compare_macro "CMPLT" "LT" >>? fun () ->
  assert_compare_macro "CMPLE" "LE" >>? fun () ->
  assert_compare_macro "CMPGT" "GT" >>? fun () ->
  assert_compare_macro "CMPGE" "GE" >>? fun () ->
  assert_compare_if_macro "IFCMPEQ" "EQ" >>? fun () ->
  assert_compare_if_macro "IFCMPNEQ" "NEQ" >>? fun () ->
  assert_compare_if_macro "IFCMPLT" "LT" >>? fun () ->
  assert_compare_if_macro "IFCMPLE" "LE" >>? fun () ->
  assert_compare_if_macro "IFCMPGT" "GT" >>? fun () ->
  assert_compare_if_macro "IFCMPGE" "GE" >>? fun () ->
  assert_expands (Prim (zero_loc, "ASSERT_LEFT", [], []))
    (Seq (zero_loc, [ Prim (zero_loc, "IF_LEFT",
                            [ Seq (zero_loc, []) ;
                              Seq (zero_loc, [
                                  Seq (zero_loc, [
                                      Prim(zero_loc, "UNIT", [], []) ;
                                      Prim(zero_loc, "FAILWITH", [], [])
                                    ])
                                ]) ],
                            []) ])) >>? fun () ->
  assert_expands (Prim (zero_loc, "ASSERT_RIGHT", [], []))
    (Seq (zero_loc, [ Prim (zero_loc, "IF_LEFT",
                            [ Seq (zero_loc, [
                                  Seq (zero_loc, [
                                      Prim(zero_loc, "UNIT", [], []) ;
                                      Prim(zero_loc, "FAILWITH", [], [])
                                    ])
                                ]) ;
                              Seq (zero_loc, []) ],
                            []) ])) >>? fun () ->
  assert_expands (Prim (zero_loc, "IF_RIGHT", [ left_branch ; right_branch ], []))
    (Seq (zero_loc, [ Prim (zero_loc, "IF_LEFT", [ right_branch ; left_branch ], []) ])) >>? fun () ->
  assert_expands (Prim (zero_loc, "IF_SOME", [ left_branch ; right_branch ], []))
    (Seq (zero_loc, [ Prim (zero_loc, "IF_NONE", [ right_branch ; left_branch ], []) ])) >>? fun () ->
  assert_expands
    (Prim (zero_loc, "PAIR", [], []))
    (Prim (zero_loc, "PAIR", [], [])) >>? fun () ->
  assert_expands
    (Prim (zero_loc, "PAPPAIIR", [], []))
    (Seq (zero_loc, [Prim
                       (zero_loc,
                        "DIP",
                        [Seq
                           (zero_loc,
                            [Prim (zero_loc, "PAIR", [], [])])],
                        []);
                     Prim
                       (zero_loc,
                        "DIP",
                        [Seq
                           (zero_loc,
                            [Prim (zero_loc, "PAIR", [], [])])],
                        []);
                     Prim (zero_loc, "PAIR", [], [])]))

let assert_unexpansion_consistent original =
  let { Michelson_v1_parser.expanded }, errors =
    let source = prn (Micheline.strip_locations original) in
    Michelson_v1_parser.expand_all ~source ~original in
  match errors with
  | _ :: _ -> Error errors
  | [] ->
      let { Michelson_v1_parser.unexpanded } =
        Michelson_v1_printer.unparse_expression expanded in
      Assert.equal ~prn unexpanded (Micheline.strip_locations original) ;
      ok ()

let test_unexpansion_consistency () =
  assert_unexpansion_consistent (Prim (zero_loc, "PAPPAIIR", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "PPAIPAIR", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "UNPAPPAIIR", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "UNPAPAPAIR", [], [])) >>? fun () ->
  assert_unexpansion_consistent
    (Prim (zero_loc, "DIIIP", [ Seq (zero_loc, [ Prim (zero_loc, "DROP", [], []) ]) ], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "SET_CAR", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "SET_CDR", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "DUP", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "DUUP", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "DUUUP", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "DUUUUP", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "DUUUUUP", [], [])) >>? fun () ->

  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_EQ", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_NEQ", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_LT", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_LE", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_GT", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_GE", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_NONE", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_SOME", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_LEFT", [], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_RIGHT", [], [])) >>? fun () ->

  assert_unexpansion_consistent (Prim (zero_loc, "IF_RIGHT", [ left_branch ; right_branch], [])) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "IF_SOME", [ left_branch ; right_branch], []))

let test_lexing () =
  let open Micheline_parser in
  let assert_tokenize_result source expected =
    match tokenize source with
    | tokens, [] ->
        let tokens =
          List.map (fun x -> x.token) tokens in
        Assert.equal tokens expected ;
        ok ()
    | _, errors -> Error errors in
  assert_tokenize_result "int"
    [ (Ident "int") ] >>? fun () ->
  assert_tokenize_result "100"
    [ (Int "100") ] >>? fun () ->
  assert_tokenize_result "(option int)"
    [ Open_paren ; Ident "option" ; Ident "int" ; Close_paren ] >>? fun () ->
  assert_tokenize_result "DIP { ADD }"
    [ Ident "DIP" ; Open_brace ; Ident "ADD" ; Close_brace ] >>? fun () ->
  assert_tokenize_result "\"hello\""
    [ String "hello" ] >>? fun () ->
  assert_tokenize_result "parameter int;"
    [ Ident "parameter" ; Ident "int" ; Semi ] >>? fun () ->
  assert_tokenize_result "PUSH string \"abcd\";"
    [ Ident "PUSH" ; Ident "string" ; String "abcd" ; Semi ] >>? fun () ->
  assert_tokenize_result "DROP; SWAP"
    [ Ident "DROP" ; Semi ; Ident "SWAP" ] >>? fun () ->
  assert_tokenize_result "string"
    [ Ident "string" ]

let test_parsing () =
  let assert_parses source expected =
    let open Micheline_parser in
    match tokenize source with
    | _, (_ :: _ as errors) -> Error errors
    | tokens, [] ->
        match Micheline_parser.parse_toplevel tokens with
        | _, (_ :: _ as errors) -> Error errors
        | ast, [] ->
            let ast = List.map Micheline.strip_locations ast in
            let expected = List.map Micheline.strip_locations expected in
            Assert.equal (List.length ast) (List.length expected) ;
            List.iter2 (Assert.equal ~prn) ast expected ;
            ok () in

  assert_parses "PUSH int 100"
    [ (Prim ((), "PUSH", [ Prim ((), "int", [], []) ;
                           Int ((), Z.of_int 100) ], [])) ] >>? fun () ->

  assert_parses "DROP" [ (Prim ((), "DROP", [], [])) ] >>? fun () ->
  assert_parses "DIP{DROP}"
    [ Prim ((), "DIP", [ Seq((), [ Prim ((), "DROP", [], []) ]) ], []) ] >>? fun () ->

  assert_parses "LAMBDA int int {}"
    [ Prim ((), "LAMBDA", [ Prim ((), "int", [], []) ;
                            Prim ((), "int", [], []) ;
                            Seq ((), []) ], []) ] >>? fun () ->

  assert_parses "LAMBDA @name int int {}"
    [ Prim ((), "LAMBDA", [ Prim ((), "int", [], []) ;
                            Prim ((), "int", [], []) ;
                            Seq ((), []) ], [ "@name" ]) ] >>? fun () ->

  assert_parses "NIL @annot string; # comment\n"
    [ Prim ((), "NIL", [ Prim ((), "string", [], []) ], [ "@annot" ]) ] >>? fun () ->

  assert_parses "PUSH (pair bool string) (Pair False \"abc\")"
    [ Prim ((), "PUSH", [ Prim ((), "pair",
                                [ Prim ((), "bool", [], []) ;
                                  Prim ((), "string", [], []) ], []) ;
                          Prim ((), "Pair",
                                [ Prim ((), "False", [], []) ;
                                  String ((), "abc")], []) ], []) ] >>? fun () ->
  assert_parses "PUSH (list nat) (List 1 2 3)"
    [ Prim ((), "PUSH", [ Prim ((), "list",
                                [ Prim ((), "nat", [], []) ], []) ;
                          Prim ((), "List",
                                [ Int((), Z.of_int 1);
                                  Int ((), Z.of_int 2);
                                  Int ((), Z.of_int 3)],
                                []) ], []) ] >>? fun () ->
  assert_parses "PUSH (lambda nat nat) {}"
    [ Prim ((), "PUSH", [ Prim ((), "lambda",
                                [ Prim ((), "nat", [], []);
                                  Prim ((), "nat", [], [])], []) ;
                          Seq((), [])],
            []) ] >>? fun () ->
  assert_parses "PUSH key \"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx\""
    [ Prim ((), "PUSH", [ Prim ((), "key", [], []) ;
                          String ((),"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx") ],
            []) ] >>? fun () ->
  assert_parses "PUSH (map int bool) (Map (Item 100 False))"
    [ Prim ((), "PUSH", [ Prim ((), "map",
                                [ Prim((), "int", [], []);
                                  Prim((), "bool", [], [])], []) ;
                          Prim ((), "Map",
                                [Prim ((), "Item",
                                       [Int ((), Z.of_int 100);
                                        Prim ((), "False", [], [])], [])], []) ],
            []) ] >>? fun () ->
  assert_parses
    "parameter int; \
     return int; \
     storage unit; \
     code {}"
    [ Prim ((), "parameter", [ Prim((), "int", [], []) ], []);
      Prim ((), "return", [ Prim((), "int", [], []) ], []);
      Prim ((), "storage", [ Prim((), "unit", [], []) ], []);
      Prim ((), "code", [ Seq((), []) ], [])] >>? fun () ->
  assert_parses
    "parameter int; \
     storage unit; \
     return int; \
     code {CAR; PUSH int 1; ADD; UNIT; SWAP; PAIR};"
    [ Prim ((), "parameter", [ Prim((), "int", [], []) ], []);
      Prim ((), "storage", [ Prim((), "unit", [], []) ], []);
      Prim ((), "return", [ Prim((), "int", [], []) ], []);
      Prim ((), "code", [ Seq((), [ Prim ((), "CAR", [], []) ;
                                    Prim ((), "PUSH", [ Prim((), "int", [], []) ;
                                                        Int ((), Z.of_int 1)], []) ;
                                    Prim ((), "ADD", [], []) ;
                                    Prim ((), "UNIT", [], []) ;
                                    Prim ((), "SWAP", [], []) ;
                                    Prim ((), "PAIR", [], [])]) ], [])] >>? fun () ->
  assert_parses
    "code {DUP @test; DROP}"
    [ Prim ((), "code", [Seq ((), [ Prim ((), "DUP", [], [ "@test" ]);
                                    Prim ((), "DROP", [], [])])], []) ] >>? fun () ->
  assert_parses
    "IF {CAR} {CDR}"
    [ Prim ((), "IF", [ Seq ((), [ Prim ((), "CAR", [], []) ]);
                        Seq ((), [ Prim ((), "CDR", [], []) ]) ], []) ] >>? fun () ->
  assert_parses
    "IF_NONE {FAIL} {}"
    [ Prim ((), "IF_NONE", [ Seq ((), [ Prim ((), "FAIL", [], []) ]);
                             Seq ((), []) ], []) ]

let tests = [
  "lexing", (fun _ -> Lwt.return (test_lexing ())) ;
  "parsing", (fun _ -> Lwt.return (test_parsing ())) ;
  "expansion", (fun _ -> Lwt.return (test_expansion ())) ;
  "consistency", (fun _ -> Lwt.return (test_unexpansion_consistency ())) ;
  "tez-litterals", (fun _ -> test_known_tez_litterals ()) ;
  "rnd-tez-litterals", (fun _ -> test_random_tez_litterals ()) ;
]

let wrap (n, f) =
  Alcotest_lwt.test_case n `Quick begin fun _ () ->
    f () >>= function
    | Ok () -> Lwt.return_unit
    | Error error ->
        Format.kasprintf Pervasives.failwith "%a" pp_print_error error
  end

let () =
  Alcotest.run ~argv:[|""|] "tezos-client-alpha" [
    "michelson", List.map wrap tests
  ]
