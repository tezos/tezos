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
    10L, "0.000,01" ;
    100L, "0.000,1" ;
    1_000L, "0.001" ;
    10_000L, "0.01" ;
    100_000L, "0.1" ;
    1_000_000L, "1" ;
    10_000_000L, "10" ;
    100_000_000L, "100" ;
    1_000_000_000L, "1,000" ;
    10_000_000_000L, "10,000" ;
    100_000_000_000L, "100,000" ;
    1_000_000_000_000L, "1,000,000" ;
    1_000_000_000_001L, "1,000,000.000,001" ;
    1_000_000_000_010L, "1,000,000.000,01" ;
    1_000_000_000_100L, "1,000,000.000,1" ;
    1_000_000_001_000L, "1,000,000.001" ;
    1_000_000_010_000L, "1,000,000.01" ;
    1_000_000_100_000L, "1,000,000.1" ;
    123_123_123_123_123_123L, "123,123,123,123.123,123" ;
    999_999_999_999_999_999L, "999,999,999,999.999,999" ]

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
  return ()

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
  return ()

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

let left_branch = Seq(zero_loc, [ Prim(zero_loc, "SWAP", [], None) ], None)
let right_branch = Seq(zero_loc, [ ], None)

let test_expansion () =
  assert_expands (Prim (zero_loc, "CAAR", [], None))
    (Seq (zero_loc,
          [(Prim (zero_loc, "CAR", [], None));
           (Prim (zero_loc, "CAR", [], None)) ],
          None)) >>? fun () ->
  assert_expands (Prim (zero_loc, "CAAR", [], Some "annot"))
    (Seq (zero_loc,
          [(Prim (zero_loc, "CAR", [], None));
           (Prim (zero_loc, "CAR", [], Some "annot")) ],
          None)) >>? fun () ->
  let car = Prim (zero_loc, "CAR", [], Some "annot") in
  assert_expands car car >>? fun () ->
  let arg = [ Seq (zero_loc, [ car ], None) ] in
  assert_expands
    (Prim (zero_loc, "DIP", arg, Some "new_annot"))
    (Prim (zero_loc, "DIP", arg, Some "new_annot")) >>? fun () ->
  assert_expands
    (Prim (zero_loc, "DIIP", arg, None))
    (Seq (zero_loc,
          [ Prim (zero_loc, "DIP",
                  [ (Seq (zero_loc,
                          [ Prim (zero_loc, "DIP", arg, None) ],
                          None)) ],
                  None) ],
          None)) >>? fun () ->
  assert_expands
    (Prim (zero_loc, "DIIIP", arg, None))
    (Seq (zero_loc,
          [ Prim (zero_loc, "DIP",
                  [ (Seq (zero_loc,
                          [ Prim (zero_loc,
                                  "DIP",
                                  [ (Seq (zero_loc,
                                          [ Prim (zero_loc, "DIP", arg, None) ],
                                          None)) ],
                                  None) ],
                          None)) ],
                  None) ],
          None)) >>? fun () ->
  assert_expands
    (Prim (zero_loc, "DUUP", [], None))
    (Seq (zero_loc,
          [ Prim (zero_loc, "DIP", [ Seq (zero_loc, [ Prim (zero_loc, "DUP", [], None) ], None) ], None) ;
            Prim (zero_loc, "SWAP", [], None) ], None)) >>? fun () ->
  assert_expands
    (Prim (zero_loc, "DUUUP", [], None))
    (Seq (zero_loc,
          [ Prim (zero_loc, "DIP",
                  [ Seq (zero_loc, [
                        Prim (zero_loc, "DIP", [
                            Seq (zero_loc, [ Prim (zero_loc, "DUP", [], None) ], None)],
                              None);
                        Prim (zero_loc, "SWAP", [], None) ],
                         None) ],
                  None) ;
            Prim (zero_loc, "SWAP", [], None) ], None)) >>? fun () ->
  let assert_compare_macro prim_name compare_name =
    assert_expands
      (Prim (zero_loc, prim_name, [], None))
      (Seq (zero_loc,
            [ Prim (zero_loc, "COMPARE", [], None) ;
              Prim (zero_loc, compare_name, [], None) ], None)) in
  let assert_compare_if_macro prim_name compare_name =
    assert_expands
      (Prim (zero_loc, prim_name,
             [ left_branch ; right_branch ],
             None))
      (Seq (zero_loc, [ Prim(zero_loc, "COMPARE", [], None);
                        Prim(zero_loc, compare_name, [], None);
                        Prim (zero_loc, "IF", [ left_branch ; right_branch ], None) ], None)) in
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
  assert_expands (Prim (zero_loc, "ASSERT_LEFT", [], None))
    (Seq (zero_loc, [ Prim (zero_loc, "IF_LEFT",
                            [ Seq (zero_loc, [ ], None) ;
                              Seq (zero_loc, [ Prim(zero_loc, "FAIL", [], None) ], None) ],
                            None) ], None)) >>? fun () ->
  assert_expands (Prim (zero_loc, "ASSERT_RIGHT", [], None))
    (Seq (zero_loc, [ Prim (zero_loc, "IF_LEFT",
                            [ Seq (zero_loc, [ Prim(zero_loc, "FAIL", [], None) ], None) ;
                              Seq (zero_loc, [ ], None) ],
                            None) ], None)) >>? fun () ->
  assert_expands (Prim (zero_loc, "IF_RIGHT", [ left_branch ; right_branch ], None))
    (Seq (zero_loc, [ Prim (zero_loc, "IF_LEFT", [ right_branch ; left_branch ], None) ], None)) >>? fun () ->
  assert_expands (Prim (zero_loc, "IF_SOME", [ left_branch ; right_branch ], None))
    (Seq (zero_loc, [ Prim (zero_loc, "IF_NONE", [ right_branch ; left_branch ], None) ], None)) >>? fun () ->
  assert_expands
    (Prim (zero_loc, "PAIR", [], None))
    (Prim (zero_loc, "PAIR", [], None)) >>? fun () ->
  assert_expands
    (Prim (zero_loc, "PAAIR", [], None))
    (Seq (zero_loc,
          [Prim
             (zero_loc,
              "DIP",
              [Seq (zero_loc, [Prim
                                 (zero_loc, "PAIR", [], None)],
                    None)],
              None)],
          None)) >>? fun () ->
  assert_expands
    (Prim (zero_loc, "PAAIAIR", [], None))
    (Seq (zero_loc, [Prim
                       (zero_loc,
                        "DIP",
                        [Seq
                           (zero_loc,
                            [Prim
                               (zero_loc,
                                "PAIR", [], None)],
                            None)],
                        None);
                     Prim
                       (zero_loc,
                        "PAIR", [], None)],
          None))

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
  assert_unexpansion_consistent (Prim (zero_loc, "PAAAIAIR", [], None)) >>? fun () ->
  assert_unexpansion_consistent
    (Prim (zero_loc, "DIIIP", [ Seq (zero_loc, [ Prim (zero_loc, "DROP", [], None) ], None) ], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "SET_CAR", [], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "SET_CDR", [], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "DUP", [], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "DUUP", [], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "DUUUP", [], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "DUUUUP", [], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "DUUUUUP", [], None)) >>? fun () ->

  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_EQ", [], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_NEQ", [], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_LT", [], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_LE", [], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_GT", [], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_GE", [], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_NONE", [], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_SOME", [], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_LEFT", [], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "ASSERT_RIGHT", [], None)) >>? fun () ->

  assert_unexpansion_consistent (Prim (zero_loc, "IF_RIGHT", [ left_branch ; right_branch], None)) >>? fun () ->
  assert_unexpansion_consistent (Prim (zero_loc, "IF_SOME", [ left_branch ; right_branch], None))

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
    [ (Prim ((), "PUSH", [ Prim ((), "int", [], None) ;
                           Int ((), "100") ], None)) ] >>? fun () ->

  assert_parses "DROP" [ (Prim ((), "DROP", [], None)) ] >>? fun () ->
  assert_parses "DIP{DROP}"
    [ Prim ((), "DIP", [ Seq((), [ Prim ((), "DROP", [], None) ], None) ], None) ] >>? fun () ->

  assert_parses "LAMBDA int int {}"
    [ Prim ((), "LAMBDA", [ Prim ((), "int", [], None) ;
                            Prim ((), "int", [], None) ;
                            Seq ((), [ ], None) ], None) ] >>? fun () ->

  assert_parses "LAMBDA @name int int {}"
    [ Prim ((), "LAMBDA", [ Prim ((), "int", [], None) ;
                            Prim ((), "int", [], None) ;
                            Seq ((), [ ], None) ], Some "@name") ] >>? fun () ->

  assert_parses "NIL @annot string; # comment\n"
    [ Prim ((), "NIL", [ Prim ((), "string", [], None) ], Some "@annot") ] >>? fun () ->

  assert_parses "PUSH (pair bool string) (Pair False \"abc\")"
    [ Prim ((), "PUSH", [ Prim ((), "pair",
                                [ Prim ((), "bool", [], None) ;
                                  Prim ((), "string", [], None) ], None) ;
                          Prim ((), "Pair",
                                [ Prim ((), "False", [], None) ;
                                  String ((), "abc")], None) ], None) ] >>? fun () ->
  assert_parses "PUSH (list nat) (List 1 2 3)"
    [ Prim ((), "PUSH", [ Prim ((), "list",
                                [ Prim ((), "nat", [], None) ], None) ;
                          Prim ((), "List",
                                [ Int((), "1");
                                  Int ((), "2");
                                  Int ((), "3")],
                                None) ], None) ] >>? fun () ->
  assert_parses "PUSH (lambda nat nat) {}"
    [ Prim ((), "PUSH", [ Prim ((), "lambda",
                                [ Prim ((), "nat", [], None);
                                  Prim ((), "nat", [], None)], None) ;
                          Seq((), [], None)],
            None) ] >>? fun () ->
  assert_parses "PUSH key \"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx\""
    [ Prim ((), "PUSH", [ Prim ((), "key", [], None) ;
                          String ((),"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx") ],
            None) ] >>? fun () ->
  assert_parses "PUSH (map int bool) (Map (Item 100 False))"
    [ Prim ((), "PUSH", [ Prim ((), "map",
                                [ Prim((), "int", [], None);
                                  Prim((), "bool", [], None)], None) ;
                          Prim ((), "Map",
                                [Prim ((), "Item",
                                       [Int ((), "100");
                                        Prim ((), "False", [], None)], None)], None) ],
            None) ] >>? fun () ->
  assert_parses
    "parameter int; \
     return int; \
     storage unit; \
     code {}"
    [ Prim ((), "parameter", [ Prim((), "int", [], None) ], None);
      Prim ((), "return", [ Prim((), "int", [], None) ], None);
      Prim ((), "storage", [ Prim((), "unit", [], None) ], None);
      Prim ((), "code", [ Seq((), [], None) ], None)] >>? fun () ->
  assert_parses
    "parameter int; \
     storage unit; \
     return int; \
     code {CAR; PUSH int 1; ADD; UNIT; SWAP; PAIR};"
    [ Prim ((), "parameter", [ Prim((), "int", [], None) ], None);
      Prim ((), "storage", [ Prim((), "unit", [], None) ], None);
      Prim ((), "return", [ Prim((), "int", [], None) ], None);
      Prim ((), "code", [ Seq((), [ Prim ((), "CAR", [], None) ;
                                    Prim ((), "PUSH", [ Prim((), "int", [], None) ;
                                                        Int ((), "1")], None) ;
                                    Prim ((), "ADD", [], None) ;
                                    Prim ((), "UNIT", [], None) ;
                                    Prim ((), "SWAP", [], None) ;
                                    Prim ((), "PAIR", [], None)], None) ], None)] >>? fun () ->
  assert_parses
    "code {DUP @test; DROP}"
    [ Prim ((), "code", [Seq ((), [ Prim ((), "DUP", [], Some "@test");
                                    Prim ((), "DROP", [], None)], None)], None) ] >>? fun () ->
  assert_parses
    "IF {CAR} {CDR}"
    [ Prim ((), "IF", [ Seq ((), [ Prim ((), "CAR", [], None) ], None);
                        Seq ((), [ Prim ((), "CDR", [], None) ], None) ], None) ] >>? fun () ->
  assert_parses
    "IF_NONE {FAIL} {}"
    [ Prim ((), "IF_NONE", [ Seq ((), [ Prim ((), "FAIL", [], None) ], None);
                             Seq ((), [ ], None) ], None) ]

let tests = [
  "lexing", (fun _ -> Lwt.return (test_lexing ())) ;
  "parsing", (fun _ -> Lwt.return (test_parsing ())) ;
  "expansion", (fun _ -> Lwt.return (test_expansion ())) ;
  "consistency", (fun _ -> Lwt.return (test_unexpansion_consistency ())) ;
  "tez-litterals", (fun _ -> test_known_tez_litterals ()) ;
  "rnd-tez-litterals", (fun _ -> test_random_tez_litterals ()) ;
]

let () =
  let module Test = Tezos_test_helpers.Test.Make(Error_monad) in
  Test.run "michelson." tests
