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

(****************************************************************************)
(* Token value   *)
(****************************************************************************)

open Assert.Compat

let assert_tokenize ~loc given expected =
  match Micheline_parser.tokenize given with
  | tokens, [] ->
      let tokens_got =
        List.map (fun x -> x.Micheline_parser.token) tokens
      in
      Assert.equal_tokens ~loc tokens_got expected
  | _, _ -> failwith "%s - Cannot tokenize %s" loc given

let assert_tokenize_error ~loc given expected =
  match Micheline_parser.tokenize given with
  | tokens, [] ->
      let tokens_got =
        List.map (fun x -> x.Micheline_parser.token) tokens
      in
      Assert.not_equal_tokens ~loc tokens_got expected
  | _, _ -> return_unit

let test_tokenize_basic () =
  (* String *)
  assert_tokenize ~loc:__LOC__ "\"abc\"" [ String "abc" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "\"abc\t\"" [ String "abc\t" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "\"abc\b\"" [ String "abc\b" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "\"abc\\n\"" [ String "abc\n" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "\"abc\\r\"" [ String "abc\r" ] >>=? fun () ->
  (*fail*)
  assert_tokenize_error ~loc:__LOC__ "\"abc\n\"" [ String "abc\n" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "\"abc\\\"" [ String "abc\\" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "\"abc\"" [ String "abc\n" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "\"abc\r\"" [ String "abc\r" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "abc\r" [ String "abc\r" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "\"abc\"\r" [ String "abc\r" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "\"abc" [ String "abc" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "abc\"" [ String "abc" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "\"\"\"" [ String "" ] >>=? fun () ->
  (* Bytes *)
  assert_tokenize ~loc:__LOC__ "0xabc" [ Bytes "0xabc" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "0x" [ Bytes "0x" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "0x1" [ Bytes "0x1" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "xabc" [ Bytes "xabc" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "1xabc" [ Bytes "1xabc" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "1c" [ Bytes "1c" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "0c" [ Bytes "0c" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "0xx" [ Bytes "0xx" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "0b" [ Bytes "0b" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "0xg" [ Bytes "0xg" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "0X" [ Bytes "0X" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "1x" [ Bytes "1x" ] >>=? fun () ->
  (* Int *)
  assert_tokenize ~loc:__LOC__ "10" [ Int "10" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "0" [ Int "0" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "00" [ Int "00" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "001" [ Int "001" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "-0" [ Int "0" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "-1" [ Int "-1" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "1" [ Int "1" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "-10" [ Int "-10" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ ".1000" [ Int ".1000" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "10_00" [ Int "10_00" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "1,000" [ Int "1,000" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "1000.000" [ Int "1000.000" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "-0" [ Int "-0" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "--0" [ Int "0" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "+0" [ Int "0" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "a" [ Int "a" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "0a" [ Int "0a" ] >>=? fun () ->
  (* Ident *)
  assert_tokenize ~loc:__LOC__ "string" [ Ident "string" ] >>=? fun () ->
  (* Annotation *)
  assert_tokenize ~loc:__LOC__ "@my_pair" [ Annot "@my_pair" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "@@my_pair" [ Annot "@@my_pair" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "$t" [ Annot "$t" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "&t" [ Annot "&t" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ ":t" [ Annot ":t" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ ":_" [ Annot ":_" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ ":0" [ Annot ":0" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ ":%" [ Annot ":%" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ ":%%" [ Annot ":%%" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ ":%@" [ Annot ":%@" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ ":%@_" [ Annot ":%@_" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ ":%@_0" [ Annot ":%@_0" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "%from" [ Annot "%from" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "%@from" [ Annot "%@from" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "%from_a" [ Annot "%from_a" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "%from.a" [ Annot "%from.a" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "%From.a" [ Annot "%From.a" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "%0From.a" [ Annot "%0From.a" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "?t" [ Annot "?t" ] >>=? fun () ->
  (*fail*)
  assert_tokenize_error ~loc:__LOC__ "??t" [ Annot "??t" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "&&t" [ Annot "&&t" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "$$t" [ Annot "$$t" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "_from" [ Annot "_from" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ ".from" [ Annot ".from" ] >>=? fun () ->
  (*NOTE: the cases below fail because ':' is used in the middle of the
    annotation. *)
  assert_tokenize_error ~loc:__LOC__ "%:from" [ Annot "%:from" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "%:@from" [ Annot "%:@from" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "::t" [ Annot "::t" ] >>=? fun () ->
  (* Comment *)
  assert_tokenize ~loc:__LOC__
    "/*\"/**/\"*/" [Comment "/*\"/**/\"*/"] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "/* /* /* */ */ */" [Comment "/* /* /* */ */ */"] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "/*parse 1" [Comment "/*parse 1"] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "parse 1*/" [Comment "parse 1*/"] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "/* */*/" [Comment "/* */*/"] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "/*/* */" [Comment "/*/* */"] >>=? fun () ->
  (* EOL *)
  assert_tokenize ~loc:__LOC__ "#Access" [ Eol_comment "#Access" ] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "##Access" [ Eol_comment "##Access" ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "?Access" [ Eol_comment "?Access" ] >>=? fun () ->
  (* SKIP *)
  assert_tokenize ~loc:__LOC__ ";" [ Semi] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "{" [ Open_brace] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "}" [ Close_brace] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "(" [ Open_paren] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ ")" [ Close_paren] >>=? fun () ->
  (*fail*)
  assert_tokenize_error ~loc:__LOC__ "{" [ Semi ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ ";" [ Open_brace ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "}" [ Open_brace ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ "(" [ Close_paren ] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__ ")" [ Open_paren ]

(*********************)
(* One line contracts *)

let test_one_line_contract () =
  assert_tokenize ~loc:__LOC__ "(option int)"
    [Open_paren; Ident "option"; Ident "int"; Close_paren] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "DIP {ADD}"
    [Ident "DIP"; Open_brace; Ident "ADD"; Close_brace] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "parameter int;"
    [Ident "parameter"; Ident "int"; Semi] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "PUSH string \"abc\";"
    [Ident "PUSH"; Ident "string"; String "abc"; Semi] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "DROP; SWAP"
    [Ident "DROP"; Semi; Ident "SWAP"] >>=? fun () ->
  (* NOTE: the cases below do not fail because we only do tokenization. *)
  assert_tokenize ~loc:__LOC__ "DIP {ADD"
    [Ident "DIP"; Open_brace; Ident "ADD"] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "(option int"
    [Open_paren; Ident "option"; Ident "int"] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "parameter int}"
    [Ident "parameter"; Ident "int"; Close_brace] >>=? fun () ->
  assert_tokenize ~loc:__LOC__ "}{}{}{"
    [Close_brace; Open_brace; Close_brace; Open_brace; Close_brace; Open_brace]

(*********************************)
(* Conditional contracts *)

let test_condition_contract () =
  assert_tokenize ~loc:__LOC__
    "parameter (or string (option int));\
     storage unit;\
     return string;\
     code {CAR;\
     IF_LEFT{}\
     {IF_NONE {FAIL}\
     {PUSH int 0; CMPGT; \
     IF {FAIL}{PUSH string \"\"}}};\
     UNIT; SWAP; PAIR}"
    [Ident "parameter"; Open_paren; Ident "or"; Ident "string"; Open_paren;
     Ident "option"; Ident "int"; Close_paren; Close_paren; Semi;
     Ident "storage"; Ident "unit"; Semi;
     Ident "return"; Ident "string"; Semi;
     Ident "code"; Open_brace; Ident "CAR"; Semi;
     Ident "IF_LEFT"; Open_brace; Close_brace;
     Open_brace; Ident "IF_NONE"; Open_brace; Ident "FAIL"; Close_brace;
     Open_brace; Ident "PUSH"; Ident "int"; Int "0"; Semi; Ident "CMPGT"; Semi;
     Ident "IF"; Open_brace; Ident "FAIL"; Close_brace;
     Open_brace; Ident "PUSH"; Ident "string"; String "";
     Close_brace; Close_brace; Close_brace; Semi;
     Ident "UNIT"; Semi; Ident "SWAP"; Semi; Ident "PAIR"; Close_brace
    ] >>=? fun () ->
  (* NOTE: the cases below do not fail because we only do tokenization. *)
  assert_tokenize ~loc:__LOC__
    "parameter (or string (option int);"
    [Ident "parameter"; Open_paren; Ident "or"; Ident "string"; Open_paren;
     Ident "option"; Ident "int"; Close_paren; Semi] >>=? fun () ->
  assert_tokenize ~loc:__LOC__
    "parameter (or)"
    [Ident "parameter"; Open_paren; Ident "or"; Close_paren] >>=? fun () ->
  assert_tokenize_error ~loc:__LOC__
    "parameter (or"
    [Ident "parameter"; Open_paren; Ident "or"; Close_paren]

(****************************************************************************)
(* Top-level parsing tests *)
(****************************************************************************)

let assert_toplevel_parsing ~loc source expected =
  match Micheline_parser.tokenize source with
  | _, (_::_)  -> failwith "%s - Cannot tokenize %s" loc source
  | tokens, [] ->
      match Micheline_parser.parse_toplevel tokens with
      | _, (_::_) -> failwith "%s - Cannot parse_toplevel %s" loc source
      | ast, [] ->
          let ast = List.map Micheline.strip_locations ast in
          let expected = List.map Micheline.strip_locations expected in
          Assert.equal ~loc (List.length ast) (List.length expected) >>=? fun () ->
          iter2_p (Assert.equal ~loc) ast expected >>=? fun () ->
          return_unit

let assert_toplevel_parsing_error ~loc source expected =
  match Micheline_parser.tokenize source with
  | _, (_::_)  -> return_unit
  | tokens, [] ->
      match Micheline_parser.parse_toplevel tokens with
      | _, (_::_) -> return_unit
      | ast, [] ->
          let ast = List.map Micheline.strip_locations ast in
          let expected = List.map Micheline.strip_locations expected in
          Assert.equal ~loc (List.length ast) (List.length expected) >>=? fun () ->
          iter2_p (Assert.not_equal ~loc) ast expected

let test_basic_parsing () =
  assert_toplevel_parsing ~loc:__LOC__ "parameter unit;"
    [Prim ((), "parameter",
           [Prim ((), "unit", [], [])],
           [])] >>=? fun () ->
  (* Sequence *)
  assert_toplevel_parsing ~loc:__LOC__ "code {}"
    [Prim ((), "code",
           [ Seq ((), [])], [])] >>=? fun () ->
  (* Int *)
  assert_toplevel_parsing ~loc:__LOC__ "PUSH int 100"
    [Prim ((), "PUSH",
           [Prim ((), "int", [], []);
            Int ((), Z.of_int 100)],
           [])] >>=? fun () ->
  (*NOTE: this case doesn't fail because we don't type check *)
  assert_toplevel_parsing ~loc:__LOC__ "PUSH string 100"
    [Prim ((), "PUSH",
           [Prim ((), "string", [], []);
            Int ((), Z.of_int 100)],
           [])] >>=? fun () ->
  assert_toplevel_parsing_error ~loc:__LOC__ "PUSH int 100_000"
    [Prim ((), "PUSH",
           [Prim ((), "string", [], []);
            Int ((), Z.of_int 100_000)],
           [])] >>=? fun () ->
  assert_toplevel_parsing_error ~loc:__LOC__ "PUSH int 100"
    [Prim ((), "PUSH",
           [Prim ((), "int", [], []);
            Int ((), Z.of_int 1000)],
           [])] >>=? fun () ->
  assert_toplevel_parsing_error ~loc:__LOC__ "PUSH int 100"
    [Prim ((), "PUSH",
           [Prim ((), "string", [], []);
            Int ((), Z.of_int 100)],
           [])] >>=? fun () ->
  assert_toplevel_parsing_error ~loc:__LOC__ "PUSH int \"100\""
    [Prim ((), "PUSH",
           [Prim ((), "string", [], []);
            Int ((), Z.of_int 100)],
           [])] >>=? fun () ->
  (* String *)
  assert_toplevel_parsing ~loc:__LOC__ "Pair False \"abc\""
    [Prim (
        (), "Pair",
        [Prim (
            (), "False", [], []);
         String ((), "abc")], []
      )] >>=? fun () ->
  assert_toplevel_parsing_error ~loc:__LOC__ "Pair False \"ab\""
    [Prim (
        (), "Pair",
        [Prim (
            (), "False", [], []);
         String ((), "abc")], []
      )] >>=? fun () ->
  assert_toplevel_parsing_error ~loc:__LOC__ "Pair False abc\""
    [Prim (
        (), "Pair",
        [Prim (
            (), "False", [], []);
         String ((), "abc")], []
      )] >>=? fun () ->
  (* annotations *)
  assert_toplevel_parsing ~loc:__LOC__ "NIL @annot string; #comment\n"
    [Prim ((), "NIL", [Prim ((), "string", [], [])], ["@annot"])] >>=? fun () ->
  assert_toplevel_parsing_error ~loc:__LOC__ "NIL @annot string; #comment\n"
    [Prim ((), "NIL", [Prim ((), "string", [], [])], [])] >>=? fun () ->
  assert_toplevel_parsing ~loc:__LOC__ "IF_NONE {FAIL} {}"
    [Prim ((), "IF_NONE", [ Seq ((), [ Prim ((), "FAIL", [], [])]);
                            Seq ((), [])], [])] >>=? fun () ->
  assert_toplevel_parsing ~loc:__LOC__ "PUSH (map int bool) (Map (Item 100 False))"
    [Prim ((), "PUSH", [Prim ((), "map", [Prim ((), "int", [], []);
                                          Prim ((), "bool", [], [])], []);
                        Prim ((), "Map", [Prim ((), "Item",
                                                [Int((), Z.of_int 100);
                                                 Prim ((), "False", [], [])
                                                ], []);
                                         ], [])
                       ]
          , [])] >>=? fun () ->
  assert_toplevel_parsing ~loc:__LOC__ "LAMDA @name int int {}"
    [Prim ((), "LAMDA", [Prim ((), "int", [], []);
                         Prim ((), "int", [], []);
                         Seq ((), [])], ["@name"])] >>=? fun () ->
  assert_toplevel_parsing ~loc:__LOC__ "code {DUP @test; DROP}"
    [Prim ((), "code", [Seq ((), [Prim ((), "DUP", [], ["@test"]);
                                  Prim ((), "DROP", [], [])])], [])]

let test_condition_contract_parsing () =
  assert_toplevel_parsing ~loc:__LOC__ "parameter unit;\
                                        return unit;\
                                        storage tez; #How much you have to send me \n\
                                        code {CDR; DUP;\
                                        AMOUNT; CMPLT;\
                                        IF {FAIL}}"
    [Prim ((), "parameter", [ Prim ((), "unit", [],[])], []);
     Prim ((), "return", [Prim ((), "unit", [], [])], []);
     Prim ((), "storage", [Prim ((), "tez", [], [])], []);
     Prim ((), "code", [Seq ((), [Prim ((), "CDR", [], []);
                                  Prim ((), "DUP", [], []);
                                  Prim ((), "AMOUNT", [], []);
                                  Prim ((), "CMPLT", [], []);
                                  Prim ((), "IF",
                                        [Seq ((),
                                              [Prim ((), "FAIL", [], [])])]
                                       , [])])],
           [])
    ]

let test_list_append_parsing () =
  assert_toplevel_parsing ~loc:__LOC__ "parameter (pair (list int)(list int));\
                                        return (list int);\
                                        storage unit;\
                                        code { CAR; DUP; DIP{CDR}; CAR;\
                                        NIL int; SWAP;\
                                        LAMDA (pair int (list int))\
                                        (list int)\
                                        {DUP; CAR; DIP {CDR}; CONS};\
                                        REDUCE;\
                                        LAMDA (pair int (list int))\
                                        (list int)\
                                        {DUP; CAR; DIP{CDR}; CONS};\
                                        UNIT; SWAP; PAIR}"
    [Prim ((), "parameter",
           [Prim ((), "pair",
                  [Prim ((), "list", [Prim ((), "int", [], [])], []);
                   Prim ((), "list", [Prim ((), "int", [], [])], [])], [])], []);
     Prim ((), "return", [Prim ((), "list", [Prim ((), "int", [], [])], [])], []);
     Prim ((), "storage", [Prim ((), "unit", [], [])], []);
     Prim ((), "code",
           [Seq ((),
                 [Prim ((), "CAR", [], []);
                  Prim ((), "DUP", [], []);
                  Prim ((), "DIP", [Seq ((), [Prim ((), "CDR", [], [])])], []);
                  Prim ((), "CAR", [], []);
                  Prim ((), "NIL", [Prim ((), "int", [], [])], []);
                  Prim ((), "SWAP", [], []);
                  Prim ((), "LAMDA",
                        [Prim ((), "pair",
                               [Prim ((), "int", [], []);
                                Prim ((), "list",
                                      [Prim ((), "int", [], [])], [])
                               ], []);
                         Prim ((), "list", [Prim ((), "int", [], [])], []);
                         Seq ((), [Prim ((), "DUP", [], []);
                                   Prim ((), "CAR", [], []);
                                   Prim ((), "DIP", [Seq ((), [Prim ((), "CDR", [], [])])], []);
                                   Prim ((), "CONS", [], [])])
                        ], []);
                  Prim ((), "REDUCE", [], []);
                  Prim ((), "LAMDA",
                        [Prim ((), "pair",
                               [Prim ((), "int", [], []);
                                Prim ((), "list",
                                      [Prim ((), "int", [], [])], [])
                               ], []);
                         Prim ((), "list", [Prim ((), "int", [], [])], []);
                         Seq ((), [Prim ((), "DUP", [], []);
                                   Prim ((), "CAR", [], []);
                                   Prim ((), "DIP", [Seq ((), [Prim ((), "CDR", [], [])])], []);
                                   Prim ((), "CONS", [], [])])
                        ], []);
                  Prim ((), "UNIT", [], []);
                  Prim ((), "SWAP", [], []);
                  Prim ((), "PAIR", [], [])
                 ])], [])]

(****************************************************************************)
(* Expression parsing tests *)
(****************************************************************************)

let assert_expression_parsing ~loc source expected =
  match Micheline_parser.tokenize source with
  | _, (_::_)  -> failwith "%s - Cannot tokenize %s" loc source
  | tokens, [] ->
      match Micheline_parser.parse_expression tokens with
      | _, (_::_) -> failwith "%s - Cannot parse_expression %s" loc source
      | ast, [] ->
          let ast = Micheline.strip_locations ast in
          let expected = Micheline.strip_locations expected in
          Assert.equal ~loc ast expected

let test_parses_expression () =
  (* String *)
  assert_expression_parsing ~loc:__LOC__ "Pair False \"abc\""
    (Prim ((), "Pair", [Prim ((), "False", [], []);
                        String ((), "abc")], [])) >>=? fun () ->
  (* Int *)
  assert_expression_parsing ~loc:__LOC__ "Item 100"
    (Prim ((), "Item", [Int ((), Z.of_int 100)], [])) >>=? fun () ->
  (* Sequence *)
  assert_expression_parsing ~loc:__LOC__ "{}"
    (Seq ((), []))

(****************************************************************************)

let tests = [
  "tokenize", (fun _ -> test_tokenize_basic ()) ;
  "test one line contract", (fun _ -> test_one_line_contract ()) ;
  "test_condition_contract", (fun _ -> test_condition_contract ()) ;
  "test_basic_parsing", (fun _ -> test_basic_parsing ()) ;
  "test_condition_contract_parsing", (fun _ -> test_condition_contract_parsing ()) ;
  "test_list_append_parsing", (fun _ -> test_list_append_parsing ()) ;
  "test_parses_expression", (fun _ -> test_parses_expression ()) ;
]

let wrap (n, f) =
  Alcotest_lwt.test_case n `Quick begin fun _ () ->
    f () >>= function
    | Ok () -> Lwt.return_unit
    | Error err -> Lwt.fail_with err
  end

let () =
  Alcotest.run ~argv:[|""|] "tezos-lib-micheline" [
    "micheline", List.map wrap tests
  ]
