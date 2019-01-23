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

type test_result =
  | Success
  | Fail

let assert_success f =
  f >>=? function
  | Success-> return ()
  | Fail -> failwith "Fail : Bad result"

let assert_fail f =
  f >>=? function
  | Success -> failwith "Unexpected Success"
  | _ -> return ()

let assert_tokenize_result source expected =
  match Micheline_parser.tokenize source with
  | tokens, [] ->
      let tokens =
        List.map (fun x -> x.Micheline_parser.token) tokens
      in
      Assert.equal ~loc:__LOC__ tokens expected;
      return Success
  | _, _ -> return Fail

let assert_not_tokenize_result source expected =
  match Micheline_parser.tokenize source with
  | tokens, [] ->
      let tokens =
        List.map (fun x -> x.Micheline_parser.token) tokens
      in
      Assert.not_equal ~loc:__LOC__ tokens expected;
      return Fail
  | _, _ -> return Success

let test_tokenize_basic () =
  (* String *)
  assert_success @@ assert_tokenize_result "\"abc\"" [ String "abc" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "\"abc\t\"" [ String "abc\t" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "\"abc\b\"" [ String "abc\b" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "\"abc\\n\"" [ String "abc\n" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "\"abc\\r\"" [ String "abc\r" ] >>=? fun () ->
  (*fail*)
  assert_fail @@ assert_tokenize_result "\"abc\n\"" [ String "abc\n" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "\"abc\\\"" [ String "abc\\" ] >>=? fun () ->
  assert_fail @@ assert_not_tokenize_result "\"abc\"" [ String "abc\n" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "\"abc\r\"" [ String "abc\r" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "abc\r" [ String "abc\r" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "\"abc\"\r" [ String "abc\r" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "\"abc" [ String "abc" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "abc\"" [ String "abc" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "\"\"\"" [ String "" ] >>=? fun () ->
  (* Bytes *)
  assert_success @@ assert_tokenize_result "0xabc" [ Bytes "0xabc" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "0x" [ Bytes "0x" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "0x1" [ Bytes "0x1" ] >>=? fun () ->
  (*FIXME why xabc is is not equal *)
  assert_fail @@ assert_not_tokenize_result "xabc" [ Bytes "xabc" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "1xabc" [ Bytes "1xabc" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "1c" [ Bytes "1c" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "0c" [ Bytes "0c" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "0xx" [ Bytes "0xx" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "0b" [ Bytes "0b" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "0xg" [ Bytes "0xg" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "0X" [ Bytes "0X" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "1x" [ Bytes "1x" ] >>=? fun () ->
  (* Int *)
  assert_success @@ assert_tokenize_result "10" [ Int "10" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "0" [ Int "0" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "00" [ Int "00" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "001" [ Int "001" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "-0" [ Int "0" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "-1" [ Int "-1" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "1" [ Int "1" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "-10" [ Int "-10" ] >>=? fun () ->
  (*FIXME it is not equal*)
  assert_fail @@ assert_tokenize_result ".1000" [ Int ".1000" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "10_00" [ Int "10_00" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "1,000" [ Int "1,000" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "1000.000" [ Int "1000.000" ] >>=? fun () ->
  assert_fail @@ assert_not_tokenize_result "-0" [ Int "-0" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "--0" [ Int "0" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "+0" [ Int "0" ] >>=? fun () ->
  assert_fail @@ assert_not_tokenize_result "a" [ Int "a" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "0a" [ Int "0a" ] >>=? fun () ->
  (* Ident *)
  assert_success @@ assert_tokenize_result "string" [ Ident "string" ] >>=? fun () ->
  (* Annotation *)
  assert_success @@ assert_tokenize_result "@my_pair" [ Annot "@my_pair" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "@@my_pair" [ Annot "@@my_pair" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "$t" [ Annot "$t" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "&t" [ Annot "&t" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result ":t" [ Annot ":t" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result ":_" [ Annot ":_" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result ":0" [ Annot ":0" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result ":%" [ Annot ":%" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result ":%%" [ Annot ":%%" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result ":%@" [ Annot ":%@" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result ":%@_" [ Annot ":%@_" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result ":%@_0" [ Annot ":%@_0" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "%from" [ Annot "%from" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "%@from" [ Annot "%@from" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "%from_a" [ Annot "%from_a" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "%from.a" [ Annot "%from.a" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "%From.a" [ Annot "%From.a" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "%0From.a" [ Annot "%0From.a" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "?t" [ Annot "?t" ] >>=? fun () ->
  (*fail*)
  assert_fail @@ assert_not_tokenize_result "??t" [ Annot "??t" ] >>=? fun () ->
  assert_fail @@ assert_not_tokenize_result "&&t" [ Annot "&&t" ] >>=? fun () ->
  assert_fail @@ assert_not_tokenize_result "$$t" [ Annot "$$t" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "_from" [ Annot "_from" ] >>=? fun () ->
  assert_fail @@ assert_tokenize_result ".from" [ Annot ".from" ] >>=? fun () ->
  (*FIXME: why these cases below are not equal? and fail and not the %@?*)
  assert_fail @@ assert_not_tokenize_result "%:from" [ Annot "%:from" ] >>=? fun () ->
  assert_fail @@ assert_not_tokenize_result "%:@from" [ Annot "%:@from" ] >>=? fun () ->
  assert_fail @@ assert_not_tokenize_result "::t" [ Annot "::t" ] >>=? fun () ->
  (* Comment *)
  assert_success @@ assert_tokenize_result "/*parse 1*/" [Comment "/*parse 1*/"] >>=? fun () ->
  assert_success @@ assert_tokenize_result "/*/**/*/" [Comment "/*/**/*/"] >>=? fun () ->
  assert_success @@ assert_tokenize_result
    "/*\"/**/\"*/" [Comment "/*\"/**/\"*/"] >>=? fun () ->
  assert_success @@ assert_tokenize_result "/* /* /* */ */ */"
    [Comment "/* /* /* */ */ */"] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "/*parse 1" [Comment "/*parse 1"] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "parse 1*/" [Comment "parse 1*/"] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "/* */*/" [Comment "/* */*/"] >>=? fun () ->
  assert_fail @@ assert_tokenize_result "/*/* */" [Comment "/*/* */"] >>=? fun () ->
  (* EOL *)
  assert_success @@ assert_tokenize_result "#Access" [ Eol_comment "#Access" ] >>=? fun () ->
  assert_success @@ assert_tokenize_result "##Access" [ Eol_comment "##Access" ] >>=? fun () ->
  assert_fail @@ assert_not_tokenize_result "?Access" [ Eol_comment "?Access" ] >>=? fun () ->
  (* SKIP *)
  assert_success @@ assert_tokenize_result ";" [Semi] >>=? fun () ->
  assert_success @@ assert_tokenize_result "{" [Open_brace] >>=? fun () ->
  assert_success @@ assert_tokenize_result "}" [Close_brace] >>=? fun () ->
  assert_success @@ assert_tokenize_result "(" [Open_paren] >>=? fun () ->
  assert_success @@ assert_tokenize_result ")" [Close_paren] >>=? fun () ->
  (*fail*)
  assert_fail @@ assert_not_tokenize_result "{" [Semi] >>=? fun () ->
  assert_fail @@ assert_not_tokenize_result ";" [Open_brace] >>=? fun () ->
  assert_fail @@ assert_not_tokenize_result "}" [Open_brace] >>=? fun () ->
  assert_fail @@ assert_not_tokenize_result "(" [Close_paren] >>=? fun () ->
  assert_fail @@ assert_not_tokenize_result ")" [Open_paren]

(*********************)
(* one line contract *)

let test_one_line_contract () =
  assert_success @@ assert_tokenize_result "(option int)"
    [Open_paren; Ident "option"; Ident "int"; Close_paren] >>=? fun () ->
  assert_success @@ assert_tokenize_result "DIP {ADD}"
    [Ident "DIP"; Open_brace; Ident "ADD"; Close_brace] >>=? fun () ->
  assert_success @@ assert_tokenize_result "parameter int;"
    [Ident "parameter"; Ident "int"; Semi] >>=? fun () ->
  assert_success @@ assert_tokenize_result "PUSH string \"abc\";"
    [Ident "PUSH"; Ident "string"; String "abc"; Semi] >>=? fun () ->
  assert_success @@ assert_tokenize_result "DROP; SWAP"
    [Ident "DROP"; Semi; Ident "SWAP"] >>=? fun () ->
  (*FIXME: these cases do not fail? *)
  assert_success @@ assert_tokenize_result "DIP {ADD"
    [Ident "DIP"; Open_brace; Ident "ADD"] >>=? fun () ->
  assert_success @@ assert_tokenize_result "(option int"
    [Open_paren; Ident "option"; Ident "int"] >>=? fun () ->
  assert_success @@ assert_tokenize_result "parameter int}"
    [Ident "parameter"; Ident "int"; Close_brace] >>=? fun () ->
  assert_success @@ assert_tokenize_result "(option int"
    [Open_paren; Ident "option"; Ident "int"]

(*********************************)
(* Example of condition contract *)

let test_condition_contract () =
  assert_success @@ assert_tokenize_result
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
  (*FIXME: this case should fail because it is missing the close_paren?*)
  assert_success @@ assert_tokenize_result
    "parameter (or string (option int);"
    [Ident "parameter"; Open_paren; Ident "or"; Ident "string"; Open_paren;
     Ident "option"; Ident "int"; Close_paren; Semi] >>=? fun () ->
  assert_success @@ assert_tokenize_result
    "parameter (or)"
    [Ident "parameter"; Open_paren; Ident "or"; Close_paren] >>=? fun () ->
  assert_fail @@ assert_not_tokenize_result
    "parameter (or"
    [Ident "parameter"; Open_paren; Ident "or"; Close_paren]

(****************************************************************************)
(* Test parse toplevel   *)
(****************************************************************************)

let assert_parses source expected =
  match Micheline_parser.tokenize source with
  | _, (_::_)  -> return Fail
  | tokens, [] ->
      match Micheline_parser.parse_toplevel tokens with
      | _, (_::_) -> return Fail
      | ast, [] ->
          let ast = List.map Micheline.strip_locations ast in
          let expected = List.map Micheline.strip_locations expected in
          Assert.equal ~loc:__LOC__ (List.length ast) (List.length expected) ;
          List.iter2 (Assert.equal ~loc:__LOC__) ast expected;
          return Success

let assert_not_parses source expected =
  match Micheline_parser.tokenize source with
  | _, (_::_)  -> return Success
  | tokens, [] ->
      match Micheline_parser.parse_toplevel tokens with
      | _, (_::_) -> return Success
      | ast, [] ->
          let ast = List.map Micheline.strip_locations ast in
          let expected = List.map Micheline.strip_locations expected in
          Assert.equal ~loc:__LOC__ (List.length ast) (List.length expected) ;
          List.iter2 (Assert.not_equal ~loc:__LOC__) ast expected;
          return Fail

let test_basic_parsing () =
  assert_success @@ assert_parses "parameter unit;"
    [Prim ((), "parameter",
           [Prim ((), "unit", [], [])],
           [])] >>=? fun () ->
  (* Sequence *)
  assert_success @@ assert_parses "code {}"
    [Prim ((), "code",
           [ Seq ((), [])], [])] >>=? fun () ->
  (* Int *)
  assert_success @@ assert_parses "PUSH int 100"
    [Prim ((), "PUSH",
           [Prim ((), "int", [], []);
            Int ((), Z.of_int 100)],
           [])] >>=? fun () ->
  (*FIXME: this case should fail *)
  assert_success @@ assert_parses "PUSH string 100"
    [Prim ((), "PUSH",
           [Prim ((), "string", [], []);
            Int ((), Z.of_int 100)],
           [])] >>=? fun () ->
  assert_success @@ assert_not_parses "PUSH int 100_000"
    [Prim ((), "PUSH",
           [Prim ((), "string", [], []);
            Int ((), Z.of_int 100_000)],
           [])] >>=? fun () ->
  assert_fail @@ assert_not_parses "PUSH int 100"
    [Prim ((), "PUSH",
           [Prim ((), "int", [], []);
            Int ((), Z.of_int 1000)],
           [])] >>=? fun () ->
  assert_fail @@ assert_not_parses "PUSH int 100"
    [Prim ((), "PUSH",
           [Prim ((), "string", [], []);
            Int ((), Z.of_int 100)],
           [])] >>=? fun () ->
  assert_fail @@ assert_not_parses "PUSH int \"100\""
    [Prim ((), "PUSH",
           [Prim ((), "string", [], []);
            Int ((), Z.of_int 100)],
           [])] >>=? fun () ->
  (* String *)
  assert_success @@ assert_parses "Pair False \"abc\""
    [Prim (
        (), "Pair",
        [Prim (
            (), "False", [], []);
         String ((), "abc")], []
      )] >>=? fun () ->
  assert_fail @@ assert_not_parses "Pair False \"ab\""
    [Prim (
        (), "Pair",
        [Prim (
            (), "False", [], []);
         String ((), "abc")], []
      )] >>=? fun () ->
  assert_fail @@ assert_parses "Pair False abc\""
    [Prim (
        (), "Pair",
        [Prim (
            (), "False", [], []);
         String ((), "abc")], []
      )] >>=? fun () ->
  (* annotations *)
  assert_success @@ assert_parses "NIL @annot string; #comment\n"
    [Prim ((), "NIL", [Prim ((), "string", [], [])], ["@annot"])] >>=? fun () ->
  assert_fail @@ assert_not_parses "NIL @annot string; #comment\n"
    [Prim ((), "NIL", [Prim ((), "string", [], [])], [])] >>=? fun () ->
  assert_success @@ assert_parses "IF_NONE {FAIL} {}"
    [Prim ((), "IF_NONE", [ Seq ((), [ Prim ((), "FAIL", [], [])]);
                            Seq ((), [])], [])] >>=? fun () ->
  assert_success @@ assert_parses "PUSH (map int bool) (Map (Item 100 False))"
    [Prim ((), "PUSH", [Prim ((), "map", [Prim ((), "int", [], []);
                                          Prim ((), "bool", [], [])], []);
                        Prim ((), "Map", [Prim ((), "Item",
                                                [Int((), Z.of_int 100);
                                                 Prim ((), "False", [], [])
                                                ], []);
                                         ], [])
                       ]
          , [])] >>=? fun () ->
  assert_success @@ assert_parses "LAMDA @name int int {}"
    [Prim ((), "LAMDA", [Prim ((), "int", [], []);
                         Prim ((), "int", [], []);
                         Seq ((), [])], ["@name"])] >>=? fun () ->
  assert_success @@ assert_parses "code {DUP @test; DROP}"
    [Prim ((), "code", [Seq ((), [Prim ((), "DUP", [], ["@test"]);
                                  Prim ((), "DROP", [], [])])], [])]

(*********************************)
(* Example of condition contract *)

let test_condition_contract_parsing () =
  assert_success @@ assert_parses "parameter unit;\
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

(* Example of append list *)

let test_list_append_parsing () =
  assert_success @@ assert_parses "parameter (pair (list int)(list int));\
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
(* Test parse expression  *)
(****************************************************************************)

let assert_parses_expression source expected =
  match Micheline_parser.tokenize source with
  | _, (_ :: _) -> return Fail
  | tokens, [] ->
      match Micheline_parser.parse_expression tokens with
      | _, (_ :: _) -> return Fail
      | ast, [] ->
          let ast = Micheline.strip_locations ast in
          let expected = Micheline.strip_locations expected in
          Assert.equal ~loc:__LOC__ ast expected;
          return Success

let test_parses_expression () =
  (* String *)
  assert_success @@ assert_parses_expression "Pair False \"abc\""
    (Prim ((), "Pair", [Prim ((), "False", [], []);
                        String ((), "abc")], [])) >>=? fun () ->
  (* Int *)
  assert_success @@ assert_parses_expression "Item 100"
    (Prim ((), "Item", [Int ((), Z.of_int 100)], [])) >>=? fun () ->
  (* Sequence *)
  assert_success @@ assert_parses_expression "{}"
    (Seq ((), []))

(****************************************************************************)
(* Test           *)
(****************************************************************************)

let tests =
  [
    "tokenize", (fun _ -> test_tokenize_basic ()) ;
    "test one line contract", (fun _ -> test_one_line_contract ()) ;
    "test_condition_contract", (fun _ -> test_condition_contract ()) ;
    "test_basic_parsing", (fun _ -> test_basic_parsing ()) ;
    "test_condition_contract_parsing",
    (fun _ -> test_condition_contract_parsing ()) ;
    "test_list_append_parsing",
    (fun _ -> test_list_append_parsing ()) ;
    "test_parses_expression",
    (fun _ -> test_parses_expression ()) ;
  ]

let wrap (n, f) =
  Alcotest_lwt.test_case n `Quick begin fun _ () ->
    f () >>= function
    | Ok () -> Lwt.return_unit
    | Error error ->
        Format.kasprintf Pervasives.failwith "%a" pp_print_error error
  end

let () =
  Alcotest.run ~argv:[|""|] "tezos-lib-micheline" [
    "micheline", List.map wrap tests
  ]
