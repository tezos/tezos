(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_embedded_proto_alpha
open Tezos_context.Script
open Client_alpha

module Helpers = Proto_alpha_helpers
module Assert = Helpers.Assert

open Script_located_ir

let zero_loc = { start=point_zero;
                 stop=point_zero};;

let assert_identity f x =
  Assert.equal
    (f x)
    x;;

(* Test expansion *)
Assert.equal (Michelson_macros.expand (Prim (zero_loc, "CAAR", [], None)))
  (Seq (zero_loc,
        [(Prim (zero_loc, "CAR", [], None));
         (Prim (zero_loc, "CAR", [], None)) ],
        None));;

Assert.equal (Michelson_macros.expand (Prim (zero_loc, "CAAR", [], Some "annot")))
  (Seq (zero_loc,
        [(Prim (zero_loc, "CAR", [], None));
         (Prim (zero_loc, "CAR", [], Some "annot")) ],
        None));;

assert_identity Michelson_macros.expand (Prim (zero_loc, "CAR", [], Some "annot"));;


let arg = [ Prim (zero_loc, "CAR", [], Some "annot") ] in
Assert.equal
  (Michelson_macros.expand (Prim (zero_loc, "DIP", arg, Some "new_annot")))
  (Prim (zero_loc, "DIP", arg, Some "new_annot"));
Assert.equal
  (Michelson_macros.expand (Prim (zero_loc, "DIIP", arg, None)))
  (Seq (zero_loc,
        [ Prim (zero_loc, "DIP",
                [ (Seq (zero_loc,
                        [ Prim (zero_loc, "DIP", arg, None) ],
                        None)) ],
                None) ],
        None));
Assert.equal
  (Michelson_macros.expand (Prim (zero_loc, "DIIIP", arg, None)))
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
        None));;

Assert.equal
  (Michelson_macros.expand (Prim (zero_loc, "DUUP", [], None)))
  (Seq (zero_loc,
        [ Prim (zero_loc, "DIP", [ Seq (zero_loc, [ Prim (zero_loc, "DUP", [], None) ], None) ], None) ;
          Prim (zero_loc, "SWAP", [], None) ], None));;

Assert.equal
  (Michelson_macros.expand (Prim (zero_loc, "DUUUP", [], None)))
  (Seq (zero_loc,
        [ Prim (zero_loc, "DIP",
                [ Seq (zero_loc, [
                      Prim (zero_loc, "DIP", [
                          Seq (zero_loc, [ Prim (zero_loc, "DUP", [], None) ], None)],
                            None);
                      Prim (zero_loc, "SWAP", [], None) ],
                       None) ],
                None) ;
          Prim (zero_loc, "SWAP", [], None) ], None));;

let assert_compare_macro prim_name compare_name =
  Assert.equal
    (Michelson_macros.expand (Prim (zero_loc, prim_name, [], None)))
    (Seq (zero_loc,
          [ Prim (zero_loc, "COMPARE", [], None) ;
            Prim (zero_loc, compare_name, [], None) ], None));;

let left_branch = Seq(zero_loc, [ Prim(zero_loc, "SWAP", [], None) ], None);;
let right_branch = Seq(zero_loc, [ ], None);;
let assert_compare_if_macro prim_name compare_name =
  Assert.equal
    (Michelson_macros.expand (Prim (zero_loc,
                                    prim_name,
                                    [ left_branch ; right_branch ],
                                    None)))
    (Seq (zero_loc, [ Prim(zero_loc, "COMPARE", [], None);
                      Prim(zero_loc, compare_name, [], None);
                      Prim (zero_loc, "IF", [ left_branch ; right_branch ], None) ], None)) in

assert_compare_macro "CMPEQ" "EQ";
assert_compare_macro "CMPNEQ" "NEQ";
assert_compare_macro "CMPLT" "LT";
assert_compare_macro "CMPLE" "LE";
assert_compare_macro "CMPGT" "GT";
assert_compare_macro "CMPGE" "GE";

assert_compare_if_macro "IFCMPEQ" "EQ";
assert_compare_if_macro "IFCMPNEQ" "NEQ";
assert_compare_if_macro "IFCMPLT" "LT";
assert_compare_if_macro "IFCMPLE" "LE";
assert_compare_if_macro "IFCMPGT" "GT";
assert_compare_if_macro "IFCMPGE" "GE";
Assert.equal (Michelson_macros.expand (Prim (zero_loc, "ASSERT_LEFT", [], None)))
  (Seq (zero_loc, [ Prim (zero_loc, "IF_LEFT",
                          [ Seq (zero_loc, [ ], None) ;
                            Seq (zero_loc, [ Prim(zero_loc, "FAIL", [], None) ], None) ],
                          None) ], None));
Assert.equal (Michelson_macros.expand (Prim (zero_loc, "ASSERT_RIGHT", [], None)))
  (Seq (zero_loc, [ Prim (zero_loc, "IF_LEFT",
                          [ Seq (zero_loc, [ Prim(zero_loc, "FAIL", [], None) ], None) ;
                            Seq (zero_loc, [ ], None) ],
                          None) ], None));
Assert.equal (Michelson_macros.expand (Prim (zero_loc, "IF_RIGHT", [ left_branch ; right_branch ], None)))
  (Seq (zero_loc, [ Prim (zero_loc, "IF_LEFT", [ right_branch ; left_branch ], None) ], None));
Assert.equal (Michelson_macros.expand (Prim (zero_loc, "IF_SOME", [ left_branch ; right_branch ], None)))
  (Seq (zero_loc, [ Prim (zero_loc, "IF_NONE", [ right_branch ; left_branch ], None) ], None));;


assert_identity Michelson_macros.expand (Prim (zero_loc, "PAIR", [], None));;

let expand_unexpand x =
  Michelson_macros.unexpand (Michelson_macros.expand x);;

assert_identity expand_unexpand (Prim (zero_loc, "PAAAIAIR", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "DIIIP{DROP}", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "SET_CAR", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "SET_CDR", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "DUP", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "DUUP", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "DUUUP", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "DUUUUP", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "DUUUUUP", [], None));

assert_identity expand_unexpand (Prim (zero_loc, "ASSERT_EQ", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "ASSERT_NEQ", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "ASSERT_LT", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "ASSERT_LE", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "ASSERT_GT", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "ASSERT_GE", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "ASSERT_NONE", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "ASSERT_SOME", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "ASSERT_LEFT", [], None));
assert_identity expand_unexpand (Prim (zero_loc, "ASSERT_RIGHT", [], None));

assert_identity expand_unexpand (Prim (zero_loc, "IF_RIGHT", [ left_branch ; right_branch], None));
assert_identity expand_unexpand (Prim (zero_loc, "IF_SOME", [ left_branch ; right_branch], None));

Assert.equal (Michelson_macros.expand (Prim (zero_loc, "PAAIR", [], None)))
  (Seq (zero_loc,
        [Prim
           (zero_loc,
            "DIP",
            [Seq (zero_loc, [Prim
                               (zero_loc, "PAIR", [], None)],
                  None)],
            None)],
        None));;

Assert.equal (Michelson_macros.expand (Prim (zero_loc, "PAAIAIR", [], None)))
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
        None));;

open Michelson_parser;;

let get_tokens =
  List.map (fun x -> x.token);;

Assert.equal (get_tokens @@ tokenize @@ "int")
  [ (Ident "int") ];
Assert.equal (get_tokens @@ tokenize @@ "100")
  [ (Int "100") ];
Assert.equal (get_tokens @@ tokenize @@ "(option int)")
  [ Open_paren ; Ident "option" ; Ident "int" ; Close_paren ];
Assert.equal (get_tokens @@ tokenize @@ "DIP { ADD }")
  [ Ident "DIP" ; Open_brace ; Ident "ADD" ; Close_brace ];
Assert.equal (get_tokens @@ tokenize @@ "\"hello\"")
  [ String "hello" ];
Assert.equal (get_tokens @@ tokenize @@ "parameter int;")
  [ Ident "parameter" ; Ident "int" ; Semi ];
Assert.equal (get_tokens @@ tokenize @@ "PUSH string \"abcd\";")
  [ Ident "PUSH" ; Ident "string" ; String "abcd" ; Semi ];
Assert.equal (get_tokens @@ tokenize @@ "DROP; SWAP")
  [ Ident "DROP" ; Semi ; Ident "SWAP" ];
Assert.equal (get_tokens @@ tokenize @@ "string")
  [ Ident "string" ]


let parse_expr_no_locs str =
  List.map strip_locations
  Michelson_parser.(parse_toplevel (tokenize str))

let assert_parses str parsed =
  Assert.equal (parse_expr_no_locs str) parsed;;

assert_parses "PUSH int 100"
  [ (Prim ((), "PUSH", [ Prim ((), "int", [], None) ;
                         Int ((), "100") ], None)) ];

assert_parses "DROP" [ (Prim ((), "DROP", [], None)) ];
assert_parses "DIP{DROP}"
  [ Prim ((), "DIP", [ Seq((), [ Prim ((), "DROP", [], None) ], None) ], None) ];

assert_parses "LAMBDA int int {}"
  [ Prim ((), "LAMBDA", [ Prim ((), "int", [], None) ;
                          Prim ((), "int", [], None) ;
                          Seq ((), [ ], None) ], None) ];

assert_parses "LAMBDA @name int int {}"
  [ Prim ((), "LAMBDA", [ Prim ((), "int", [], None) ;
                          Prim ((), "int", [], None) ;
                          Seq ((), [ ], None) ], Some "@name") ];

assert_parses "NIL @annot string; # comment\n"
  [ Prim ((), "NIL", [ Prim ((), "string", [], None) ], Some "@annot") ];

assert_parses "PUSH (pair bool string) (Pair False \"abc\")"
  [ Prim ((), "PUSH", [ Prim ((), "pair",
                              [ Prim ((), "bool", [], None) ;
                                Prim ((), "string", [], None) ], None) ;
                        Prim ((), "Pair", 
                                   [ Prim ((), "False", [], None) ;
                                     String ((), "abc")], None) ], None) ];
assert_parses "PUSH (list nat) (List 1 2 3)"
  [ Prim ((), "PUSH", [ Prim ((), "list",
                              [ Prim ((), "nat", [], None) ], None) ;
                        Prim ((), "List",
                              [ Int((), "1");
                                Int ((), "2");
                                Int ((), "3")],
                              None) ], None) ];
assert_parses "PUSH (lambda nat nat) {}"
  [ Prim ((), "PUSH", [ Prim ((), "lambda",
                              [ Prim ((), "nat", [], None);
                                Prim ((), "nat", [], None)], None) ;
                        Seq((), [], None)],
                        None) ];
assert_parses "PUSH key \"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx\""
  [ Prim ((), "PUSH", [ Prim ((), "key", [], None) ;
                        String ((),"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx") ],
          None) ];
assert_parses "PUSH (map int bool) (Map (Item 100 False))"
  [ Prim ((), "PUSH", [ Prim ((), "map",
                              [ Prim((), "int", [], None);
                                Prim((), "bool", [], None)], None) ;
                        Prim ((), "Map",
                              [Prim ((), "Item",
                                     [Int ((), "100");
                                      Prim ((), "False", [], None)], None)], None) ],
          None) ];
assert_parses
  "parameter int; \
return int; \ 
storage unit; \
code {}"
  [ Prim ((), "parameter", [ Prim((), "int", [], None) ], None);
    Prim ((), "return", [ Prim((), "int", [], None) ], None);
    Prim ((), "storage", [ Prim((), "unit", [], None) ], None);
    Prim ((), "code", [ Seq((), [], None) ], None)];
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
                                  Prim ((), "PAIR", [], None)], None) ], None)];
assert_parses
  "code {DUP @test; DROP}"
  [ Prim ((), "code", [Seq ((), [ Prim ((), "DUP", [], Some "@test");
                                  Prim ((), "DROP", [], None)], None)], None) ];
assert_parses
  "IF {CAR} {CDR}"
  [ Prim ((), "IF", [ Seq ((), [ Prim ((), "CAR", [], None) ], None); 
                      Seq ((), [ Prim ((), "CDR", [], None) ], None) ], None) ];
assert_parses
  "IF_NONE {FAIL} {}"
  [ Prim ((), "IF_NONE", [ Seq ((), [ Prim ((), "FAIL", [], None) ], None); 
                           Seq ((), [ ], None) ], None) ];
