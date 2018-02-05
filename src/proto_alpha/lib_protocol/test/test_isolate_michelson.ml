(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Tezos_context

let name = "Isolate Michelson"
module Logger = Logging.Make(struct let name = name end)

let (//) = Filename.concat
let contract_path =
  try Sys.argv.(1) with _ -> Filename.dirname Sys.executable_name // "contracts"

open Logger

open Isolate_helpers
open Shorthands

let (>>??) = Assert.(>>??)
let (>>=??) = Assert.(>>=??)

open Tezos_micheline

let parse_param s : Proto_alpha.Tezos_context.Script.expr =
  let (parsed, _) = Michelson_v1_parser.parse_expression s in
  parsed.expanded


let parse_script code_str storage_str : Proto_alpha.Tezos_context.Script.t =
  let code = parse_param code_str in
  let storage = parse_param storage_str in
  let return: Proto_alpha.Tezos_context.Script.t = {code ; storage} in
  return


let program param ret st code =
  let storage s = "  storage " ^ s ^ " ; \n" in
  let parameter s = "  parameter " ^ s ^ " ; \n" in
  let return s = "  return " ^ s ^ " ; \n" in
  "{\n" ^ (storage st) ^ (parameter param) ^ (return ret) ^ "  " ^ code ^ "}"

let quote s = "\"" ^ s ^ "\""

let parse_execute sb ?tc code_str param_str storage_str =
  let param = parse_param param_str in
  let script = parse_script code_str storage_str in
  Script.execute_code_pred ?tc sb script param >>=?? fun (ret, st, _, tc, nonce) ->
  let contracts = Contract.originated_contracts nonce in
  return (ret, st, tc, contracts)

let test ctxt ?tc (file_name: string) (storage: string) (input: string) =
  let full_path = contract_path // file_name ^ ".tz" in
  let file = Helpers_misc.read_file full_path in
  let spaced_file = Str.global_replace (Str.regexp_string "\n") "\n " file in
  let program = "{" ^ spaced_file ^ "}" in
  parse_execute ctxt ?tc program input storage

let test_fails ctxt ?location f s i =
  test ctxt f s i >>= fun x ->
  let msg = Option.unopt ~default:"Not failing" location in
  Assert.generic_economic_error ~msg x ;
  return ()

let string_of_canon output_prim =
  let output_can = Proto_alpha.Michelson_v1_primitives.strings_of_prims output_prim in
  let location_maker _ =
    let ret : Micheline_printer.location = {comment=None} in
    ret in
  let output_node = Micheline.inject_locations location_maker output_can in
  Format.fprintf
    Format.str_formatter "%a" Micheline_printer.print_expr output_node ;
  let output = Format.flush_str_formatter () in
  output

let test_print ctxt fn s i =
  test ctxt fn s i >>=? fun (sp, op, _, _) ->
  let ss = string_of_canon sp in
  let os = string_of_canon op in
  debug "Storage : %s" ss ;
  debug "Output : %s" os ;
  return ()


let test_output ctxt ?location (file_name: string) (storage: string) (input: string) (expected_output: string) =
  test ctxt file_name storage input >>=? fun (_storage_prim, output_prim, _tc, _contracts) ->
  let output = string_of_canon output_prim in
  let msg = Option.unopt ~default:"strings aren't equal" location in
  Assert.equal_string ~msg expected_output output ;
  return ()


let test_tc ctxt ?tc (file_name: string) (storage: string) (input: string) =
  test ctxt ?tc file_name storage input >>=? fun (_storage_prim, _output_prim, tc, _contracts) ->
  return (tc)


let test_contract ctxt ?tc (file_name: string) (storage: string) (input: string) =
  test ctxt ?tc file_name storage input >>=? fun (_storage_prim, _output_prim, tc, contracts) ->
  return (contracts, tc)



let test_storage ctxt ?location (file_name: string) (storage: string) (input: string) (expected_storage: string) =
  test ctxt file_name storage input >>=? fun (storage_prim, _output_prim, _tc, _contracts) ->
  let storage = string_of_canon storage_prim in
  let msg = Option.unopt ~default:"strings aren't equal" location in
  Assert.equal_string ~msg expected_storage storage ;
  return ()


let test_example () =
  Init.main () >>=?? fun sb ->
  let test_output ?location a b c d =
    test_output sb ?location a b c d >>= function
    | Ok(x) -> return x
    | Error(errs) -> (
        match location with
        | None -> ()
        | Some(loc) -> debug "loc : %s" loc
      ) ; Lwt.return (Error(errs))
  in
  let test_fails ?location = test_fails ?location sb in
  let test_tc ?tc = test_tc ?tc sb in
  let test_contract ?tc = test_contract ?tc sb in
  (*  let test_print ?location = test_print ?location sb in*)
  let test_storage ?location = test_storage ?location sb in

  (*  FORMAT: assert_output contract_file storage input expected_result *)
  test_output ~location: __LOC__ "ret_int" "Unit" "Unit" "300" >>=? fun _ ->

  (*  Identity on strings *)
  test_output ~location: __LOC__ "str_id" "Unit" "\"Hello\"" "\"Hello\"" >>=? fun _ ->
  test_output ~location: __LOC__ "str_id" "Unit" "\"abcd\"" "\"abcd\"" >>=? fun _ ->

  (*  Identity on pairs *)
  test_output ~location: __LOC__ "pair_id" "Unit" "(Pair True False)" "(Pair True False)" >>=? fun _ ->
  test_output ~location: __LOC__ "pair_id" "Unit" "(Pair False True)" "(Pair False True)" >>=? fun _ ->
  test_output ~location: __LOC__ "pair_id" "Unit" "(Pair True True)" "(Pair True True)" >>=? fun _ ->
  test_output ~location: __LOC__ "pair_id" "Unit" "(Pair False False)" "(Pair False False)" >>=? fun _ ->

  (*  Logical not *)
  test_output ~location: __LOC__ "not" "Unit" "True" "False" >>=? fun _ ->
  test_output ~location: __LOC__ "not" "Unit" "False" "True" >>=? fun _ ->

  (*  Logical and *)
  test_output ~location: __LOC__ "and" "Unit" "(Pair False False)" "False" >>=? fun _ ->
  test_output ~location: __LOC__ "and" "Unit" "(Pair False True)" "False" >>=? fun _ ->
  test_output ~location: __LOC__ "and" "Unit" "(Pair True False)" "False" >>=? fun _ ->
  test_output ~location: __LOC__ "and" "Unit" "(Pair True True)" "True" >>=? fun _ ->

  (*  Logical or *)
  test_output ~location: __LOC__ "or" "Unit" "(Pair False False)" "False" >>=? fun _ ->
  test_output ~location: __LOC__ "or" "Unit" "(Pair False True)" "True" >>=? fun _ ->
  test_output ~location: __LOC__ "or" "Unit" "(Pair True False)" "True" >>=? fun _ ->
  test_output ~location: __LOC__ "or" "Unit" "(Pair True True)" "True" >>=? fun _ ->

  (*  XOR *)
  test_output ~location: __LOC__ "xor" "Unit" "(Pair False False)" "False" >>=? fun _ ->
  test_output ~location: __LOC__ "xor" "Unit" "(Pair False True)" "True" >>=? fun _ ->
  test_output ~location: __LOC__ "xor" "Unit" "(Pair True False)" "True" >>=? fun _ ->
  test_output ~location: __LOC__ "xor" "Unit" "(Pair True True)" "False" >>=? fun _ ->


  (*  Build list *)
  test_output ~location: __LOC__ "build_list" "Unit" "0" "{ 0 }" >>=? fun _ ->
  test_output ~location: __LOC__ "build_list" "Unit" "3" "{ 0 ; 1 ; 2 ; 3 }" >>=? fun _ ->
  test_output ~location: __LOC__ "build_list" "Unit" "10" "{ 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 }" >>=? fun _ ->

  (*  Concatenate all strings of a list into one string *)
  test_output ~location: __LOC__ "concat_list" "Unit" "{ \"a\" ; \"b\" ; \"c\" }" "\"abc\"" >>=? fun _ ->
  test_output ~location: __LOC__ "concat_list" "Unit" "{}" "\"\"" >>=? fun _ ->
  test_output ~location: __LOC__ "concat_list" "Unit" "{ \"Hello\" ; \" \" ; \"World\" ; \"!\" }" "\"Hello World!\"" >>=? fun _ ->

  (*  Find maximum int in list -- returns None if not found *)
  test_output ~location: __LOC__ "max_in_list" "Unit" "{}" "None" >>=? fun _ ->
  test_output ~location: __LOC__ "max_in_list" "Unit" "{ 1 }" "(Some 1)" >>=? fun _ ->
  test_output ~location: __LOC__ "max_in_list" "Unit" "{ -1 }" "(Some -1)" >>=? fun _ ->
  test_output ~location: __LOC__ "max_in_list" "Unit" "{ 10 ; -1 ; -20 ; 100 ; 0 }" "(Some 100)" >>=? fun _ ->
  test_output ~location: __LOC__ "max_in_list" "Unit" "{ 10 ; -1 ; -20 ; 100 ; 0 }" "(Some 100)" >>=? fun _ ->
  test_output ~location: __LOC__ "max_in_list" "Unit" "{ -10 ; -1 ; -20 ; -100 }" "(Some -1)" >>=? fun _ ->

  (*  Identity on lists *)
  test_output ~location: __LOC__ "list_id" "Unit" "{ \"1\" ; \"2\" ; \"3\" }" "{ \"1\" ; \"2\" ; \"3\" }" >>=? fun _ ->
  test_output ~location: __LOC__ "list_id" "Unit" "{}" "{}" >>=? fun _ ->
  test_output ~location: __LOC__ "list_id" "Unit" "{ \"a\" ; \"b\" ; \"c\" }" "{ \"a\" ; \"b\" ; \"c\" }" >>=? fun _ ->

  test_output ~location: __LOC__ "list_id_map" "Unit" "{ \"1\" ; \"2\" ; \"3\" }" "{ \"1\" ; \"2\" ; \"3\" }" >>=? fun _ ->
  test_output ~location: __LOC__ "list_id_map" "Unit" "{}" "{}" >>=? fun _ ->
  test_output ~location: __LOC__ "list_id_map" "Unit" "{ \"a\" ; \"b\" ; \"c\" }" "{ \"a\" ; \"b\" ; \"c\" }" >>=? fun _ ->


  (*  Identity on maps *)
  test_output ~location: __LOC__ "map_id" "Unit" "{ Elt 0 1 }" "{ Elt 0 1 }" >>=? fun _ ->
  test_output ~location: __LOC__ "map_id" "Unit" "{ Elt 0 0 }" "{ Elt 0 0 }" >>=? fun _ ->
  test_output ~location: __LOC__ "map_id" "Unit" "{ Elt 0 0 ; Elt 3 4 }" "{ Elt 0 0 ; Elt 3 4 }" >>=? fun _ ->

  (*  Map block on lists *)
  test_output ~location: __LOC__ "list_map_block" "Unit" "{}" "{}" >>=? fun _ ->
  test_output ~location: __LOC__ "list_map_block" "Unit" "{ 1 ; 1 ; 1 ; 1 }" "{ 1 ; 2 ; 3 ; 4 }" >>=? fun _ ->
  test_output ~location: __LOC__ "list_map_block" "Unit" "{ 1 ; 2 ; 3 ; 0 }" "{ 1 ; 3 ; 5 ; 3 }" >>=? fun _ ->

  (*  List iter *)
  test_output ~location: __LOC__ "list_iter" "Unit" "{ 10 ; 2 ; 1 }" "20" >>=? fun _ ->
  test_output ~location: __LOC__ "list_iter" "Unit" "{ 3 ; 6 ; 9 }" "162" >>=? fun _ ->

  test_output ~location: __LOC__ "list_iter2" "Unit" "{ \"a\" ; \"b\" ; \"c\" }" "\"cba\"" >>=? fun _ ->
  test_output ~location: __LOC__ "list_iter2" "Unit" "{}" "\"\"" >>=? fun _ ->


  (*  Identity on sets *)
  test_output ~location: __LOC__ "set_id" "Unit" "{ \"a\" ; \"b\" ; \"c\" }" "{ \"a\" ; \"b\" ; \"c\" }" >>=? fun _ ->
  test_output ~location: __LOC__ "set_id" "Unit" "{}" "{}" >>=? fun _ ->
  test_output ~location: __LOC__ "set_id" "Unit" "{ \"asdf\" ; \"bcde\" }" "{ \"asdf\" ; \"bcde\" }" >>=? fun _ ->

  (*  Set member -- set is in storage *)
  test_output ~location: __LOC__ "set_member" "{}" "\"Hi\"" "False" >>=? fun _ ->
  test_output ~location: __LOC__ "set_member" "{ \"Hi\" }" "\"Hi\"" "True" >>=? fun _ ->
  test_output ~location: __LOC__ "set_member" "{ \"Hello\" ; \"World\" }" "\"\"" "False" >>=? fun _ ->

  (*  Set size *)
  test_output ~location: __LOC__ "set_size" "Unit" "{}" "0" >>=? fun _ ->
  test_output ~location: __LOC__ "set_size" "Unit" "{ 1 }" "1" >>=? fun _ ->
  test_output ~location: __LOC__ "set_size" "Unit" "{ 1 ; 2 ; 3 }" "3" >>=? fun _ ->
  test_output ~location: __LOC__ "set_size" "Unit" "{ 1 ; 2 ; 3 ; 4 ; 5 ; 6 }" "6" >>=? fun _ ->

  (*  Set iter *)
  test_output ~location: __LOC__ "set_iter" "Unit" "{}" "0" >>=? fun _ ->
  test_output ~location: __LOC__ "set_iter" "Unit" "{ 1 }" "1" >>=? fun _ ->
  test_output ~location: __LOC__ "set_iter" "Unit" "{ -100 ; 1 ; 2 ; 3 }" "-94" >>=? fun _ ->

  (*  Map size *)
  test_output ~location: __LOC__ "map_size" "Unit" "{}" "0" >>=? fun _ ->
  test_output ~location: __LOC__ "map_size" "Unit" "{ Elt \"a\" 1 }" "1" >>=? fun _ ->
  test_output ~location: __LOC__ "map_size" "Unit" "{ Elt \"a\" 1 ; Elt \"b\" 2 ; Elt \"c\" 3 }" "3" >>=? fun _ ->
  test_output ~location: __LOC__ "map_size" "Unit" "{ Elt \"a\" 1 ; Elt \"b\" 2 ; Elt \"c\" 3 ; Elt \"d\" 4 ; Elt \"e\" 5 ; Elt \"f\" 6 }" "6" >>=? fun _ ->

  (*  Contains all elements -- does the second list contain all of the same elements *)
  (*  as the first one? I'm ignoring element multiplicity *)
  test_output ~location: __LOC__ "contains_all" "Unit" "(Pair {} {})" "True" >>=? fun _ ->
  test_output ~location: __LOC__ "contains_all" "Unit" "(Pair { \"a\" } { \"B\" })" "False" >>=? fun _ ->
  test_output ~location: __LOC__ "contains_all" "Unit" "(Pair { \"A\" } { \"B\" })" "False" >>=? fun _ ->
  test_output ~location: __LOC__ "contains_all" "Unit" "(Pair { \"B\" } { \"B\" })" "True" >>=? fun _ ->
  test_output ~location: __LOC__ "contains_all" "Unit" "(Pair { \"B\" ; \"C\" ; \"asdf\" } { \"B\" ; \"B\" ; \"asdf\" ; \"C\" })" "True" >>=? fun _ ->
  test_output ~location: __LOC__ "contains_all" "Unit" "(Pair { \"B\" ; \"B\" ; \"asdf\" ; \"C\" } { \"B\" ; \"C\" ; \"asdf\" })" "True" >>=? fun _ ->

  (*  Concatenate the string in storage with all strings in the given list *)
  test_output ~location: __LOC__ "concat_hello" "Unit" "{ \"World!\" }" "{ \"Hello World!\" }" >>=? fun _ ->
  test_output ~location: __LOC__ "concat_hello" "Unit" "{}" "{}" >>=? fun _ ->
  test_output ~location: __LOC__ "concat_hello" "Unit" "{ \"test1\" ; \"test2\" }" "{ \"Hello test1\" ; \"Hello test2\" }" >>=? fun _ ->

  (*  Create an empty map and add a string to it *)
  test_output ~location: __LOC__ "empty_map" "Unit" "Unit" "{ Elt \"hello\" \"world\" }" >>=? fun _ ->

  (*  Get the value stored at the given key in the map *)
  test_output ~location: __LOC__ "get_map_value" "{ Elt \"hello\" \"hi\" }" "\"hello\"" "(Some \"hi\")" >>=? fun _ ->
  test_output ~location: __LOC__ "get_map_value" "{ Elt \"hello\" \"hi\" }" "\"\"" "None" >>=? fun _ ->
  test_output ~location: __LOC__ "get_map_value" "{ Elt \"1\" \"one\" ; Elt \"2\" \"two\" }" "\"1\"" "(Some \"one\")" >>=? fun _ ->

  (*  Map iter *)
  test_output ~location: __LOC__ "map_iter" "Unit" "{ Elt 0 100 ; Elt 2 100 }" "(Pair 2 200)" >>=? fun _ ->
  test_output ~location: __LOC__ "map_iter" "Unit" "{ Elt 1 1 ; Elt 2 100 }" "(Pair 3 101)" >>=? fun _ ->

  (*  Return True if True branch of if was taken and False otherwise *)
  test_output ~location: __LOC__ "if" "Unit" "True" "True" >>=? fun _ ->
  test_output ~location: __LOC__ "if" "Unit" "False" "False" >>=? fun _ ->

  (*  Generate a pair of or types *)
  test_output ~location: __LOC__ "swap_left_right" "Unit" "(Left True)" "(Right True)" >>=? fun _ ->
  test_output ~location: __LOC__ "swap_left_right" "Unit" "(Right \"a\")" "(Left \"a\")" >>=? fun _ ->

  (*  Reverse a list *)
  test_output ~location: __LOC__ "reverse" "Unit" "{}" "{}" >>=? fun _ ->
  test_output ~location: __LOC__ "reverse" "Unit" "{ \"c\" ; \"b\" ; \"a\" }" "{ \"a\" ; \"b\" ; \"c\" }" >>=? fun _ ->
  test_output ~location: __LOC__ "reverse_loop" "Unit" "{}" "{}" >>=? fun _ ->
  test_output ~location: __LOC__ "reverse_loop" "Unit" "{ \"c\" ; \"b\" ; \"a\" }" "{ \"a\" ; \"b\" ; \"c\" }" >>=? fun _ ->

  (*  Reverse using LOOP_LEFT *)
  test_output ~location: __LOC__ "loop_left" "Unit" "{}" "{}" >>=? fun _ ->
  test_output ~location: __LOC__ "loop_left" "Unit" "{ \"c\" ; \"b\" ; \"a\" }" "{ \"a\" ; \"b\" ; \"c\" }" >>=? fun _ ->

  (*  Exec concat contract *)
  test_output ~location: __LOC__ "exec_concat" "Unit" "\"\"" "\"_abc\"" >>=? fun _ ->
  test_output ~location: __LOC__ "exec_concat" "Unit" "\"test\"" "\"test_abc\"" >>=? fun _ ->

  (*  Get current steps to quota *)
  test_output ~location: __LOC__ "steps_to_quota" "Unit" "Unit" "39991" >>=? fun _ ->

  let bootstrap_0 = List.nth Account.bootstrap_accounts 0 in
  get_balance_res bootstrap_0 sb >>=?? fun _balance ->
  let amount = Proto_alpha.Tezos_context.Tez.to_string @@ Cast.cents_of_int Script.init_amount in
  (*  Get the current balance of the contract *)
  test_output ~location: __LOC__ "balance" "Unit" "Unit" ("\"" ^ amount ^ "\"") >>=? fun _ ->

  (*  Test comparisons on tez { EQ ; GT ; LT ; GE ; LE } *)
  test_output ~location: __LOC__ "compare" "Unit" "(Pair \"1.00\" \"2.00\")" "{ False ; False ; True ; False ; True }" >>=? fun _ ->
  test_output ~location: __LOC__ "compare" "Unit" "(Pair \"2.00\" \"1.00\")" "{ False ; True ; False ; True ; False }" >>=? fun _ ->
  test_output ~location: __LOC__ "compare" "Unit" "(Pair \"2.37\" \"2.37\")" "{ True ; False ; False ; True ; True }" >>=? fun _ ->

  (*  Test addition and subtraction on tez *)
  test_output ~location: __LOC__ "tez_add_sub" "Unit" "(Pair \"2\" \"1\")" "(Pair \"3\" \"1\")" >>=? fun _ ->
  test_output ~location: __LOC__ "tez_add_sub" "Unit" "(Pair \"2.31\" \"1.01\")" "(Pair \"3.32\" \"1.3\")" >>=? fun _ ->

  (*  Test get first element of list *)
  test_output ~location: __LOC__ "first" "Unit" "{ 1 ; 2 ; 3 ; 4 }" "1" >>=? fun _ ->
  test_output ~location: __LOC__ "first" "Unit" "{ 4 }" "4" >>=? fun _ ->

  (*  Hash input string *)
  (*  Test assumed to be correct -- hash is based on encoding of AST *)
  test_output ~location: __LOC__ "hash_string" "Unit" "\"abcdefg\"" "\"exprv3MnhXvjthGzZ7jDtXRRFremZyey9rsGtL7JRkeaQX1fThN7WF\"" >>=? fun _ ->
  test_output ~location: __LOC__ "hash_string" "Unit" "\"12345\"" "\"expru81QVHsW2qaWLNHnMHSxDNhqtat17ajadri6mKUvXyc2EWHZC3\"" >>=? fun _ ->

  (*  Test ASSERT *)
  test_output ~location: __LOC__ "assert" "Unit" "True" "Unit" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert" "Unit" "False" >>=? fun _ ->

  (*  COMPARE ; ASSERT_ *)
  test_output ~location: __LOC__ "assert_eq" "Unit" "(Pair -1 -1)" "Unit" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_eq" "Unit" "(Pair 0 -1)" >>=? fun _ ->

  test_output ~location: __LOC__ "assert_eq" "Unit" "(Pair -1 -1)" "Unit" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_eq" "Unit" "(Pair 0 -1)" >>=? fun _ ->

  test_output ~location: __LOC__ "assert_neq" "Unit" "(Pair 0 -1)" "Unit" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_neq" "Unit" "(Pair -1 -1)" >>=? fun _ ->

  test_output ~location: __LOC__ "assert_lt" "Unit" "(Pair -1 0)" "Unit" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_lt" "Unit" "(Pair 0 -1)" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_lt" "Unit" "(Pair 0 0)" >>=? fun _ ->

  test_output ~location: __LOC__ "assert_le" "Unit" "(Pair 0 0)" "Unit" >>=? fun _ ->
  test_output ~location: __LOC__ "assert_le" "Unit" "(Pair -1 0)" "Unit" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_le" "Unit" "(Pair 0 -1)" >>=? fun _ ->

  test_output ~location: __LOC__ "assert_gt" "Unit" "(Pair 0 -1)" "Unit" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_gt" "Unit" "(Pair -1 0)" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_gt" "Unit" "(Pair 0 0)" >>=? fun _ ->

  test_output ~location: __LOC__ "assert_ge" "Unit" "(Pair 0 0)" "Unit" >>=? fun _ ->
  test_output ~location: __LOC__ "assert_ge" "Unit" "(Pair 0 -1)" "Unit" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_ge" "Unit" "(Pair -1 0)" >>=? fun _ ->

  (*  ASSERT_CMP *)
  test_output ~location: __LOC__ "assert_cmpeq" "Unit" "(Pair -1 -1)" "Unit" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_cmpeq" "Unit" "(Pair 0 -1)" >>=? fun _ ->

  test_output ~location: __LOC__ "assert_cmpeq" "Unit" "(Pair -1 -1)" "Unit" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_cmpeq" "Unit" "(Pair 0 -1)" >>=? fun _ ->

  test_output ~location: __LOC__ "assert_cmpneq" "Unit" "(Pair 0 -1)" "Unit" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_cmpneq" "Unit" "(Pair -1 -1)" >>=? fun _ ->

  test_output ~location: __LOC__ "assert_cmplt" "Unit" "(Pair -1 0)" "Unit" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_cmplt" "Unit" "(Pair 0 -1)" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_cmplt" "Unit" "(Pair 0 0)" >>=? fun _ ->

  test_output ~location: __LOC__ "assert_cmple" "Unit" "(Pair 0 0)" "Unit" >>=? fun _ ->
  test_output ~location: __LOC__ "assert_cmple" "Unit" "(Pair -1 0)" "Unit" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_cmple" "Unit" "(Pair 0 -1)" >>=? fun _ ->

  test_output ~location: __LOC__ "assert_cmpgt" "Unit" "(Pair 0 -1)" "Unit" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_cmpgt" "Unit" "(Pair -1 0)" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_cmpgt" "Unit" "(Pair 0 0)" >>=? fun _ ->

  test_output ~location: __LOC__ "assert_cmpge" "Unit" "(Pair 0 0)" "Unit" >>=? fun _ ->
  test_output ~location: __LOC__ "assert_cmpge" "Unit" "(Pair 0 -1)" "Unit" >>=? fun _ ->
  test_fails ~location: __LOC__ "assert_cmpge" "Unit" "(Pair -1 0)" >>=? fun _ ->

  (*  IF_SOME *)
  test_output ~location: __LOC__ "if_some" "Unit" "(Some \"hello\")" "\"hello\"" >>=? fun _ ->
  test_output ~location: __LOC__ "if_some" "Unit" "None" "\"\"" >>=? fun _ ->

  (*  Tests the SET_CAR and SET_CDR instructions *)
  test_output ~location: __LOC__ "set_car" "(Pair \"hello\" 0)" "\"world\"" "(Pair \"world\" 0)" >>=? fun _ ->
  test_output ~location: __LOC__ "set_car" "(Pair \"hello\" 0)" "\"abc\"" "(Pair \"abc\" 0)" >>=? fun _ ->
  test_output ~location: __LOC__ "set_car" "(Pair \"hello\" 0)" "\"\"" "(Pair \"\" 0)" >>=? fun _ ->

  test_output ~location: __LOC__ "set_cdr" "(Pair \"hello\" 0)" "1" "(Pair \"hello\" 1)" >>=? fun _ ->
  test_output ~location: __LOC__ "set_cdr" "(Pair \"hello\" 500)" "3" "(Pair \"hello\" 3)" >>=? fun _ ->
  test_output ~location: __LOC__ "set_cdr" "(Pair \"hello\" 7)" "100" "(Pair \"hello\" 100)" >>=? fun _ ->

  test_storage ~location: __LOC__ "set_caddaadr" "(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 \"0\") 4) 5))) 6)" "\"3\"" "(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 \"3\") 4) 5))) 6)" >>=? fun _ ->

  test_storage ~location: __LOC__ "map_caddaadr" "(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 \"0\") 4) 5))) 6)" "Unit" "(Pair (Pair 1 (Pair 2 (Pair (Pair (Pair 3 \"1\") 4) 5))) 6)" >>=? fun _ ->

  (*  Did the given key sign the string? (key is bootstrap1) *)
  test_output ~location: __LOC__ "check_signature" "(Pair \"26981d372a7b3866621bf79713d249197fe6d518ef702fa65738e1715bde9da54df04fefbcc84287ecaa9f74ad9296462731aa24bbcece63c6bf73a8f5752309\" \"hello\")" "\"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav\"" "True" >>=? fun _ ->

  test_output ~location: __LOC__ "check_signature" "(Pair \"26981d372a7b3866621bf79713d249197fe6d518ef702fa65738e1715bde9da54df04fefbcc84287ecaa9f74ad9296462731aa24bbcece63c6bf73a8f5752309\" \"abcd\")" "\"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav\"" "False" >>=? fun _ ->

  (*  Convert a public key to a public key hash *)
  test_output ~location: __LOC__ "hash_key" "Unit" "\"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav\"" "\"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx\"" >>=? fun _ ->
  test_output ~location: __LOC__ "hash_key" "Unit" "\"edpkuJqtDcA2m2muMxViSM47MPsGQzmyjnNTawUPqR8vZTAMcx61ES\"" "\"tz1XPTDmvT3vVE5Uunngmixm7gj7zmdbPq6k\"" >>=? fun _ ->

  (*  Test timestamp operations *)
  test_output ~location: __LOC__ "add_timestamp_delta" "Unit" "(Pair 100 100)" "\"1970-01-01T00:03:20Z\"" >>=? fun _ ->
  test_output ~location: __LOC__ "add_timestamp_delta" "Unit" "(Pair 100 -100)" "\"1970-01-01T00:00:00Z\"" >>=? fun _ ->
  test_output ~location: __LOC__ "add_timestamp_delta" "Unit" "(Pair \"1970-01-01T00:00:00Z\" 0)" "\"1970-01-01T00:00:00Z\"" >>=? fun _ ->

  test_output ~location: __LOC__ "add_delta_timestamp" "Unit" "(Pair 100 100)" "\"1970-01-01T00:03:20Z\"" >>=? fun _ ->
  test_output ~location: __LOC__ "add_delta_timestamp" "Unit" "(Pair -100 100)" "\"1970-01-01T00:00:00Z\"" >>=? fun _ ->
  test_output ~location: __LOC__ "add_delta_timestamp" "Unit" "(Pair 0 \"1970-01-01T00:00:00Z\")" "\"1970-01-01T00:00:00Z\"" >>=? fun _ ->

  test_output ~location: __LOC__ "sub_timestamp_delta" "Unit" "(Pair 100 100)" "\"1970-01-01T00:00:00Z\"" >>=? fun _ ->
  test_output ~location: __LOC__ "sub_timestamp_delta" "Unit" "(Pair 100 -100)" "\"1970-01-01T00:03:20Z\"" >>=? fun _ ->
  test_output ~location: __LOC__ "sub_timestamp_delta" "Unit" "(Pair 100 2000000000000000000)" "-1999999999999999900" >>=? fun _ ->

  test_output ~location: __LOC__ "diff_timestamps" "Unit" "(Pair 0 0)" "0" >>=? fun _ ->
  test_output ~location: __LOC__ "diff_timestamps" "Unit" "(Pair 0 1)" "-1" >>=? fun _ ->
  test_output ~location: __LOC__ "diff_timestamps" "Unit" "(Pair 1 0)" "1" >>=? fun _ ->
  test_output ~location: __LOC__ "diff_timestamps" "Unit" "(Pair \"1970-01-01T00:03:20Z\" \"1970-01-01T00:00:00Z\")" "200" >>=? fun _ ->

  (* Test NOW *)
  let now = sb.tezos_header.shell.timestamp in
  let now_str = quote @@ Tezos_base.Time.to_notation now in
  test_storage ~location: __LOC__ "store_now" "\"1970-01-01T00:03:20Z\"" "Unit" now_str >>=? fun _ ->

  (* Test TRANSFER_TO *)
  Account.make_account ~tc: sb.tezos_context >>=?? fun (account, tc) ->
  let account_str = quote @@ Ed25519.Public_key_hash.to_b58check account.hpub in
  test_tc ~tc "transfer_to" "Unit" account_str >>=? fun tc ->
  let amount = Account.init_amount + 100 in
  Assert.equal_cents_balance ~tc (account.contract, amount * 100) >>=?? fun _ ->

  (* Test CREATE_ACCOUNT *)
  Account.make_account ~tc: sb.tezos_context >>=?? fun (account, tc) ->
  let account_str = quote @@ Ed25519.Public_key_hash.to_b58check account.hpub in
  test_contract ~tc "create_account" account_str account_str >>=? fun (cs, tc) ->
  Assert.equal_int 1 @@ List.length cs ;

  (* Test CREATE_CONTRACT *)
  test_contract ~tc "create_contract" account_str account_str >>=? fun (cs, tc) ->
  Assert.equal_int 1 @@ List.length cs ;
  let contract = List.hd cs in
  Proto_alpha.Tezos_context.Contract.get_script tc contract >>=?? fun res ->
  let script = Option.unopt_exn (Failure "get_script") res in
  Script.execute_code_pred ~tc sb script (parse_param "\"abc\"") >>=?? fun (_, ret, _, _, _) ->
  Assert.equal_string ~msg: __LOC__ "\"abc\"" @@ string_of_canon ret ;

  (* Test DEFAULT_ACCOUNT *)
  let account = Account.new_account () in
  let b_str = quote @@ Ed25519.Public_key_hash.to_b58check account.hpub in
  test_contract ~tc "default_account" "Unit" b_str >>=? fun (_cs, tc) ->
  Assert.equal_cents_balance ~tc (account.contract, 100 * 100) >>=?? fun _ ->
  return ()


let test_program () =
  Init.main () >>=?? fun sb ->
  let id_code = "code
     { DUP ;
       PAIR ;
       CAR }" in
  let id_int_program =
    program "int" "int" "int" id_code in
  let id_ill_param_program =
    program "string" "int" "string" id_code in
  let id_ill_return_program =
    program "int" "string" "int" id_code in
  let id_pbool_program =
    program "(pair bool bool)" "(pair bool bool)" "unit" id_code in
  let push_300_code = "code
     { CAR ;
       PUSH nat 300 ;
       PAIR }" in
  let push_300 =
    program "unit" "nat" "unit" push_300_code in
  parse_execute sb id_int_program "2" "3" >>=? fun _ ->
  parse_execute sb id_ill_param_program "2" "3" >>= fun x ->
  Assert.ill_typed_data_error ~msg: "Good data type" x ;
  parse_execute sb id_ill_return_program "2" "3" >>= fun x ->
  Assert.ill_typed_return_error ~msg: "Good return type" x ;
  parse_execute sb push_300 "Unit" "Unit" >>=? fun _ ->
  parse_execute sb id_pbool_program "(Pair True True)" "Unit" >>=? fun _ ->
  return ()

let tests = [
  "michelson.example", (fun _ -> test_example ()) ;
  "michelson.program", (fun _ -> test_program ()) ;
]
