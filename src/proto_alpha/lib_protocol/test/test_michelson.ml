(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Proto_alpha
open Alpha_context

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

let parse_param s : Proto_alpha.Alpha_context.Script.expr =
  let (parsed, _) = Michelson_v1_parser.parse_expression s in
  parsed.expanded


let parse_script code_str storage_str : Proto_alpha.Alpha_context.Script.t =
  let code = parse_param code_str in
  let storage = parse_param storage_str in
  let return: Proto_alpha.Alpha_context.Script.t = {code ; storage} in
  return


let program param st code =
  let storage s = "  storage " ^ s ^ " ; \n" in
  let parameter s = "  parameter " ^ s ^ " ; \n" in
  "{\n" ^ (storage st) ^ (parameter param) ^ "  " ^ code ^ "}"

let quote s = "\"" ^ s ^ "\""

let parse_execute sb ?tc code_str param_str storage_str =
  let param = parse_param param_str in
  let script = parse_script code_str storage_str in
  Script.execute_code_pred ?tc sb script param
  >>=?? fun (dst, { ctxt = tc ; operations = ops ;
                    origination_nonce = nonce ; big_map_diff = bgm }) ->
  let payer =
    (List.hd Account.bootstrap_accounts).contract in
  Proto_alpha.Apply.apply_internal_manager_operations tc ~payer nonce ops >>=?? fun (tc, nonce, err, _, ops) ->
  let contracts = Contract.originated_contracts nonce in
  match err with
  | None ->
      let tc = Proto_alpha.Alpha_context.Gas.set_unlimited tc in
      Proto_alpha.Alpha_context.Contract.get_storage tc dst >>=?? begin function
        | (_, None) -> assert false
        | (tc, Some st) -> return (st, ops, tc, contracts, bgm)
      end
  | Some err ->
      Lwt.return (Alpha_environment.wrap_error (Error_monad.error (List.hd err)))

let test ctxt ?tc (file_name: string) (storage: string) (input: string) =
  let full_path = contract_path // file_name ^ ".tz" in
  let file = Helpers_misc.read_file full_path in
  let spaced_file = Re.Str.(global_replace (regexp_string "\n") "\n " file) in
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
  test ctxt fn s i >>=? fun (sp, _, _, _, _bgm) ->
  let ss = string_of_canon sp in
  debug "Storage : %s" ss ;
  return ()

let test_tc ctxt ?tc (file_name: string) (storage: string) (input: string) =
  test ctxt ?tc file_name storage input >>=? fun (_storage_prim, _output_prim, tc, _contracts, _bgm) ->
  return (tc)


let test_contract ctxt ?tc (file_name: string) (storage: string) (input: string) =
  test ctxt ?tc file_name storage input >>=? fun (_storage_prim, _output_prim, tc, contracts, _bgm) ->
  return (contracts, tc)



let test_storage ctxt ?location (file_name: string) (storage: string) (input: string) (expected_storage: string) =
  let msg = Option.unopt ~default:"strings aren't equal" location in
  generic_trace "%s" msg @@
  test ctxt file_name storage input >>=? fun (storage_prim, _output_prim, _tc, _contracts, _bgm) ->
  let storage = string_of_canon storage_prim in
  Assert.equal_string ~msg expected_storage storage ;
  return ()

let test_success ctxt ?location (file_name: string) (storage: string) (input: string) =
  let msg = Option.unopt ~default:"strings aren't equal" location in
  generic_trace "%s" msg @@
  test ctxt file_name storage input >>=? fun (_storage_prim, _output_prim, _tc, _contracts, _bgm) ->
  return ()


let test_example () =
  Init.main () >>=?? fun sb ->
  let test_output ?location a b c d =
    test_storage sb ?location a b c d >>= function
    | Ok(x) -> return x
    | Error(errs) -> (
        match location with
        | None -> ()
        | Some(loc) -> debug "loc : %s" loc
      ) ; Lwt.return (Error(errs))
  in
  let test_fails ?location = test_fails ?location sb in
  let test_success ?location = test_success ?location sb in
  let test_tc ?tc = test_tc ?tc sb in
  let test_contract ?tc = test_contract ?tc sb in
  (*  let test_print ?location = test_print ?location sb in*)
  let test_storage ?location = test_storage ?location sb in

  (*  FORMAT: assert_output contract_file storage input expected_result *)
  test_output ~location: __LOC__ "ret_int" "None" "Unit" "(Some 300)" >>=? fun _ ->

  (*  Identity on strings *)
  test_output ~location: __LOC__ "str_id" "None" "\"Hello\"" "(Some \"Hello\")" >>=? fun _ ->
  test_output ~location: __LOC__ "str_id" "None" "\"abcd\"" "(Some \"abcd\")" >>=? fun _ ->

  (*  Identity on pairs *)
  test_output ~location: __LOC__ "pair_id" "None" "(Pair True False)" "(Some (Pair True False))" >>=? fun _ ->
  test_output ~location: __LOC__ "pair_id" "None" "(Pair False True)" "(Some (Pair False True))" >>=? fun _ ->
  test_output ~location: __LOC__ "pair_id" "None" "(Pair True True)" "(Some (Pair True True))" >>=? fun _ ->
  test_output ~location: __LOC__ "pair_id" "None" "(Pair False False)" "(Some (Pair False False))" >>=? fun _ ->

  (*  Logical not *)
  test_output ~location: __LOC__ "not" "None" "True" "(Some False)" >>=? fun _ ->
  test_output ~location: __LOC__ "not" "None" "False" "(Some True)" >>=? fun _ ->

  (*  Logical and *)
  test_output ~location: __LOC__ "and" "None" "(Pair False False)" "(Some False)" >>=? fun _ ->
  test_output ~location: __LOC__ "and" "None" "(Pair False True)" "(Some False)" >>=? fun _ ->
  test_output ~location: __LOC__ "and" "None" "(Pair True False)" "(Some False)" >>=? fun _ ->
  test_output ~location: __LOC__ "and" "None" "(Pair True True)" "(Some True)" >>=? fun _ ->

  (*  Logical or *)
  test_output ~location: __LOC__ "or" "None" "(Pair False False)" "(Some False)" >>=? fun _ ->
  test_output ~location: __LOC__ "or" "None" "(Pair False True)" "(Some True)" >>=? fun _ ->
  test_output ~location: __LOC__ "or" "None" "(Pair True False)" "(Some True)" >>=? fun _ ->
  test_output ~location: __LOC__ "or" "None" "(Pair True True)" "(Some True)" >>=? fun _ ->

  (*  XOR *)
  test_output ~location: __LOC__ "xor" "None" "(Pair False False)" "(Some False)" >>=? fun _ ->
  test_output ~location: __LOC__ "xor" "None" "(Pair False True)" "(Some True)" >>=? fun _ ->
  test_output ~location: __LOC__ "xor" "None" "(Pair True False)" "(Some True)" >>=? fun _ ->
  test_output ~location: __LOC__ "xor" "None" "(Pair True True)" "(Some False)" >>=? fun _ ->


  (*  Build list *)
  test_output ~location: __LOC__ "build_list" "{111}" "0" "{ 0 }" >>=? fun _ ->
  test_output ~location: __LOC__ "build_list" "{111}" "3" "{ 0 ; 1 ; 2 ; 3 }" >>=? fun _ ->
  test_output ~location: __LOC__ "build_list" "{111}" "10" "{ 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 }" >>=? fun _ ->

  (*  Concatenate all strings of a list into one string *)
  test_output ~location: __LOC__ "concat_list" "\"?\"" "{ \"a\" ; \"b\" ; \"c\" }" "\"abc\"" >>=? fun _ ->
  test_output ~location: __LOC__ "concat_list" "\"?\"" "{}" "\"\"" >>=? fun _ ->
  test_output ~location: __LOC__ "concat_list" "\"?\"" "{ \"Hello\" ; \" \" ; \"World\" ; \"!\" }" "\"Hello World!\"" >>=? fun _ ->

  (*  Find maximum int in list -- returns None if not found *)
  test_output ~location: __LOC__ "max_in_list" "None" "{}" "None" >>=? fun _ ->
  test_output ~location: __LOC__ "max_in_list" "None" "{ 1 }" "(Some 1)" >>=? fun _ ->
  test_output ~location: __LOC__ "max_in_list" "None" "{ -1 }" "(Some -1)" >>=? fun _ ->
  test_output ~location: __LOC__ "max_in_list" "None" "{ 10 ; -1 ; -20 ; 100 ; 0 }" "(Some 100)" >>=? fun _ ->
  test_output ~location: __LOC__ "max_in_list" "None" "{ 10 ; -1 ; -20 ; 100 ; 0 }" "(Some 100)" >>=? fun _ ->
  test_output ~location: __LOC__ "max_in_list" "None" "{ -10 ; -1 ; -20 ; -100 }" "(Some -1)" >>=? fun _ ->

  (*  Identity on lists *)
  test_output ~location: __LOC__ "list_id" "{\"?\"}" "{ \"1\" ; \"2\" ; \"3\" }" "{ \"1\" ; \"2\" ; \"3\" }" >>=? fun _ ->
  test_output ~location: __LOC__ "list_id" "{\"?\"}" "{}" "{}" >>=? fun _ ->
  test_output ~location: __LOC__ "list_id" "{\"?\"}" "{ \"a\" ; \"b\" ; \"c\" }" "{ \"a\" ; \"b\" ; \"c\" }" >>=? fun _ ->

  test_output ~location: __LOC__ "list_id_map" "{\"?\"}" "{ \"1\" ; \"2\" ; \"3\" }" "{ \"1\" ; \"2\" ; \"3\" }" >>=? fun _ ->
  test_output ~location: __LOC__ "list_id_map" "{\"?\"}" "{}" "{}" >>=? fun _ ->
  test_output ~location: __LOC__ "list_id_map" "{\"?\"}" "{ \"a\" ; \"b\" ; \"c\" }" "{ \"a\" ; \"b\" ; \"c\" }" >>=? fun _ ->


  (*  Identity on maps *)
  test_output ~location: __LOC__ "map_id" "{}" "{ Elt 0 1 }" "{ Elt 0 1 }" >>=? fun _ ->
  test_output ~location: __LOC__ "map_id" "{}" "{ Elt 0 0 }" "{ Elt 0 0 }" >>=? fun _ ->
  test_output ~location: __LOC__ "map_id" "{}" "{ Elt 0 0 ; Elt 3 4 }" "{ Elt 0 0 ; Elt 3 4 }" >>=? fun _ ->

  (*  Map block on lists *)
  test_output ~location: __LOC__ "list_map_block" "{111}" "{}" "{}" >>=? fun _ ->
  test_output ~location: __LOC__ "list_map_block" "{111}" "{ 1 ; 1 ; 1 ; 1 }" "{ 1 ; 2 ; 3 ; 4 }" >>=? fun _ ->
  test_output ~location: __LOC__ "list_map_block" "{111}" "{ 1 ; 2 ; 3 ; 0 }" "{ 1 ; 3 ; 5 ; 3 }" >>=? fun _ ->

  (*  List iter *)
  test_output ~location: __LOC__ "list_iter" "111" "{ 10 ; 2 ; 1 }" "20" >>=? fun _ ->
  test_output ~location: __LOC__ "list_iter" "111" "{ 3 ; 6 ; 9 }" "162" >>=? fun _ ->

  test_output ~location: __LOC__ "list_iter2" "\"?\"" "{ \"a\" ; \"b\" ; \"c\" }" "\"cba\"" >>=? fun _ ->
  test_output ~location: __LOC__ "list_iter2" "\"?\"" "{}" "\"\"" >>=? fun _ ->


  (*  Identity on sets *)
  test_output ~location: __LOC__ "set_id" "{\"?\"}" "{ \"a\" ; \"b\" ; \"c\" }" "{ \"a\" ; \"b\" ; \"c\" }" >>=? fun _ ->
  test_output ~location: __LOC__ "set_id" "{\"?\"}" "{}" "{}" >>=? fun _ ->
  test_output ~location: __LOC__ "set_id" "{\"?\"}" "{ \"asdf\" ; \"bcde\" }" "{ \"asdf\" ; \"bcde\" }" >>=? fun _ ->

  (*  Set member -- set is in storage *)
  test_output ~location: __LOC__ "set_member" "(Pair {} None)" "\"Hi\"" "(Pair {} (Some False))" >>=? fun _ ->
  test_output ~location: __LOC__ "set_member" "(Pair { \"Hi\" } None)" "\"Hi\"" "(Pair { \"Hi\" } (Some True))" >>=? fun _ ->
  test_output ~location: __LOC__ "set_member" "(Pair { \"Hello\" ; \"World\" } None)" "\"\"" "(Pair { \"Hello\" ; \"World\" } (Some False))" >>=? fun _ ->

  (*  Set size *)
  test_output ~location: __LOC__ "set_size" "111" "{}" "0" >>=? fun _ ->
  test_output ~location: __LOC__ "set_size" "111" "{ 1 }" "1" >>=? fun _ ->
  test_output ~location: __LOC__ "set_size" "111" "{ 1 ; 2 ; 3 }" "3" >>=? fun _ ->
  test_output ~location: __LOC__ "set_size" "111" "{ 1 ; 2 ; 3 ; 4 ; 5 ; 6 }" "6" >>=? fun _ ->

  (*  Set iter *)
  test_output ~location: __LOC__ "set_iter" "111" "{}" "0" >>=? fun _ ->
  test_output ~location: __LOC__ "set_iter" "111" "{ 1 }" "1" >>=? fun _ ->
  test_output ~location: __LOC__ "set_iter" "111" "{ -100 ; 1 ; 2 ; 3 }" "-94" >>=? fun _ ->

  (*  Map size *)
  test_output ~location: __LOC__ "map_size" "111" "{}" "0" >>=? fun _ ->
  test_output ~location: __LOC__ "map_size" "111" "{ Elt \"a\" 1 }" "1" >>=? fun _ ->
  test_output ~location: __LOC__ "map_size" "111" "{ Elt \"a\" 1 ; Elt \"b\" 2 ; Elt \"c\" 3 }" "3" >>=? fun _ ->
  test_output ~location: __LOC__ "map_size" "111" "{ Elt \"a\" 1 ; Elt \"b\" 2 ; Elt \"c\" 3 ; Elt \"d\" 4 ; Elt \"e\" 5 ; Elt \"f\" 6 }" "6" >>=? fun _ ->

  (*  Contains all elements -- does the second list contain all of the same elements *)
  (*  as the first one? I'm ignoring element multiplicity *)
  test_output ~location: __LOC__ "contains_all" "None" "(Pair {} {})" "(Some True)" >>=? fun _ ->
  test_output ~location: __LOC__ "contains_all" "None" "(Pair { \"a\" } { \"B\" })" "(Some False)" >>=? fun _ ->
  test_output ~location: __LOC__ "contains_all" "None" "(Pair { \"A\" } { \"B\" })" "(Some False)" >>=? fun _ ->
  test_output ~location: __LOC__ "contains_all" "None" "(Pair { \"B\" } { \"B\" })" "(Some True)" >>=? fun _ ->
  test_output ~location: __LOC__ "contains_all" "None" "(Pair { \"B\" ; \"C\" ; \"asdf\" } { \"B\" ; \"B\" ; \"asdf\" ; \"C\" })" "(Some True)" >>=? fun _ ->
  test_output ~location: __LOC__ "contains_all" "None" "(Pair { \"B\" ; \"B\" ; \"asdf\" ; \"C\" } { \"B\" ; \"C\" ; \"asdf\" })" "(Some True)" >>=? fun _ ->

  (*  Concatenate the string in storage with all strings in the given list *)
  test_output ~location: __LOC__ "concat_hello" "{\"?\"}" "{ \"World!\" }" "{ \"Hello World!\" }" >>=? fun _ ->
  test_output ~location: __LOC__ "concat_hello" "{\"?\"}" "{}" "{}" >>=? fun _ ->
  test_output ~location: __LOC__ "concat_hello" "{\"?\"}" "{ \"test1\" ; \"test2\" }" "{ \"Hello test1\" ; \"Hello test2\" }" >>=? fun _ ->

  (*  Create an empty map and add a string to it *)
  test_output ~location: __LOC__ "empty_map" "{}" "Unit" "{ Elt \"hello\" \"world\" }" >>=? fun _ ->

  (*  Get the value stored at the given key in the map *)
  test_output ~location: __LOC__ "get_map_value" "(Pair None { Elt \"hello\" \"hi\" })" "\"hello\"" "(Pair (Some \"hi\") { Elt \"hello\" \"hi\" })" >>=? fun _ ->
  test_output ~location: __LOC__ "get_map_value" "(Pair None { Elt \"hello\" \"hi\" }" "\"\"" "(Pair None { Elt \"hello\" \"hi\" })" >>=? fun _ ->
  test_output ~location: __LOC__ "get_map_value" "(Pair None { Elt \"1\" \"one\" ; Elt \"2\" \"two\" })" "\"1\"" "(Pair (Some \"one\") { Elt \"1\" \"one\" ; Elt \"2\" \"two\" })" >>=? fun _ ->

  (*  Map iter *)
  test_output ~location: __LOC__ "map_iter" "(Pair 3 3)" "{ Elt 0 100 ; Elt 2 100 }" "(Pair 2 200)" >>=? fun _ ->
  test_output ~location: __LOC__ "map_iter" "(Pair 3 3)" "{ Elt 1 1 ; Elt 2 100 }" "(Pair 3 101)" >>=? fun _ ->

  (*  Return True if True branch of if was taken and False otherwise *)
  test_output ~location: __LOC__ "if" "None" "True" "(Some True)" >>=? fun _ ->
  test_output ~location: __LOC__ "if" "None" "False" "(Some False)" >>=? fun _ ->

  (*  Generate a pair of or types *)
  test_output ~location: __LOC__ "swap_left_right" "(Left \"\")" "(Left True)" "(Right True)" >>=? fun _ ->
  test_output ~location: __LOC__ "swap_left_right" "(Right False)" "(Right \"a\")" "(Left \"a\")" >>=? fun _ ->

  (*  Reverse a list *)
  test_output ~location: __LOC__ "reverse" "{\"?\"}" "{}" "{}" >>=? fun _ ->
  test_output ~location: __LOC__ "reverse" "{\"?\"}" "{ \"c\" ; \"b\" ; \"a\" }" "{ \"a\" ; \"b\" ; \"c\" }" >>=? fun _ ->
  test_output ~location: __LOC__ "reverse_loop" "{\"?\"}" "{}" "{}" >>=? fun _ ->
  test_output ~location: __LOC__ "reverse_loop" "{\"?\"}" "{ \"c\" ; \"b\" ; \"a\" }" "{ \"a\" ; \"b\" ; \"c\" }" >>=? fun _ ->

  (*  Reverse using LOOP_LEFT *)
  test_output ~location: __LOC__ "loop_left" "{\"?\"}" "{}" "{}" >>=? fun _ ->
  test_output ~location: __LOC__ "loop_left" "{\"?\"}" "{ \"c\" ; \"b\" ; \"a\" }" "{ \"a\" ; \"b\" ; \"c\" }" >>=? fun _ ->

  (*  Exec concat contract *)
  test_output ~location: __LOC__ "exec_concat" "\"?\"" "\"\"" "\"_abc\"" >>=? fun _ ->
  test_output ~location: __LOC__ "exec_concat" "\"?\"" "\"test\"" "\"test_abc\"" >>=? fun _ ->

  (*  Get current steps to quota *)
  test_output ~location: __LOC__ "steps_to_quota" "111" "Unit" "399992" >>=? fun _ ->

  let bootstrap_0 = List.nth Account.bootstrap_accounts 0 in
  get_balance_res bootstrap_0 sb >>=?? fun _balance ->
  let amount = Proto_alpha.Alpha_context.Tez.to_string @@ Cast.cents_of_int Script.init_amount in
  (*  Get the current balance of the contract *)
  test_output ~location: __LOC__ "balance" "\"111\"" "Unit" ("\"" ^ amount ^ "\"") >>=? fun _ ->

  (*  Test comparisons on tez { EQ ; GT ; LT ; GE ; LE } *)
  test_output ~location: __LOC__ "compare" "{}" "(Pair \"1.00\" \"2.00\")" "{ False ; False ; True ; False ; True }" >>=? fun _ ->
  test_output ~location: __LOC__ "compare" "{}" "(Pair \"2.00\" \"1.00\")" "{ False ; True ; False ; True ; False }" >>=? fun _ ->
  test_output ~location: __LOC__ "compare" "{}" "(Pair \"2.37\" \"2.37\")" "{ True ; False ; False ; True ; True }" >>=? fun _ ->

  (*  Test addition and subtraction on tez *)
  test_output ~location: __LOC__ "tez_add_sub" "None" "(Pair \"2\" \"1\")" "(Some (Pair \"3\" \"1\"))" >>=? fun _ ->
  test_output ~location: __LOC__ "tez_add_sub" "None" "(Pair \"2.31\" \"1.01\")" "(Some (Pair \"3.32\" \"1.3\"))" >>=? fun _ ->

  (*  Test get first element of list *)
  test_output ~location: __LOC__ "first" "111" "{ 1 ; 2 ; 3 ; 4 }" "1" >>=? fun _ ->
  test_output ~location: __LOC__ "first" "111" "{ 4 }" "4" >>=? fun _ ->

  (*  Hash input string *)
  (*  Test assumed to be correct -- hash is based on encoding of AST *)
  test_output ~location: __LOC__ "hash_string" "\"?\"" "\"abcdefg\"" "\"exprv3MnhXvjthGzZ7jDtXRRFremZyey9rsGtL7JRkeaQX1fThN7WF\"" >>=? fun _ ->
  test_output ~location: __LOC__ "hash_string" "\"?\"" "\"12345\"" "\"expru81QVHsW2qaWLNHnMHSxDNhqtat17ajadri6mKUvXyc2EWHZC3\"" >>=? fun _ ->

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
  test_output ~location: __LOC__ "if_some" "\"?\"" "(Some \"hello\")" "\"hello\"" >>=? fun _ ->
  test_output ~location: __LOC__ "if_some" "\"?\"" "None" "\"\"" >>=? fun _ ->

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
  test_success ~location: __LOC__ "check_signature" "(Pair \"1f19f8f37e80d96797b019f30d23ede6a26a0f698220f942103a3401f047623746e51a9c6e77e269b5df9593994ab96b001aae0f73728a2259187cb640b61e01\" \"hello\")" "\"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav\"" >>=? fun _ ->

  test_fails ~location: __LOC__ "check_signature" "(Pair \"1f19f8f37e80d96797b019f30d23ede6a26a0f698220f942103a3401f047623746e51a9c6e77e269b5df9593994ab96b001aae0f73728a2259187cb640b61e01\" \"abcd\")" "\"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav\""  >>=? fun _ ->

  (*  Convert a public key to a public key hash *)
  test_output ~location: __LOC__ "hash_key" "None" "\"edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav\"" "(Some \"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx\")" >>=? fun _ ->
  test_output ~location: __LOC__ "hash_key" "None" "\"edpkuJqtDcA2m2muMxViSM47MPsGQzmyjnNTawUPqR8vZTAMcx61ES\"" "(Some \"tz1XPTDmvT3vVE5Uunngmixm7gj7zmdbPq6k\")" >>=? fun _ ->

  (*  Test timestamp operations *)
  test_output ~location: __LOC__ "add_timestamp_delta" "None" "(Pair 100 100)" "(Some \"1970-01-01T00:03:20Z\")" >>=? fun _ ->
  test_output ~location: __LOC__ "add_timestamp_delta" "None" "(Pair 100 -100)" "(Some \"1970-01-01T00:00:00Z\")" >>=? fun _ ->
  test_output ~location: __LOC__ "add_timestamp_delta" "None" "(Pair \"1970-01-01T00:00:00Z\" 0)" "(Some \"1970-01-01T00:00:00Z\")" >>=? fun _ ->

  test_output ~location: __LOC__ "add_delta_timestamp" "None" "(Pair 100 100)" "(Some \"1970-01-01T00:03:20Z\")" >>=? fun _ ->
  test_output ~location: __LOC__ "add_delta_timestamp" "None" "(Pair -100 100)" "(Some \"1970-01-01T00:00:00Z\")" >>=? fun _ ->
  test_output ~location: __LOC__ "add_delta_timestamp" "None" "(Pair 0 \"1970-01-01T00:00:00Z\")" "(Some \"1970-01-01T00:00:00Z\")" >>=? fun _ ->

  test_output ~location: __LOC__ "sub_timestamp_delta" "111" "(Pair 100 100)" "\"1970-01-01T00:00:00Z\"" >>=? fun _ ->
  test_output ~location: __LOC__ "sub_timestamp_delta" "111" "(Pair 100 -100)" "\"1970-01-01T00:03:20Z\"" >>=? fun _ ->
  test_output ~location: __LOC__ "sub_timestamp_delta" "111" "(Pair 100 2000000000000000000)" "-1999999999999999900" >>=? fun _ ->

  test_output ~location: __LOC__ "diff_timestamps" "111" "(Pair 0 0)" "0" >>=? fun _ ->
  test_output ~location: __LOC__ "diff_timestamps" "111" "(Pair 0 1)" "-1" >>=? fun _ ->
  test_output ~location: __LOC__ "diff_timestamps" "111" "(Pair 1 0)" "1" >>=? fun _ ->
  test_output ~location: __LOC__ "diff_timestamps" "111" "(Pair \"1970-01-01T00:03:20Z\" \"1970-01-01T00:00:00Z\")" "200" >>=? fun _ ->

  (* Test NOW *)
  let now = sb.tezos_header.shell.timestamp in
  let now_str = quote @@ Tezos_base.Time.to_notation now in
  test_storage ~location: __LOC__ "store_now" "\"1970-01-01T00:03:20Z\"" "Unit" now_str >>=? fun _ ->

  (* Test TRANSFER_TO *)
  Account.make_account ~tc: sb.tezos_context >>=?? fun (account, tc) ->
  let account_str = quote @@ Signature.Public_key_hash.to_b58check account.hpub in
  test_tc ~tc "transfer_to" "Unit" account_str >>=? fun tc ->
  let amount = Account.init_amount + 100 in
  Assert.equal_cents_balance ~tc (account.contract, amount * 100) >>=?? fun _ ->

  (* Test CREATE_ACCOUNT *)
  Account.make_account ~tc: sb.tezos_context >>=?? fun (account, tc) ->
  let account_str = quote @@ Signature.Public_key_hash.to_b58check account.hpub in
  test_contract ~tc "create_account" account_str account_str >>=? fun (cs, tc) ->
  Assert.equal_int 1 @@ List.length cs ;

  (* Test CREATE_CONTRACT *)
  test_contract ~tc "create_contract" account_str account_str >>=? fun (cs, tc) ->
  Assert.equal_int 1 @@ List.length cs ;
  let contract = List.hd cs in
  Proto_alpha.Alpha_context.Contract.get_script tc contract >>=?? fun (_, res) ->
  let script = Option.unopt_exn (Failure "get_script") res in
  Script.execute_code_pred ~tc sb script (parse_param "\"abc\"") >>=?? fun (_, { storage }) ->
  Assert.equal_string ~msg: __LOC__ "\"abc\"" @@ string_of_canon storage ;

  (* Test IMPLICIT_ACCOUNT *)
  let account = Account.new_account () in
  let b_str = quote @@ Signature.Public_key_hash.to_b58check account.hpub in
  test_contract ~tc "default_account" "Unit" b_str >>=? fun (_cs, tc) ->
  Assert.equal_cents_balance ~tc (account.contract, 100 * 100) >>=?? fun _ ->
  return ()


let test_program () =
  Init.main () >>=?? fun sb ->
  let id_code = "code
     { CAR ;
       NIL operation ;
       PAIR }" in
  let id_int_program =
    program "int" "int" id_code in
  let id_ill_param_program =
    program "string" "string" id_code in
  let id_ill_return_program =
    program "int" "int" "code {}" in
  let id_pbool_program =
    program "(pair bool bool)" "(pair bool bool)" id_code in
  let push_300_code = "code
     { DROP ;
       PUSH nat 300 ;
       NIL operation ;
       PAIR }" in
  let push_300 =
    program "unit" "nat" push_300_code in
  parse_execute sb id_int_program "2" "3" >>=? fun _ ->
  parse_execute sb id_ill_param_program "2" "3" >>= fun x ->
  Assert.ill_typed_data_error ~msg: "Good data type" x ;
  parse_execute sb id_ill_return_program "2" "3" >>= fun x ->
  Assert.ill_typed_return_error ~msg: "Good return type" x ;
  parse_execute sb push_300 "Unit" "111" >>=? fun _ ->
  parse_execute sb id_pbool_program "(Pair True True)" "(Pair False False)" >>=? fun _ ->
  return ()

let tests = [
  "example", (fun _ -> test_example ()) ;
  "program", (fun _ -> test_program ()) ;
]
