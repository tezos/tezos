(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Micheline

type error += Unknown_primitive_name of string
type error += Invalid_case of string
type error += Invalid_primitive_name of Micheline.canonical_location

type prim =
  | K_parameter
  | K_return
  | K_storage
  | K_code
  | D_False
  | D_Item
  | D_Left
  | D_List
  | D_Map
  | D_None
  | D_Pair
  | D_Right
  | D_Set
  | D_Some
  | D_True
  | D_Unit
  | I_H
  | I_ABS
  | I_ADD
  | I_AMOUNT
  | I_AND
  | I_BALANCE
  | I_CAR
  | I_CDR
  | I_CHECK_SIGNATURE
  | I_COMPARE
  | I_CONCAT
  | I_CONS
  | I_CREATE_ACCOUNT
  | I_CREATE_CONTRACT
  | I_DEFAULT_ACCOUNT
  | I_DIP
  | I_DROP
  | I_DUP
  | I_EDIV
  | I_EMPTY_MAP
  | I_EMPTY_SET
  | I_EQ
  | I_EXEC
  | I_FAIL
  | I_GE
  | I_GET
  | I_GT
  | I_HASH_KEY
  | I_IF
  | I_IF_CONS
  | I_IF_LEFT
  | I_IF_NONE
  | I_INT
  | I_LAMBDA
  | I_LE
  | I_LEFT
  | I_LOOP
  | I_LSL
  | I_LSR
  | I_LT
  | I_MANAGER
  | I_MAP
  | I_MEM
  | I_MUL
  | I_NEG
  | I_NEQ
  | I_NIL
  | I_NONE
  | I_NOT
  | I_NOW
  | I_OR
  | I_PAIR
  | I_PUSH
  | I_REDUCE
  | I_RIGHT
  | I_SIZE
  | I_SOME
  | I_SOURCE
  | I_STEPS_TO_QUOTA
  | I_SUB
  | I_SWAP
  | I_TRANSFER_TOKENS
  | I_UNIT
  | I_UPDATE
  | I_XOR
  | I_ITER
  | I_LOOP_LEFT
  | T_bool
  | T_contract
  | T_int
  | T_key
  | T_key_hash
  | T_lambda
  | T_list
  | T_map
  | T_nat
  | T_option
  | T_or
  | T_pair
  | T_set
  | T_signature
  | T_string
  | T_tez
  | T_timestamp
  | T_unit

let valid_case name =
  let is_lower = function  '_' | 'a'..'z' -> true | _ -> false in
  let is_upper = function  '_' | 'A'..'Z' -> true | _ -> false in
  let rec for_all a b f =
    Compare.Int.(a > b) || f a && for_all (a + 1) b f in
  let len = String.length name in
  Compare.Int.(len <> 0)
  &&
  Compare.Char.(String.get name 0 <> '_')
  &&
  ((is_upper (String.get name 0)
    && for_all 1 (len - 1) (fun i -> is_upper (String.get name i)))
   ||
   (is_upper (String.get name 0)
    && for_all 1 (len - 1) (fun i -> is_lower (String.get name i)))
   ||
   (is_lower (String.get name 0)
    && for_all 1 (len - 1) (fun i -> is_lower (String.get name i))))

let string_of_prim = function
  | K_parameter -> "parameter"
  | K_return -> "return"
  | K_storage -> "storage"
  | K_code -> "code"
  | D_False -> "False"
  | D_Item -> "Item"
  | D_Left -> "Left"
  | D_List -> "List"
  | D_Map -> "Map"
  | D_None -> "None"
  | D_Pair -> "Pair"
  | D_Right -> "Right"
  | D_Set -> "Set"
  | D_Some -> "Some"
  | D_True -> "True"
  | D_Unit -> "Unit"
  | I_H -> "H"
  | I_ABS -> "ABS"
  | I_ADD -> "ADD"
  | I_AMOUNT -> "AMOUNT"
  | I_AND -> "AND"
  | I_BALANCE -> "BALANCE"
  | I_CAR -> "CAR"
  | I_CDR -> "CDR"
  | I_CHECK_SIGNATURE -> "CHECK_SIGNATURE"
  | I_COMPARE -> "COMPARE"
  | I_CONCAT -> "CONCAT"
  | I_CONS -> "CONS"
  | I_CREATE_ACCOUNT -> "CREATE_ACCOUNT"
  | I_CREATE_CONTRACT -> "CREATE_CONTRACT"
  | I_DEFAULT_ACCOUNT -> "DEFAULT_ACCOUNT"
  | I_DIP -> "DIP"
  | I_DROP -> "DROP"
  | I_DUP -> "DUP"
  | I_EDIV -> "EDIV"
  | I_EMPTY_MAP -> "EMPTY_MAP"
  | I_EMPTY_SET -> "EMPTY_SET"
  | I_EQ -> "EQ"
  | I_EXEC -> "EXEC"
  | I_FAIL -> "FAIL"
  | I_GE -> "GE"
  | I_GET -> "GET"
  | I_GT -> "GT"
  | I_HASH_KEY -> "HASH_KEY"
  | I_IF -> "IF"
  | I_IF_CONS -> "IF_CONS"
  | I_IF_LEFT -> "IF_LEFT"
  | I_IF_NONE -> "IF_NONE"
  | I_INT -> "INT"
  | I_LAMBDA -> "LAMBDA"
  | I_LE -> "LE"
  | I_LEFT -> "LEFT"
  | I_LOOP -> "LOOP"
  | I_LSL -> "LSL"
  | I_LSR -> "LSR"
  | I_LT -> "LT"
  | I_MANAGER -> "MANAGER"
  | I_MAP -> "MAP"
  | I_MEM -> "MEM"
  | I_MUL -> "MUL"
  | I_NEG -> "NEG"
  | I_NEQ -> "NEQ"
  | I_NIL -> "NIL"
  | I_NONE -> "NONE"
  | I_NOT -> "NOT"
  | I_NOW -> "NOW"
  | I_OR -> "OR"
  | I_PAIR -> "PAIR"
  | I_PUSH -> "PUSH"
  | I_REDUCE -> "REDUCE"
  | I_RIGHT -> "RIGHT"
  | I_SIZE -> "SIZE"
  | I_SOME -> "SOME"
  | I_SOURCE -> "SOURCE"
  | I_STEPS_TO_QUOTA -> "STEPS_TO_QUOTA"
  | I_SUB -> "SUB"
  | I_SWAP -> "SWAP"
  | I_TRANSFER_TOKENS -> "TRANSFER_TOKENS"
  | I_UNIT -> "UNIT"
  | I_UPDATE -> "UPDATE"
  | I_XOR -> "XOR"
  | I_ITER -> "ITER"
  | I_LOOP_LEFT -> "LOOP_LEFT"
  | T_bool -> "bool"
  | T_contract -> "contract"
  | T_int -> "int"
  | T_key -> "key"
  | T_key_hash -> "key_hash"
  | T_lambda -> "lambda"
  | T_list -> "list"
  | T_map -> "map"
  | T_nat -> "nat"
  | T_option -> "option"
  | T_or -> "or"
  | T_pair -> "pair"
  | T_set -> "set"
  | T_signature -> "signature"
  | T_string -> "string"
  | T_tez -> "tez"
  | T_timestamp -> "timestamp"
  | T_unit -> "unit"

let prim_of_string = function
  | "parameter" -> ok K_parameter
  | "return" -> ok K_return
  | "storage" -> ok K_storage
  | "code" -> ok K_code
  | "False" -> ok D_False
  | "Item" -> ok D_Item
  | "Left" -> ok D_Left
  | "List" -> ok D_List
  | "Map" -> ok D_Map
  | "None" -> ok D_None
  | "Pair" -> ok D_Pair
  | "Right" -> ok D_Right
  | "Set" -> ok D_Set
  | "Some" -> ok D_Some
  | "True" -> ok D_True
  | "Unit" -> ok D_Unit
  | "H" -> ok I_H
  | "ABS" -> ok I_ABS
  | "ADD" -> ok I_ADD
  | "AMOUNT" -> ok I_AMOUNT
  | "AND" -> ok I_AND
  | "BALANCE" -> ok I_BALANCE
  | "CAR" -> ok I_CAR
  | "CDR" -> ok I_CDR
  | "CHECK_SIGNATURE" -> ok I_CHECK_SIGNATURE
  | "COMPARE" -> ok I_COMPARE
  | "CONCAT" -> ok I_CONCAT
  | "CONS" -> ok I_CONS
  | "CREATE_ACCOUNT" -> ok I_CREATE_ACCOUNT
  | "CREATE_CONTRACT" -> ok I_CREATE_CONTRACT
  | "DEFAULT_ACCOUNT" -> ok I_DEFAULT_ACCOUNT
  | "DIP" -> ok I_DIP
  | "DROP" -> ok I_DROP
  | "DUP" -> ok I_DUP
  | "EDIV" -> ok I_EDIV
  | "EMPTY_MAP" -> ok I_EMPTY_MAP
  | "EMPTY_SET" -> ok I_EMPTY_SET
  | "EQ" -> ok I_EQ
  | "EXEC" -> ok I_EXEC
  | "FAIL" -> ok I_FAIL
  | "GE" -> ok I_GE
  | "GET" -> ok I_GET
  | "GT" -> ok I_GT
  | "HASH_KEY" -> ok I_HASH_KEY
  | "IF" -> ok I_IF
  | "IF_CONS" -> ok I_IF_CONS
  | "IF_LEFT" -> ok I_IF_LEFT
  | "IF_NONE" -> ok I_IF_NONE
  | "INT" -> ok I_INT
  | "LAMBDA" -> ok I_LAMBDA
  | "LE" -> ok I_LE
  | "LEFT" -> ok I_LEFT
  | "LOOP" -> ok I_LOOP
  | "LSL" -> ok I_LSL
  | "LSR" -> ok I_LSR
  | "LT" -> ok I_LT
  | "MANAGER" -> ok I_MANAGER
  | "MAP" -> ok I_MAP
  | "MEM" -> ok I_MEM
  | "MUL" -> ok I_MUL
  | "NEG" -> ok I_NEG
  | "NEQ" -> ok I_NEQ
  | "NIL" -> ok I_NIL
  | "NONE" -> ok I_NONE
  | "NOT" -> ok I_NOT
  | "NOW" -> ok I_NOW
  | "OR" -> ok I_OR
  | "PAIR" -> ok I_PAIR
  | "PUSH" -> ok I_PUSH
  | "REDUCE" -> ok I_REDUCE
  | "RIGHT" -> ok I_RIGHT
  | "SIZE" -> ok I_SIZE
  | "SOME" -> ok I_SOME
  | "SOURCE" -> ok I_SOURCE
  | "STEPS_TO_QUOTA" -> ok I_STEPS_TO_QUOTA
  | "SUB" -> ok I_SUB
  | "SWAP" -> ok I_SWAP
  | "TRANSFER_TOKENS" -> ok I_TRANSFER_TOKENS
  | "UNIT" -> ok I_UNIT
  | "UPDATE" -> ok I_UPDATE
  | "XOR" -> ok I_XOR
  | "ITER" -> ok I_ITER
  | "LOOP_LEFT" -> ok I_LOOP_LEFT
  | "bool" -> ok T_bool
  | "contract" -> ok T_contract
  | "int" -> ok T_int
  | "key" -> ok T_key
  | "key_hash" -> ok T_key_hash
  | "lambda" -> ok T_lambda
  | "list" -> ok T_list
  | "map" -> ok T_map
  | "nat" -> ok T_nat
  | "option" -> ok T_option
  | "or" -> ok T_or
  | "pair" -> ok T_pair
  | "set" -> ok T_set
  | "signature" -> ok T_signature
  | "string" -> ok T_string
  | "tez" -> ok T_tez
  | "timestamp" -> ok T_timestamp
  | "unit" -> ok T_unit
  | n ->
      if valid_case n then
        error (Unknown_primitive_name n)
      else
        error (Invalid_case n)

let prims_of_strings expr =
  let rec convert = function
    | Int _ | String _ as expr -> ok expr
    | Prim (loc, prim, args, annot) ->
        Error_monad.record_trace
          (Invalid_primitive_name loc)
          (prim_of_string prim) >>? fun prim ->
        List.fold_left
          (fun acc arg ->
             acc >>? fun args ->
             convert arg >>? fun arg ->
             ok (arg :: args))
          (ok []) args >>? fun args ->
        ok (Prim (0, prim, List.rev args, annot))
    | Seq (_, args, annot) ->
        List.fold_left
          (fun acc arg ->
             acc >>? fun args ->
             convert arg >>? fun arg ->
             ok (arg :: args))
          (ok []) args >>? fun args ->
        ok (Seq (0, List.rev args, annot)) in
  convert (root expr) >>? fun expr ->
  ok (strip_locations expr)

let strings_of_prims expr =
  let rec convert = function
    | Int _ | String _ as expr -> expr
    | Prim (_, prim, args, annot) ->
        let prim = string_of_prim prim in
        let args = List.map convert args in
        Prim (0, prim, args, annot)
    | Seq (_, args, annot) ->
        let args = List.map convert args in
        Seq (0, args, annot) in
  strip_locations (convert (root expr))

let prim_encoding =
  let open Data_encoding in
  string_enum [
    ("parameter", K_parameter) ;
    ("return", K_return) ;
    ("storage", K_storage) ;
    ("code", K_code) ;
    ("False", D_False) ;
    ("Item", D_Item) ;
    ("Left", D_Left) ;
    ("List", D_List) ;
    ("Map", D_Map) ;
    ("None", D_None) ;
    ("Pair", D_Pair) ;
    ("Right", D_Right) ;
    ("Set", D_Set) ;
    ("Some", D_Some) ;
    ("True", D_True) ;
    ("Unit", D_Unit) ;
    ("H", I_H) ;
    ("ABS", I_ABS) ;
    ("ADD", I_ADD) ;
    ("AMOUNT", I_AMOUNT) ;
    ("AND", I_AND) ;
    ("BALANCE", I_BALANCE) ;
    ("CAR", I_CAR) ;
    ("CDR", I_CDR) ;
    ("CHECK_SIGNATURE", I_CHECK_SIGNATURE) ;
    ("COMPARE", I_COMPARE) ;
    ("CONCAT", I_CONCAT) ;
    ("CONS", I_CONS) ;
    ("CREATE_ACCOUNT", I_CREATE_ACCOUNT) ;
    ("CREATE_CONTRACT", I_CREATE_CONTRACT) ;
    ("DEFAULT_ACCOUNT", I_DEFAULT_ACCOUNT) ;
    ("DIP", I_DIP) ;
    ("DROP", I_DROP) ;
    ("DUP", I_DUP) ;
    ("EDIV", I_EDIV) ;
    ("EMPTY_MAP", I_EMPTY_MAP) ;
    ("EMPTY_SET", I_EMPTY_SET) ;
    ("EQ", I_EQ) ;
    ("EXEC", I_EXEC) ;
    ("FAIL", I_FAIL) ;
    ("GE", I_GE) ;
    ("GET", I_GET) ;
    ("GT", I_GT) ;
    ("HASH_KEY", I_HASH_KEY) ;
    ("IF", I_IF) ;
    ("IF_CONS", I_IF_CONS) ;
    ("IF_LEFT", I_IF_LEFT) ;
    ("IF_NONE", I_IF_NONE) ;
    ("INT", I_INT) ;
    ("LAMBDA", I_LAMBDA) ;
    ("LE", I_LE) ;
    ("LEFT", I_LEFT) ;
    ("LOOP", I_LOOP) ;
    ("LSL", I_LSL) ;
    ("LSR", I_LSR) ;
    ("LT", I_LT) ;
    ("MANAGER", I_MANAGER) ;
    ("MAP", I_MAP) ;
    ("MEM", I_MEM) ;
    ("MUL", I_MUL) ;
    ("NEG", I_NEG) ;
    ("NEQ", I_NEQ) ;
    ("NIL", I_NIL) ;
    ("NONE", I_NONE) ;
    ("NOT", I_NOT) ;
    ("NOW", I_NOW) ;
    ("OR", I_OR) ;
    ("PAIR", I_PAIR) ;
    ("PUSH", I_PUSH) ;
    ("REDUCE", I_REDUCE) ;
    ("RIGHT", I_RIGHT) ;
    ("SIZE", I_SIZE) ;
    ("SOME", I_SOME) ;
    ("SOURCE", I_SOURCE) ;
    ("STEPS_TO_QUOTA", I_STEPS_TO_QUOTA) ;
    ("SUB", I_SUB) ;
    ("SWAP", I_SWAP) ;
    ("TRANSFER_TOKENS", I_TRANSFER_TOKENS) ;
    ("UNIT", I_UNIT) ;
    ("UPDATE", I_UPDATE) ;
    ("XOR", I_XOR) ;
    ("ITER", I_ITER) ;
    ("LOOP_LEFT", I_LOOP_LEFT) ;
    ("bool", T_bool) ;
    ("contract", T_contract) ;
    ("int", T_int) ;
    ("key", T_key) ;
    ("key_hash", T_key_hash) ;
    ("lambda", T_lambda) ;
    ("list", T_list) ;
    ("map", T_map) ;
    ("nat", T_nat) ;
    ("option", T_option) ;
    ("or", T_or) ;
    ("pair", T_pair) ;
    ("set", T_set) ;
    ("signature", T_signature) ;
    ("string", T_string) ;
    ("tez", T_tez) ;
    ("timestamp", T_timestamp) ;
    ("unit", T_unit) ]

let () =
  register_error_kind
    `Permanent
    ~id:"unknownPrimitiveNameTypeError"
    ~title: "Unknown primitive name (typechecking error)"
    ~description:
      "In a script or data expression, a primitive was unknown."
    ~pp:(fun ppf n -> Format.fprintf ppf "Unknown primitive %s.@," n)
    Data_encoding.(obj1 (req "wrongPrimitiveName" string))
    (function
      | Unknown_primitive_name got -> Some got
      | _ -> None)
    (fun got ->
       Unknown_primitive_name got) ;
  register_error_kind
    `Permanent
    ~id:"invalidPrimitiveNameCaseTypeError"
    ~title: "Invalid primitive name case (typechecking error)"
    ~description:
      "In a script or data expression, a primitive name is \
       neither uppercase, lowercase or capitalized."
    ~pp:(fun ppf n -> Format.fprintf ppf "Primitive %s has invalid case." n)
    Data_encoding.(obj1 (req "wrongPrimitiveName" string))
    (function
      | Invalid_case name -> Some name
      | _ -> None)
    (fun name ->
       Invalid_case name) ;
  register_error_kind
    `Permanent
    ~id:"invalidPrimitiveNameTypeErro"
    ~title: "Invalid primitive name (typechecking error)"
    ~description:
      "In a script or data expression, a primitive name is \
       unknown or has a wrong case."
    ~pp:(fun ppf _ -> Format.fprintf ppf "Invalid primitive.")
    Data_encoding.(obj1 (req "location" Micheline.canonical_location_encoding))
    (function
      | Invalid_primitive_name loc -> Some loc
      | _ -> None)
    (fun loc ->
       Invalid_primitive_name loc)
