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

open Micheline

type error += Unknown_primitive_name of string
type error += Invalid_case of string
type error += Invalid_primitive_name of string Micheline.canonical * Micheline.canonical_location

type prim =
  | K_parameter
  | K_storage
  | K_code
  | D_False
  | D_Elt
  | D_Left
  | D_None
  | D_Pair
  | D_Right
  | D_Some
  | D_True
  | D_Unit
  | I_PACK
  | I_UNPACK
  | I_BLAKE2B
  | I_SHA256
  | I_SHA512
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
  | I_IMPLICIT_ACCOUNT
  | I_DIP
  | I_DROP
  | I_DUP
  | I_EDIV
  | I_EMPTY_MAP
  | I_EMPTY_SET
  | I_EQ
  | I_EXEC
  | I_FAILWITH
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
  | I_RIGHT
  | I_SIZE
  | I_SOME
  | I_SOURCE
  | I_SENDER
  | I_SELF
  | I_SLICE
  | I_STEPS_TO_QUOTA
  | I_SUB
  | I_SWAP
  | I_TRANSFER_TOKENS
  | I_SET_DELEGATE
  | I_UNIT
  | I_UPDATE
  | I_XOR
  | I_ITER
  | I_LOOP_LEFT
  | I_ADDRESS
  | I_CONTRACT
  | I_ISNAT
  | I_CAST
  | I_RENAME
  | T_bool
  | T_contract
  | T_int
  | T_key
  | T_key_hash
  | T_lambda
  | T_list
  | T_map
  | T_big_map
  | T_nat
  | T_option
  | T_or
  | T_pair
  | T_set
  | T_signature
  | T_string
  | T_bytes
  | T_mutez
  | T_timestamp
  | T_unit
  | T_operation
  | T_address

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
  | K_storage -> "storage"
  | K_code -> "code"
  | D_False -> "False"
  | D_Elt -> "Elt"
  | D_Left -> "Left"
  | D_None -> "None"
  | D_Pair -> "Pair"
  | D_Right -> "Right"
  | D_Some -> "Some"
  | D_True -> "True"
  | D_Unit -> "Unit"
  | I_PACK -> "PACK"
  | I_UNPACK -> "UNPACK"
  | I_BLAKE2B -> "BLAKE2B"
  | I_SHA256 -> "SHA256"
  | I_SHA512 -> "SHA512"
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
  | I_IMPLICIT_ACCOUNT -> "IMPLICIT_ACCOUNT"
  | I_DIP -> "DIP"
  | I_DROP -> "DROP"
  | I_DUP -> "DUP"
  | I_EDIV -> "EDIV"
  | I_EMPTY_MAP -> "EMPTY_MAP"
  | I_EMPTY_SET -> "EMPTY_SET"
  | I_EQ -> "EQ"
  | I_EXEC -> "EXEC"
  | I_FAILWITH -> "FAILWITH"
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
  | I_RIGHT -> "RIGHT"
  | I_SIZE -> "SIZE"
  | I_SOME -> "SOME"
  | I_SOURCE -> "SOURCE"
  | I_SENDER -> "SENDER"
  | I_SELF -> "SELF"
  | I_SLICE -> "SLICE"
  | I_STEPS_TO_QUOTA -> "STEPS_TO_QUOTA"
  | I_SUB -> "SUB"
  | I_SWAP -> "SWAP"
  | I_TRANSFER_TOKENS -> "TRANSFER_TOKENS"
  | I_SET_DELEGATE -> "SET_DELEGATE"
  | I_UNIT -> "UNIT"
  | I_UPDATE -> "UPDATE"
  | I_XOR -> "XOR"
  | I_ITER -> "ITER"
  | I_LOOP_LEFT -> "LOOP_LEFT"
  | I_ADDRESS -> "ADDRESS"
  | I_CONTRACT -> "CONTRACT"
  | I_ISNAT -> "ISNAT"
  | I_CAST -> "CAST"
  | I_RENAME -> "RENAME"
  | T_bool -> "bool"
  | T_contract -> "contract"
  | T_int -> "int"
  | T_key -> "key"
  | T_key_hash -> "key_hash"
  | T_lambda -> "lambda"
  | T_list -> "list"
  | T_map -> "map"
  | T_big_map -> "big_map"
  | T_nat -> "nat"
  | T_option -> "option"
  | T_or -> "or"
  | T_pair -> "pair"
  | T_set -> "set"
  | T_signature -> "signature"
  | T_string -> "string"
  | T_bytes -> "bytes"
  | T_mutez -> "mutez"
  | T_timestamp -> "timestamp"
  | T_unit -> "unit"
  | T_operation -> "operation"
  | T_address -> "address"

let prim_of_string = function
  | "parameter" -> ok K_parameter
  | "storage" -> ok K_storage
  | "code" -> ok K_code
  | "False" -> ok D_False
  | "Elt" -> ok D_Elt
  | "Left" -> ok D_Left
  | "None" -> ok D_None
  | "Pair" -> ok D_Pair
  | "Right" -> ok D_Right
  | "Some" -> ok D_Some
  | "True" -> ok D_True
  | "Unit" -> ok D_Unit
  | "PACK" -> ok I_PACK
  | "UNPACK" -> ok I_UNPACK
  | "BLAKE2B" -> ok I_BLAKE2B
  | "SHA256" -> ok I_SHA256
  | "SHA512" -> ok I_SHA512
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
  | "IMPLICIT_ACCOUNT" -> ok I_IMPLICIT_ACCOUNT
  | "DIP" -> ok I_DIP
  | "DROP" -> ok I_DROP
  | "DUP" -> ok I_DUP
  | "EDIV" -> ok I_EDIV
  | "EMPTY_MAP" -> ok I_EMPTY_MAP
  | "EMPTY_SET" -> ok I_EMPTY_SET
  | "EQ" -> ok I_EQ
  | "EXEC" -> ok I_EXEC
  | "FAILWITH" -> ok I_FAILWITH
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
  | "RIGHT" -> ok I_RIGHT
  | "SIZE" -> ok I_SIZE
  | "SOME" -> ok I_SOME
  | "SOURCE" -> ok I_SOURCE
  | "SENDER" -> ok I_SENDER
  | "SELF" -> ok I_SELF
  | "SLICE" -> ok I_SLICE
  | "STEPS_TO_QUOTA" -> ok I_STEPS_TO_QUOTA
  | "SUB" -> ok I_SUB
  | "SWAP" -> ok I_SWAP
  | "TRANSFER_TOKENS" -> ok I_TRANSFER_TOKENS
  | "SET_DELEGATE" -> ok I_SET_DELEGATE
  | "UNIT" -> ok I_UNIT
  | "UPDATE" -> ok I_UPDATE
  | "XOR" -> ok I_XOR
  | "ITER" -> ok I_ITER
  | "LOOP_LEFT" -> ok I_LOOP_LEFT
  | "ADDRESS" -> ok I_ADDRESS
  | "CONTRACT" -> ok I_CONTRACT
  | "ISNAT" -> ok I_ISNAT
  | "CAST" -> ok I_CAST
  | "RENAME" -> ok I_RENAME
  | "bool" -> ok T_bool
  | "contract" -> ok T_contract
  | "int" -> ok T_int
  | "key" -> ok T_key
  | "key_hash" -> ok T_key_hash
  | "lambda" -> ok T_lambda
  | "list" -> ok T_list
  | "map" -> ok T_map
  | "big_map" -> ok T_big_map
  | "nat" -> ok T_nat
  | "option" -> ok T_option
  | "or" -> ok T_or
  | "pair" -> ok T_pair
  | "set" -> ok T_set
  | "signature" -> ok T_signature
  | "string" -> ok T_string
  | "bytes" -> ok T_bytes
  | "mutez" -> ok T_mutez
  | "timestamp" -> ok T_timestamp
  | "unit" -> ok T_unit
  | "operation" -> ok T_operation
  | "address" -> ok T_address
  | n ->
      if valid_case n then
        error (Unknown_primitive_name n)
      else
        error (Invalid_case n)

let prims_of_strings expr =
  let rec convert = function
    | Int _ | String _ | Bytes _ as expr -> ok expr
    | Prim (loc, prim, args, annot) ->
        Error_monad.record_trace
          (Invalid_primitive_name (expr, loc))
          (prim_of_string prim) >>? fun prim ->
        List.fold_left
          (fun acc arg ->
             acc >>? fun args ->
             convert arg >>? fun arg ->
             ok (arg :: args))
          (ok []) args >>? fun args ->
        ok (Prim (0, prim, List.rev args, annot))
    | Seq (_, args) ->
        List.fold_left
          (fun acc arg ->
             acc >>? fun args ->
             convert arg >>? fun arg ->
             ok (arg :: args))
          (ok []) args >>? fun args ->
        ok (Seq (0, List.rev args)) in
  convert (root expr) >>? fun expr ->
  ok (strip_locations expr)

let strings_of_prims expr =
  let rec convert = function
    | Int _ | String _ | Bytes _ as expr -> expr
    | Prim (_, prim, args, annot) ->
        let prim = string_of_prim prim in
        let args = List.map convert args in
        Prim (0, prim, args, annot)
    | Seq (_, args) ->
        let args = List.map convert args in
        Seq (0, args) in
  strip_locations (convert (root expr))

let prim_encoding =
  let open Data_encoding in
  def "michelson.v1.primitives" @@
  string_enum [
    ("parameter", K_parameter) ;
    ("storage", K_storage) ;
    ("code", K_code) ;
    ("False", D_False) ;
    ("Elt", D_Elt) ;
    ("Left", D_Left) ;
    ("None", D_None) ;
    ("Pair", D_Pair) ;
    ("Right", D_Right) ;
    ("Some", D_Some) ;
    ("True", D_True) ;
    ("Unit", D_Unit) ;
    ("PACK", I_PACK) ;
    ("UNPACK", I_UNPACK) ;
    ("BLAKE2B", I_BLAKE2B) ;
    ("SHA256", I_SHA256) ;
    ("SHA512", I_SHA512) ;
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
    ("IMPLICIT_ACCOUNT", I_IMPLICIT_ACCOUNT) ;
    ("DIP", I_DIP) ;
    ("DROP", I_DROP) ;
    ("DUP", I_DUP) ;
    ("EDIV", I_EDIV) ;
    ("EMPTY_MAP", I_EMPTY_MAP) ;
    ("EMPTY_SET", I_EMPTY_SET) ;
    ("EQ", I_EQ) ;
    ("EXEC", I_EXEC) ;
    ("FAILWITH", I_FAILWITH) ;
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
    ("RIGHT", I_RIGHT) ;
    ("SIZE", I_SIZE) ;
    ("SOME", I_SOME) ;
    ("SOURCE", I_SOURCE) ;
    ("SENDER", I_SENDER) ;
    ("SELF", I_SELF) ;
    ("STEPS_TO_QUOTA", I_STEPS_TO_QUOTA) ;
    ("SUB", I_SUB) ;
    ("SWAP", I_SWAP) ;
    ("TRANSFER_TOKENS", I_TRANSFER_TOKENS) ;
    ("SET_DELEGATE", I_SET_DELEGATE) ;
    ("UNIT", I_UNIT) ;
    ("UPDATE", I_UPDATE) ;
    ("XOR", I_XOR) ;
    ("ITER", I_ITER) ;
    ("LOOP_LEFT", I_LOOP_LEFT) ;
    ("ADDRESS", I_ADDRESS) ;
    ("CONTRACT", I_CONTRACT) ;
    ("ISNAT", I_ISNAT) ;
    ("CAST", I_CAST) ;
    ("RENAME", I_RENAME) ;
    ("bool", T_bool) ;
    ("contract", T_contract) ;
    ("int", T_int) ;
    ("key", T_key) ;
    ("key_hash", T_key_hash) ;
    ("lambda", T_lambda) ;
    ("list", T_list) ;
    ("map", T_map) ;
    ("big_map", T_big_map) ;
    ("nat", T_nat) ;
    ("option", T_option) ;
    ("or", T_or) ;
    ("pair", T_pair) ;
    ("set", T_set) ;
    ("signature", T_signature) ;
    ("string", T_string) ;
    ("bytes", T_bytes) ;
    ("mutez", T_mutez) ;
    ("timestamp", T_timestamp) ;
    ("unit", T_unit) ;
    ("operation", T_operation) ;
    ("address", T_address) ;
    (* Alpha_002 addition *)
    ("SLICE", I_SLICE) ;
  ]

let () =
  register_error_kind
    `Permanent
    ~id:"unknownPrimitiveNameTypeError"
    ~title: "Unknown primitive name (typechecking error)"
    ~description:
      "In a script or data expression, a primitive was unknown."
    ~pp:(fun ppf n -> Format.fprintf ppf "Unknown primitive %s." n)
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
    Data_encoding.(obj2
                     (req "expression" (Micheline.canonical_encoding ~variant:"generic" string))
                     (req "location" Micheline.canonical_location_encoding))
    (function
      | Invalid_primitive_name (expr, loc) -> Some (expr, loc)
      | _ -> None)
    (fun (expr, loc) ->
       Invalid_primitive_name (expr, loc))
