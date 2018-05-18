(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Alpha_context
open Micheline
open Script
open Script_typed_ir
open Script_tc_errors

type ex_comparable_ty = Ex_comparable_ty : 'a comparable_ty -> ex_comparable_ty
type ex_ty = Ex_ty : 'a ty -> ex_ty
type ex_stack_ty = Ex_stack_ty : 'a stack_ty -> ex_stack_ty

type tc_context =
  | Lambda : tc_context
  | Dip : 'a stack_ty * tc_context -> tc_context
  | Toplevel : { storage_type : 'sto ty ; param_type : 'param ty } -> tc_context

type unparsing_mode = Optimized | Readable

type type_logger =
  int -> (Script.expr * Script.annot) list -> (Script.expr * Script.annot)  list -> unit

let add_dip ty annot prev =
  match prev with
  | Lambda | Toplevel _ -> Dip (Item_t (ty, Empty_t, annot), prev)
  | Dip (stack, _) -> Dip (Item_t (ty, stack, annot), prev)

let default_arg_annot = Some (`Var_annot "arg")
let default_now_annot = Some (`Var_annot "now")
let default_amount_annot = Some (`Var_annot "amount")
let default_balance_annot = Some (`Var_annot "balance")
let default_steps_annot = Some (`Var_annot "steps")
let default_source_annot = Some (`Var_annot "source")
let default_self_annot = Some (`Var_annot "self")

let default_param_annot = Some (`Field_annot "parameter")
let default_storage_annot = Some (`Field_annot "storage")
let default_elt_annot = Some (`Field_annot "elt")
let default_key_annot = Some (`Field_annot "key")
let default_hd_annot = Some (`Field_annot "hd")
let default_contract_annot = Some (`Field_annot "contract")
let default_addr_annot = Some (`Field_annot "address")
let default_manager_annot = Some (`Field_annot "manager")

let default_annot ~default = function
  | None -> default
  | annot -> annot

let access_annot (value_annot : var_annot option) (field_annot : field_annot option) =
  match value_annot, field_annot with
  | _, None -> None
  | None, Some `Field_annot f ->
      Some (`Var_annot f)
  | Some `Var_annot v, Some `Field_annot f ->
      Some (`Var_annot (String.concat "." [v; f]))
  (* TODO maybe remove . *)

(* ---- Type size accounting ------------------------------------------------*)

(* TODO include annot in size ? *)
let comparable_type_size : type t. t comparable_ty -> int = fun ty ->
  (* No wildcard to force the update when comparable_ty chages. *)
  match ty with
  | Int_key _ -> 1
  | Nat_key _ -> 1
  | String_key _ -> 1
  | Mutez_key _ -> 1
  | Bool_key _ -> 1
  | Key_hash_key _ -> 1
  | Timestamp_key _ -> 1
  | Address_key _ -> 1

(* TODO include annot in size ? *)
let rec type_size : type t. t ty -> int =
  fun ty -> match ty with
  | Unit_t _ -> 1
  | Int_t _ -> 1
  | Nat_t _ -> 1
  | Signature_t _ -> 1
  | String_t _ -> 1
  | Mutez_t _ -> 1
  | Key_hash_t _ -> 1
  | Key_t _ -> 1
  | Timestamp_t _ -> 1
  | Address_t _ -> 1
  | Bool_t _ -> 1
  | Operation_t _ -> 1
  | Pair_t ((l, _), (r, _), _) ->
      1 + type_size l + type_size r
  | Union_t ((l, _), (r, _), _) ->
      1 + type_size l + type_size r
  | Lambda_t (arg, ret, _) ->
      1 + type_size arg + type_size ret
  | Option_t ((t,_), _, _) ->
      1 + type_size t
  | List_t (t, _) ->
      1 + type_size t
  | Set_t (k, _) ->
      1 + comparable_type_size k
  | Map_t (k, v, _) ->
      1 + comparable_type_size k + type_size v
  | Big_map_t (k, v, _) ->
      1 + comparable_type_size k + type_size v
  | Contract_t (arg, _) ->
      1 + type_size arg

let rec type_size_of_stack_head
  : type st. st stack_ty -> up_to:int -> int
  = fun stack ~up_to ->
    match stack with
    | Empty_t -> 0
    | Item_t (head, tail, _annot) ->
        if Compare.Int.(up_to > 0) then
          Compare.Int.max (type_size head)
            (type_size_of_stack_head tail ~up_to:(up_to - 1))
        else
          0

(* This is the depth of the stack to inspect for sizes overflow. We
   only need to check the produced types that can be larger than the
   arguments. That's why Swap is 0 for instance as no type grows.
   Constant sized types are not checked: it is assumed they are lower
   than the bound (otherwise every program would be rejected). *)
let number_of_generated_growing_types : type b a. (b, a) instr -> int = function
  | Drop -> 0
  | Dup -> 0
  | Swap -> 0
  | Const _ -> 1
  | Cons_pair -> 1
  | Car -> 0
  | Cdr -> 0
  | Cons_some -> 1
  | Cons_none _ -> 1
  | If_none _ -> 0
  | Left -> 0
  | Right -> 0
  | If_left _ -> 0
  | Cons_list -> 1
  | Nil -> 1
  | If_cons _ -> 0
  | List_map _ -> 1
  | List_size -> 0
  | List_iter _ -> 1
  | Empty_set _ -> 1
  | Set_iter _ -> 0
  | Set_mem -> 0
  | Set_update -> 0
  | Set_size -> 0
  | Empty_map _ -> 1
  | Map_map _ -> 1
  | Map_iter _ -> 1
  | Map_mem -> 0
  | Map_get -> 0
  | Map_update -> 0
  | Map_size -> 0
  | Big_map_get -> 0
  | Big_map_update -> 0
  | Big_map_mem -> 0
  | Concat -> 0
  | Add_seconds_to_timestamp -> 0
  | Add_timestamp_to_seconds -> 0
  | Sub_timestamp_seconds -> 0
  | Diff_timestamps -> 0
  | Add_tez -> 0
  | Sub_tez -> 0
  | Mul_teznat -> 0
  | Mul_nattez -> 0
  | Ediv_teznat -> 0
  | Ediv_tez -> 0
  | Or -> 0
  | And -> 0
  | Xor -> 0
  | Not -> 0
  | Is_nat -> 0
  | Neg_nat -> 0
  | Neg_int -> 0
  | Abs_int -> 0
  | Int_nat -> 0
  | Add_intint -> 0
  | Add_intnat -> 0
  | Add_natint -> 0
  | Add_natnat -> 0
  | Sub_int -> 0
  | Mul_intint -> 0
  | Mul_intnat -> 0
  | Mul_natint -> 0
  | Mul_natnat -> 0
  | Ediv_intint -> 0
  | Ediv_intnat -> 0
  | Ediv_natint -> 0
  | Ediv_natnat -> 0
  | Lsl_nat -> 0
  | Lsr_nat -> 0
  | Or_nat -> 0
  | And_nat -> 0
  | And_int_nat -> 0
  | Xor_nat -> 0
  | Not_nat -> 0
  | Not_int -> 0
  | Seq _ -> 0
  | If _ -> 0
  | Loop _ -> 0
  | Loop_left _ -> 0
  | Dip _ -> 0
  | Exec -> 0
  | Lambda _ -> 1
  | Fail -> 1
  | Nop -> 0
  | Compare _ -> 1
  | Eq -> 0
  | Neq -> 0
  | Lt -> 0
  | Gt -> 0
  | Le -> 0
  | Ge -> 0
  | Address -> 0
  | Contract _ -> 1
  | Manager -> 0
  | Address_manager -> 0
  | Transfer_tokens -> 1
  | Create_account -> 0
  | Implicit_account -> 0
  | Create_contract _ -> 1
  | Now -> 0
  | Balance -> 0
  | Check_signature -> 0
  | Hash_key -> 0
  | H _ -> 0
  | Steps_to_quota -> 0
  | Source -> 0
  | Self _ -> 1
  | Amount -> 0
  | Set_delegate -> 0

(* ---- Error helpers -------------------------------------------------------*)

let location = function
  | Prim (loc, _, _, _)
  | Int (loc, _)
  | String (loc, _)
  | Seq (loc, _) -> loc

let kind = function
  | Int _ -> Int_kind
  | String _ -> String_kind
  | Prim _ -> Prim_kind
  | Seq _ -> Seq_kind

let namespace = function
  | K_parameter
  | K_storage
  | K_code -> Keyword_namespace
  | D_False
  | D_Elt
  | D_Left
  | D_None
  | D_Pair
  | D_Right
  | D_Some
  | D_True
  | D_Unit -> Constant_namespace
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
  | I_IMPLICIT_ACCOUNT
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
  | I_RIGHT
  | I_SIZE
  | I_SOME
  | I_SOURCE
  | I_SELF
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
  | I_ISNAT -> Instr_namespace
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
  | T_mutez
  | T_timestamp
  | T_unit
  | T_operation
  | T_address -> Type_namespace


let unexpected expr exp_kinds exp_ns exp_prims =
  match expr with
  | Int (loc, _) -> Invalid_kind (loc, Prim_kind :: exp_kinds, Int_kind)
  | String (loc, _ ) -> Invalid_kind (loc, Prim_kind :: exp_kinds, String_kind)
  | Seq (loc, _) -> Invalid_kind (loc, Prim_kind :: exp_kinds, Seq_kind)
  | Prim (loc, name, _, _) ->
      match namespace name, exp_ns with
      | Type_namespace, Type_namespace
      | Instr_namespace, Instr_namespace
      | Constant_namespace, Constant_namespace ->
          Invalid_primitive (loc, exp_prims, name)
      | ns, _ ->
          Invalid_namespace (loc, name, exp_ns, ns)

let check_kind kinds expr =
  let kind = kind expr in
  if List.mem kind kinds then
    return ()
  else
    let loc = location expr in
    fail (Invalid_kind (loc, kinds, kind))

(* ---- Sets and Maps -------------------------------------------------------*)

let compare_comparable
  : type a. a comparable_ty -> a -> a -> int
  = fun kind x y -> match kind with
    | String_key _ -> Compare.String.compare x y
    | Bool_key _ -> Compare.Bool.compare x y
    | Mutez_key _ -> Tez.compare x y
    | Key_hash_key _ -> Signature.Public_key_hash.compare x y
    | Int_key _ ->
        let res = (Script_int.compare x y) in
        if Compare.Int.(res = 0) then 0
        else if Compare.Int.(res > 0) then 1
        else -1
    | Nat_key _ ->
        let res = (Script_int.compare x y) in
        if Compare.Int.(res = 0) then 0
        else if Compare.Int.(res > 0) then 1
        else -1
    | Timestamp_key _ -> Script_timestamp.compare x y
    | Address_key _ -> Contract.compare x y

let empty_set
  : type a. a comparable_ty -> a set
  = fun ty ->
    let module OPS = Set.Make (struct
        type t = a
        let compare = compare_comparable ty
      end) in
    (module struct
      type elt = a
      module OPS = OPS
      let boxed = OPS.empty
      let size = 0
    end)

let set_update
  : type a. a -> bool -> a set -> a set
  = fun v b (module Box) ->
    (module struct
      type elt = a
      module OPS = Box.OPS
      let boxed =
        if b
        then Box.OPS.add v Box.boxed
        else Box.OPS.remove v Box.boxed
      let size =
        let mem = Box.OPS.mem v Box.boxed in
        if mem
        then if b then Box.size else Box.size - 1
        else if b then Box.size + 1 else Box.size
    end)

let set_mem
  : type elt. elt -> elt set -> bool
  = fun v (module Box) ->
    Box.OPS.mem v Box.boxed

let set_fold
  : type elt acc. (elt -> acc -> acc) -> elt set -> acc -> acc
  = fun f (module Box) ->
    Box.OPS.fold f Box.boxed

let set_size
  : type elt. elt set -> Script_int.n Script_int.num =
  fun (module Box) ->
    Script_int.(abs (of_int Box.size))

let map_key_ty
  : type a b. (a, b) map -> a comparable_ty
  = fun (module Box) -> Box.key_ty

let empty_map
  : type a b. a comparable_ty -> (a, b) map
  = fun ty ->
    let module OPS = Map.Make (struct
        type t = a
        let compare = compare_comparable ty
      end) in
    (module struct
      type key = a
      type value = b
      let key_ty = ty
      module OPS = OPS
      let boxed = (OPS.empty, 0)
    end)

let map_get
  : type key value. key -> (key, value) map -> value option
  = fun k (module Box) ->
    Box.OPS.find_opt k (fst Box.boxed)

let map_update
  : type a b. a -> b option -> (a, b) map -> (a, b) map
  = fun k v (module Box) ->
    (module struct
      type key = a
      type value = b
      let key_ty = Box.key_ty
      module OPS = Box.OPS
      let boxed =
        let (map, size) = Box.boxed in
        let contains = Box.OPS.mem k map in
        match v with
        | Some v -> (Box.OPS.add k v map, size + if contains then 0 else 1)
        | None -> (Box.OPS.remove k map, size - if contains then 1 else 0)
    end)

let map_set
  : type a b. a -> b -> (a, b) map -> (a, b) map
  = fun k v (module Box) ->
    (module struct
      type key = a
      type value = b
      let key_ty = Box.key_ty
      module OPS = Box.OPS
      let boxed =
        let (map, size) = Box.boxed in
        (Box.OPS.add k v map, if Box.OPS.mem k map then size else size + 1)
    end)

let map_mem
  : type key value. key -> (key, value) map -> bool
  = fun k (module Box) ->
    Box.OPS.mem k (fst Box.boxed)

let map_fold
  : type key value acc. (key -> value -> acc -> acc) -> (key, value) map -> acc -> acc
  = fun f (module Box) ->
    Box.OPS.fold f (fst Box.boxed)

let map_size
  : type key value. (key, value) map -> Script_int.n Script_int.num =
  fun (module Box) ->
    Script_int.(abs (of_int (snd Box.boxed)))

(* ---- Unparsing (Typed IR -> Untyped expressions) of types -----------------*)

let ty_of_comparable_ty
  : type a. a comparable_ty -> a ty
  = function
    | Int_key tname -> Int_t tname
    | Nat_key tname -> Nat_t tname
    | String_key tname -> String_t tname
    | Mutez_key tname -> Mutez_t tname
    | Bool_key tname -> Bool_t tname
    | Key_hash_key tname -> Key_hash_t tname
    | Timestamp_key tname -> Timestamp_t tname
    | Address_key tname -> Address_t tname

let unparse_type_annot : type_annot option -> string list = function
  | None -> []
  | Some `Type_annot a -> [ ":" ^ a ]

let unparse_var_annot : var_annot option -> string list = function
  | None -> []
  | Some `Var_annot a -> [ "@" ^ a ]

let unparse_field_annot : field_annot option -> string list = function
  | None -> []
  | Some `Field_annot a -> [ "%" ^ a ]

let unparse_comparable_ty
  : type a. a comparable_ty -> Script.node
  = function
    | Int_key tname -> Prim (-1, T_int, [], unparse_type_annot tname)
    | Nat_key tname -> Prim (-1, T_nat, [], unparse_type_annot tname)
    | String_key tname -> Prim (-1, T_string, [], unparse_type_annot tname)
    | Mutez_key tname -> Prim (-1, T_mutez, [], unparse_type_annot tname)
    | Bool_key tname -> Prim (-1, T_bool, [], unparse_type_annot tname)
    | Key_hash_key tname -> Prim (-1, T_key_hash, [], unparse_type_annot tname)
    | Timestamp_key tname -> Prim (-1, T_timestamp, [], unparse_type_annot tname)
    | Address_key tname -> Prim (-1, T_address, [], unparse_type_annot tname)

let add_field_annot a = function
  | Prim (loc, prim, args, annots) ->
      Prim (loc, prim, args, annots @ (unparse_field_annot a) )
  | expr -> expr

let rec unparse_ty
  : type a. a ty -> Script.node
  = function
  | Unit_t tname -> Prim (-1, T_unit, [], unparse_type_annot tname)
  | Int_t tname -> Prim (-1, T_int, [], unparse_type_annot tname)
  | Nat_t tname -> Prim (-1, T_nat, [], unparse_type_annot tname)
  | String_t tname -> Prim (-1, T_string, [], unparse_type_annot tname)
  | Mutez_t tname -> Prim (-1, T_mutez, [], unparse_type_annot tname)
  | Bool_t tname -> Prim (-1, T_bool, [], unparse_type_annot tname)
  | Key_hash_t tname -> Prim (-1, T_key_hash, [], unparse_type_annot tname)
  | Key_t tname -> Prim (-1, T_key, [], unparse_type_annot tname)
  | Timestamp_t tname -> Prim (-1, T_timestamp, [], unparse_type_annot tname)
  | Address_t tname -> Prim (-1, T_address, [], unparse_type_annot tname)
  | Signature_t tname -> Prim (-1, T_signature, [], unparse_type_annot tname)
  | Operation_t tname -> Prim (-1, T_operation, [], unparse_type_annot tname)
  | Contract_t (ut, tname) ->
      let t = unparse_ty ut in
      Prim (-1, T_contract, [ t ], unparse_type_annot tname)
  | Pair_t ((utl, l_field), (utr, r_field ), tname) ->
      let annot = unparse_type_annot tname in
      let tl = unparse_ty utl |> add_field_annot l_field in
      let tr = unparse_ty utr |> add_field_annot r_field in
      Prim (-1, T_pair, [ tl; tr ], annot)
  | Union_t ((utl, l_field), (utr, r_field), tname) ->
      let annot = unparse_type_annot tname in
      let tl = unparse_ty utl |> add_field_annot l_field in
      let tr = unparse_ty utr |> add_field_annot r_field in
      Prim (-1, T_or, [ tl; tr ], annot)
  | Lambda_t (uta, utr, tname) ->
      let ta = unparse_ty uta in
      let tr = unparse_ty utr in
      Prim (-1, T_lambda, [ ta; tr ], unparse_type_annot tname)
  | Option_t ((ut, some_field), _none_field, tname) ->
      let annot = unparse_type_annot tname in
      let t = unparse_ty ut |> add_field_annot some_field in
      Prim (-1, T_option, [ t ], annot)
  | List_t (ut, tname) ->
      let t = unparse_ty ut in
      Prim (-1, T_list, [ t ], unparse_type_annot tname)
  | Set_t (ut, tname) ->
      let t = unparse_comparable_ty ut in
      Prim (-1, T_set, [ t ], unparse_type_annot tname)
  | Map_t (uta, utr, tname) ->
      let ta = unparse_comparable_ty uta in
      let tr = unparse_ty utr in
      Prim (-1, T_map, [ ta; tr ], unparse_type_annot tname)
  | Big_map_t (uta, utr, tname) ->
      let ta = unparse_comparable_ty uta in
      let tr = unparse_ty utr in
      Prim (-1, T_big_map, [ ta; tr ], unparse_type_annot tname)

let name_of_ty
  : type a. a ty -> type_annot option
  = function
  | Unit_t tname -> tname
  | Int_t tname -> tname
  | Nat_t tname -> tname
  | String_t tname -> tname
  | Mutez_t tname -> tname
  | Bool_t tname -> tname
  | Key_hash_t tname -> tname
  | Key_t tname -> tname
  | Timestamp_t tname -> tname
  | Address_t tname -> tname
  | Signature_t tname -> tname
  | Operation_t tname -> tname
  | Contract_t (_, tname) -> tname
  | Pair_t (_, _, tname) -> tname
  | Union_t (_, _, tname) -> tname
  | Lambda_t (_, _, tname) -> tname
  | Option_t (_, _, tname) -> tname
  | List_t (_, tname) -> tname
  | Set_t (_, tname) -> tname
  | Map_t (_, _, tname) -> tname
  | Big_map_t (_, _, tname) -> tname

(* ---- Equality witnesses --------------------------------------------------*)

type ('ta, 'tb) eq = Eq : ('same, 'same) eq

let comparable_ty_eq
  : type ta tb.
    ta comparable_ty -> tb comparable_ty ->
    (ta comparable_ty, tb comparable_ty) eq tzresult
  = fun ta tb -> match ta, tb with
    | Int_key _, Int_key _ -> Ok Eq
    | Nat_key _, Nat_key _ -> Ok Eq
    | String_key _, String_key _ -> Ok Eq
    | Mutez_key _, Mutez_key _ -> Ok Eq
    | Bool_key _, Bool_key _ -> Ok Eq
    | Key_hash_key _, Key_hash_key _ -> Ok Eq
    | Timestamp_key _, Timestamp_key _ -> Ok Eq
    | Address_key _, Address_key _ -> Ok Eq
    | _, _ -> error (Inconsistent_types (ty_of_comparable_ty ta, ty_of_comparable_ty tb))

let rec ty_eq
  : type ta tb. ta ty -> tb ty -> (ta ty, tb ty) eq tzresult
  = fun ta tb ->
    match ta, tb with
    | Unit_t _, Unit_t _ -> Ok Eq
    | Int_t _, Int_t _ -> Ok Eq
    | Nat_t _, Nat_t _ -> Ok Eq
    | Key_t _, Key_t _ -> Ok Eq
    | Key_hash_t _, Key_hash_t _ -> Ok Eq
    | String_t _, String_t _ -> Ok Eq
    | Signature_t _, Signature_t _ -> Ok Eq
    | Mutez_t _, Mutez_t _ -> Ok Eq
    | Timestamp_t _, Timestamp_t _ -> Ok Eq
    | Address_t _, Address_t _ -> Ok Eq
    | Bool_t _, Bool_t _ -> Ok Eq
    | Operation_t _, Operation_t _ -> Ok Eq
    | Map_t (tal, tar, _), Map_t (tbl, tbr, _) ->
        (comparable_ty_eq tal tbl >>? fun Eq ->
         ty_eq tar tbr >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Big_map_t (tal, tar, _), Big_map_t (tbl, tbr, _) ->
        (comparable_ty_eq tal tbl >>? fun Eq ->
         ty_eq tar tbr >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Set_t (ea, _), Set_t (eb, _) ->
        (comparable_ty_eq ea eb >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Pair_t ((tal, _), (tar, _), _),
      Pair_t ((tbl, _), (tbr, _), _) ->
        (ty_eq tal tbl >>? fun Eq ->
         ty_eq tar tbr >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Union_t ((tal, _), (tar, _), _), Union_t ((tbl, _), (tbr, _), _) ->
        (ty_eq tal tbl >>? fun Eq ->
         ty_eq tar tbr >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Lambda_t (tal, tar, _), Lambda_t (tbl, tbr, _) ->
        (ty_eq tal tbl >>? fun Eq ->
         ty_eq tar tbr >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Contract_t (tal, _), Contract_t (tbl, _) ->
        (ty_eq tal tbl >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Option_t ((tva, _), _, _), Option_t ((tvb, _), _, _) ->
        (ty_eq tva tvb >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | List_t (tva, _), List_t (tvb, _) ->
        (ty_eq tva tvb >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | _, _ -> error (Inconsistent_types (ta, tb))

let rec stack_ty_eq
  : type ta tb. int -> ta stack_ty -> tb stack_ty ->
    (ta stack_ty, tb stack_ty) eq tzresult = fun lvl ta tb ->
  match ta, tb with
  | Item_t (tva, ra, _), Item_t (tvb, rb, _) ->
      ty_eq tva tvb |>
      record_trace (Bad_stack_item lvl) >>? fun  Eq ->
      stack_ty_eq (lvl + 1) ra rb >>? fun Eq ->
      (Ok Eq : (ta stack_ty, tb stack_ty) eq tzresult)
  | Empty_t, Empty_t -> Ok Eq
  | _, _ -> error Bad_stack_length

let merge_type_annot
  : type_annot option -> type_annot option -> type_annot option tzresult
  = fun annot1 annot2 ->
  match annot1, annot2 with
  | None, None
  | Some _, None
  | None, Some _ -> ok None
  | Some `Type_annot a1, Some `Type_annot a2 ->
      if String.equal a1 a2
      then ok annot1
      else error (Inconsistent_annotations (":" ^ a1, ":" ^ a2))

let merge_field_annot
  : field_annot option -> field_annot option -> field_annot option tzresult
  = fun annot1 annot2 ->
  match annot1, annot2 with
  | None, None
  | Some _, None
  | None, Some _ -> ok None
  | Some `Field_annot a1, Some `Field_annot a2 ->
      if String.equal a1 a2
      then ok annot1
      else error (Inconsistent_annotations ("%" ^ a1, "%" ^ a2))

let merge_var_annot
  : var_annot option -> var_annot option -> var_annot option
  = fun annot1 annot2 ->
  match annot1, annot2 with
  | None, None
  | Some _, None
  | None, Some _ -> None
  | Some `Var_annot a1, Some `Var_annot a2 ->
      if String.equal a1 a2 then annot1 else None

let merge_comparable_types
  : type ta. ta comparable_ty -> ta comparable_ty -> ta comparable_ty tzresult
  = fun ta tb ->
    match ta, tb with
    | Int_key annot_a, Int_key annot_b ->
        merge_type_annot annot_a annot_b >|? fun annot ->
        Int_key annot
    | Nat_key annot_a, Nat_key annot_b ->
        merge_type_annot annot_a annot_b >|? fun annot ->
        Nat_key annot
    | String_key annot_a, String_key annot_b ->
        merge_type_annot annot_a annot_b >|? fun annot ->
        String_key annot
    | Mutez_key annot_a, Mutez_key annot_b ->
        merge_type_annot annot_a annot_b >|? fun annot ->
        Mutez_key annot
    | Bool_key annot_a, Bool_key annot_b ->
        merge_type_annot annot_a annot_b >|? fun annot ->
        Bool_key annot
    | Key_hash_key annot_a, Key_hash_key annot_b ->
        merge_type_annot annot_a annot_b >|? fun annot ->
        Key_hash_key annot
    | Timestamp_key annot_a, Timestamp_key annot_b ->
        merge_type_annot annot_a annot_b >|? fun annot ->
        Timestamp_key annot
    | Address_key annot_a, Address_key annot_b ->
        merge_type_annot annot_a annot_b >|? fun annot ->
        Address_key annot
    | _, _ -> assert false (* FIXME: fix injectivity of some types *)

let error_unexpected_annot loc annot =
  match annot with
  | [] -> ok ()
  | _ :: _ -> error (Unexpected_annotation loc)

let rec strip_annotations = function
  | (Int (_,_) as i) -> i
  | (String (_,_) as s) -> s
  | Prim (loc, prim, args, _) -> Prim (loc, prim, List.map strip_annotations args, [])
  | Seq (loc, items) -> Seq (loc, List.map strip_annotations items)

let fail_unexpected_annot loc annot =
  Lwt.return (error_unexpected_annot loc annot)

let merge_types :
  type b.Script.location -> b ty -> b ty -> b ty tzresult =
  let rec help : type a.a ty -> a ty -> a ty tzresult
    = fun ty1 ty2 ->
      match ty1, ty2 with
      | Unit_t tn1, Unit_t tn2 ->
          merge_type_annot tn1 tn2 >|? fun tname ->
          Unit_t tname
      | Int_t tn1, Int_t tn2 ->
          merge_type_annot tn1 tn2 >|? fun tname ->
          Int_t tname
      | Nat_t tn1, Nat_t tn2 ->
          merge_type_annot tn1 tn2 >|? fun tname ->
          Nat_t tname
      | Key_t tn1, Key_t tn2 ->
          merge_type_annot tn1 tn2 >|? fun tname ->
          Key_t tname
      | Key_hash_t tn1, Key_hash_t tn2 ->
          merge_type_annot tn1 tn2 >|? fun tname ->
          Key_hash_t tname
      | String_t tn1, String_t tn2 ->
          merge_type_annot tn1 tn2 >|? fun tname ->
          String_t tname
      | Signature_t tn1, Signature_t tn2 ->
          merge_type_annot tn1 tn2 >|? fun tname ->
          Signature_t tname
      | Mutez_t tn1, Mutez_t tn2 ->
          merge_type_annot tn1 tn2 >|? fun tname ->
          Mutez_t tname
      | Timestamp_t tn1, Timestamp_t tn2 ->
          merge_type_annot tn1 tn2 >|? fun tname ->
          Timestamp_t tname
      | Address_t tn1, Address_t tn2 ->
          merge_type_annot tn1 tn2 >|? fun tname ->
          Address_t tname
      | Bool_t tn1, Bool_t tn2 ->
          merge_type_annot tn1 tn2 >|? fun tname ->
          Bool_t tname
      | Operation_t tn1, Operation_t tn2 ->
          merge_type_annot tn1 tn2 >|? fun tname ->
          Operation_t tname
      | Map_t (tal, tar, tn1), Map_t (tbl, tbr, tn2) ->
          merge_type_annot tn1 tn2 >>? fun tname ->
          help tar tbr >>? fun value ->
          ty_eq tar value >>? fun Eq ->
          merge_comparable_types tal tbl >|? fun tk ->
          Map_t (tk, value, tname)
      | Big_map_t (tal, tar, tn1), Big_map_t (tbl, tbr, tn2) ->
          merge_type_annot tn1 tn2 >>? fun tname ->
          help tar tbr >>? fun value ->
          ty_eq tar value >>? fun Eq ->
          merge_comparable_types tal tbl >|? fun tk ->
          Big_map_t (tk, value, tname)
      | Set_t (ea, tn1), Set_t (eb, tn2) ->
          merge_type_annot tn1 tn2 >>? fun tname ->
          merge_comparable_types ea eb >|? fun e ->
          Set_t (e, tname)
      | Pair_t ((tal, l_field1), (tar, r_field1), tn1),
        Pair_t ((tbl, l_field2), (tbr, r_field2), tn2) ->
          merge_type_annot tn1 tn2 >>? fun tname ->
          merge_field_annot l_field1 l_field2 >>? fun l_field ->
          merge_field_annot r_field1 r_field2 >>? fun r_field ->
          help tal tbl >>? fun left_ty ->
          help tar tbr >|? fun right_ty ->
          Pair_t ((left_ty, l_field), (right_ty, r_field), tname)
      | Union_t ((tal, tal_annot), (tar, tar_annot), tn1),
        Union_t ((tbl, tbl_annot), (tbr, tbr_annot), tn2) ->
          merge_type_annot tn1 tn2 >>? fun tname ->
          merge_field_annot tal_annot tbl_annot >>? fun left_annot ->
          merge_field_annot tar_annot tbr_annot >>? fun right_annot ->
          help tal tbl >>? fun left_ty ->
          help tar tbr >|? fun right_ty ->
          Union_t ((left_ty, left_annot), (right_ty, right_annot), tname)
      | Lambda_t (tal, tar, tn1), Lambda_t (tbl, tbr, tn2) ->
          merge_type_annot tn1 tn2 >>? fun tname ->
          help tal tbl >>? fun left_ty ->
          help tar tbr >|? fun right_ty ->
          Lambda_t (left_ty, right_ty, tname)
      | Contract_t (tal, tn1), Contract_t (tbl, tn2) ->
          merge_type_annot tn1 tn2 >>? fun tname ->
          help tal tbl >|? fun arg_ty ->
          Contract_t (arg_ty, tname)
      | Option_t ((tva, some_annot_a), none_annot_a, tn1),
        Option_t ((tvb, some_annot_b), none_annot_b, tn2) ->
          merge_type_annot tn1 tn2 >>? fun tname ->
          merge_field_annot some_annot_a some_annot_b >>? fun some_annot ->
          merge_field_annot none_annot_a none_annot_b >>? fun none_annot ->
          help tva tvb >|? fun ty ->
          Option_t ((ty, some_annot), none_annot, tname)
      | List_t (tva, tn1), List_t (tvb, tn2) ->
          merge_type_annot tn1 tn2 >>? fun tname ->
          help tva tvb >|? fun ty ->
          List_t (ty, tname)
      | _, _ -> assert false
  in (fun loc ty1 ty2 ->
      record_trace
        (Inconsistent_type_annotations (loc, ty1, ty2))
        (help ty1 ty2))

let merge_stacks
  : type ta. Script.location -> ta stack_ty -> ta stack_ty -> ta stack_ty tzresult
  = fun loc ->
    let rec help : type a. a stack_ty -> a stack_ty -> a stack_ty tzresult
      = fun stack1 stack2 ->
        match stack1, stack2 with
        | Empty_t, Empty_t -> ok Empty_t
        | Item_t (ty1, rest1, annot1),
          Item_t (ty2, rest2, annot2) ->
            let annot = merge_var_annot annot1 annot2 in
            merge_types loc ty1 ty2 >>? fun ty ->
            help rest1 rest2 >|? fun rest ->
            Item_t (ty, rest, annot)
    in help

(* ---- Type checker results -------------------------------------------------*)

type 'bef judgement =
  | Typed : ('bef, 'aft) descr -> 'bef judgement
  | Failed : { descr : 'aft. 'aft stack_ty -> ('bef, 'aft) descr } -> 'bef judgement

(* ---- Type checker (Untyped expressions -> Typed IR) ----------------------*)

type ('t, 'f, 'b) branch =
  { branch : 'r. ('t, 'r) descr -> ('f, 'r) descr -> ('b, 'r) descr } [@@unboxed]


let merge_branches
  : type bef a b. int -> a judgement -> b judgement ->
    (a, b, bef) branch ->
    bef judgement tzresult Lwt.t
  = fun loc btr bfr { branch } ->
    match btr, bfr with
    | Typed ({ aft = aftbt ; _ } as dbt), Typed ({ aft = aftbf ; _ } as dbf) ->
        let unmatched_branches = (Unmatched_branches (loc, aftbt, aftbf)) in
        trace
          unmatched_branches
          (Lwt.return (stack_ty_eq 1 aftbt aftbf) >>=? fun Eq ->
           Lwt.return (merge_stacks loc aftbt aftbf) >>=? fun merged_stack ->
           return (Typed (branch {dbt with aft=merged_stack} {dbf with aft=merged_stack})))
    | Failed { descr = descrt }, Failed { descr = descrf } ->
        let descr ret =
          branch (descrt ret) (descrf ret) in
        return (Failed { descr })
    | Typed dbt, Failed { descr = descrf } ->
        return (Typed (branch dbt (descrf dbt.aft)))
    | Failed { descr = descrt }, Typed dbf ->
        return (Typed (branch (descrt dbf.aft) dbf))

module Typecheck_costs = Michelson_v1_gas.Cost_of.Typechecking

let annots_of_strings loc l =
  List.fold_left (fun acc s ->
      match acc with
      | Ok acc ->
          begin match s.[0] with
            | '@' -> ok (`Var_annot (String.sub s 1 @@ String.length s - 1) :: acc)
            | ':' -> ok (`Type_annot (String.sub s 1 @@ String.length s - 1) :: acc)
            | '%' -> ok (`Field_annot (String.sub s 1 @@ String.length s - 1) :: acc)
            | _ -> error (Unexpected_annotation loc)
            | exception Invalid_argument _ -> error (Unexpected_annotation loc)
          end
      | Error _ -> acc
    ) (ok []) l
  >|? List.rev

let parse_type_annot
  : int -> string list -> type_annot option tzresult
  = fun loc annot ->
    annots_of_strings loc annot >>? function
    | [] -> ok None
    | [ `Type_annot _ as a ] -> ok (Some a)
    | _ -> error (Unexpected_annotation loc)

let parse_composed_type_annot
  : int -> string list -> (type_annot option * field_annot option * field_annot option) tzresult
  = fun loc annot ->
    annots_of_strings loc annot >>? function
    | [] -> ok (None, None, None)
    | [ `Type_annot _ as a ] -> ok (Some a, None, None)
    | [ `Type_annot _ as a ; `Field_annot _ as b] -> ok (Some a, Some b, None)
    | [ `Type_annot _ as a ; `Field_annot _ as b; `Field_annot _ as c ] ->
        ok (Some a, Some b, Some c)
    | [ `Field_annot _ as b ] ->
        ok (None, Some b, None)
    | [ `Field_annot _ as b; `Field_annot _ as c ] ->
        ok (None, Some b, Some c)
    | _ -> error (Unexpected_annotation loc)

let check_const_type_annot
  : int -> string list -> type_annot option -> unit tzresult Lwt.t
  = fun loc annot expected_annot ->
    Lwt.return
    (parse_type_annot loc annot >>? merge_type_annot expected_annot >|? fun _ -> ())

let parse_field_annot
  : int -> string list -> field_annot option tzresult
  = fun loc annot ->
    annots_of_strings loc annot >>?
    function
    | [] -> ok None
    | [ `Field_annot _ as a ] -> ok (Some a)
    | _ -> error (Unexpected_annotation loc) (* (Invalid_var_annotation (loc, annot)) *)


let extract_field_annot
  : Script.node -> (Script.node * field_annot option) tzresult
  = function
    | Prim (loc, prim, args, annot) ->
        let field_annots, annot = List.partition (fun s ->
            match s.[0] with
            | '%' -> true
            | _ -> false
            | exception Invalid_argument _ -> false
          ) annot in
        parse_field_annot loc field_annots >|? fun field_annot ->
        Prim (loc, prim, args, annot), field_annot
    | expr -> ok (expr, None)


let rec parse_comparable_ty
  : Script.node -> ex_comparable_ty tzresult
  = function
    | Prim (loc, T_int, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( Int_key tname )
    | Prim (loc, T_nat, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( Nat_key tname )
    | Prim (loc, T_string, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( String_key tname )
    | Prim (loc, T_mutez, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( Mutez_key tname )
    | Prim (loc, T_bool, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( Bool_key tname )
    | Prim (loc, T_key_hash, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( Key_hash_key tname )
    | Prim (loc, T_timestamp, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( Timestamp_key tname )
    | Prim (loc, T_address, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( Address_key tname )
    | Prim (loc, (T_int | T_nat
                 | T_string | T_mutez | T_bool
                 | T_key | T_address | T_timestamp as prim), l, _) ->
        error (Invalid_arity (loc, prim, 0, List.length l))
    | Prim (loc, (T_pair | T_or | T_set | T_map
                 | T_list | T_option  | T_lambda
                 | T_unit | T_signature  | T_contract), _, _) as expr ->
        parse_ty ~allow_big_map:false ~allow_operation:false expr >>? fun (Ex_ty ty) ->
        error (Comparable_type_expected (loc, ty))
    | expr ->
        error @@ unexpected expr [] Type_namespace
          [ T_int ; T_nat ;
            T_string ; T_mutez ; T_bool ;
            T_key ; T_key_hash ; T_timestamp ]

and parse_ty :
  allow_big_map: bool ->
  allow_operation: bool ->
  Script.node -> ex_ty tzresult
  = fun ~allow_big_map ~allow_operation node ->
    match node with
    | Prim (loc, T_pair,
            [ Prim (big_map_loc, T_big_map, args, map_annot) ; remaining_storage ],
            storage_annot)
      when allow_big_map ->
        begin match args with
          | [ key_ty ; value_ty ] ->
              parse_comparable_ty key_ty >>? fun (Ex_comparable_ty key_ty) ->
              parse_ty ~allow_big_map:false ~allow_operation value_ty
              >>? fun (Ex_ty value_ty) ->
              parse_ty ~allow_big_map:false ~allow_operation remaining_storage
              >>? fun (Ex_ty remaining_storage) ->
              parse_type_annot big_map_loc map_annot >>? fun map_name ->
              parse_composed_type_annot loc storage_annot
              >|? fun (ty_name, map_field, storage_field) ->
              let big_map_ty = Big_map_t (key_ty, value_ty, map_name) in
              Ex_ty (Pair_t ((big_map_ty, map_field),
                             (remaining_storage, storage_field),
                             ty_name))
          | args -> error @@ Invalid_arity (big_map_loc, T_big_map, 2, List.length args)
        end
    | Prim (loc, T_unit, [], annot) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Unit_t ty_name)
    | Prim (loc, T_int, [], annot) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Int_t ty_name)
    | Prim (loc, T_nat, [], annot) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Nat_t ty_name)
    | Prim (loc, T_string, [], annot) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (String_t ty_name)
    | Prim (loc, T_mutez, [], annot) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Mutez_t ty_name)
    | Prim (loc, T_bool, [], annot) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Bool_t ty_name)
    | Prim (loc, T_key, [], annot) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Key_t ty_name)
    | Prim (loc, T_key_hash, [], annot) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Key_hash_t ty_name)
    | Prim (loc, T_timestamp, [], annot) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Timestamp_t ty_name)
    | Prim (loc, T_address, [], annot) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Address_t ty_name)
    | Prim (loc, T_signature, [], annot) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Signature_t ty_name)
    | Prim (loc, T_operation, [], annot) ->
        if allow_operation then
          parse_type_annot loc annot >|? fun ty_name ->
          Ex_ty (Operation_t ty_name)
        else
          error (Unexpected_operation loc)
    | Prim (loc, T_contract, [ utl ], annot) ->
        parse_ty ~allow_big_map:false ~allow_operation utl >>? fun (Ex_ty tl) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Contract_t (tl, ty_name))
    | Prim (loc, T_pair, [ utl; utr ], annot) ->
        extract_field_annot utl >>? fun (utl, left_field) ->
        extract_field_annot utr >>? fun (utr, right_field) ->
        parse_ty ~allow_big_map:false ~allow_operation utl >>? fun (Ex_ty tl) ->
        parse_ty ~allow_big_map:false ~allow_operation utr >>? fun (Ex_ty tr) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Pair_t ((tl, left_field), (tr, right_field), ty_name))
    | Prim (loc, T_or, [ utl; utr ], annot) ->
        extract_field_annot utl >>? fun (utl, left_constr) ->
        extract_field_annot utr >>? fun (utr, right_constr) ->
        parse_ty ~allow_big_map:false ~allow_operation utl >>? fun (Ex_ty tl) ->
        parse_ty ~allow_big_map:false ~allow_operation utr >>? fun (Ex_ty tr) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Union_t ((tl, left_constr), (tr, right_constr), ty_name))
    | Prim (loc, T_lambda, [ uta; utr ], annot) ->
        parse_ty ~allow_big_map:false ~allow_operation uta >>? fun (Ex_ty ta) ->
        parse_ty ~allow_big_map:false ~allow_operation utr >>? fun (Ex_ty tr) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Lambda_t (ta, tr, ty_name))
    | Prim (loc, T_option, [ ut ], annot) ->
        extract_field_annot ut >>? fun (ut, some_constr) ->
        parse_ty ~allow_big_map:false ~allow_operation ut >>? fun (Ex_ty t) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Option_t ((t, some_constr), None, ty_name))
    | Prim (loc, T_list, [ ut ], annot) ->
        parse_ty ~allow_big_map:false ~allow_operation ut >>? fun (Ex_ty t) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (List_t (t, ty_name))
    | Prim (loc, T_set, [ ut ], annot) ->
        parse_comparable_ty ut >>? fun (Ex_comparable_ty t) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Set_t (t, ty_name))
    | Prim (loc, T_map, [ uta; utr ], annot) ->
        parse_comparable_ty uta >>? fun (Ex_comparable_ty ta) ->
        parse_ty ~allow_big_map:false ~allow_operation utr >>? fun (Ex_ty tr) ->
        parse_type_annot loc annot >|? fun ty_name ->
        Ex_ty (Map_t (ta, tr, ty_name))
    | Prim (loc, T_big_map, _, _) ->
        error (Unexpected_big_map loc)
    | Prim (loc, (T_unit | T_signature
                 | T_int | T_nat
                 | T_string | T_mutez | T_bool
                 | T_key | T_key_hash
                 | T_timestamp | T_address as prim), l, _) ->
        error (Invalid_arity (loc, prim, 0, List.length l))
    | Prim (loc, (T_set | T_list | T_option as prim), l, _) ->
        error (Invalid_arity (loc, prim, 1, List.length l))
    | Prim (loc, (T_pair | T_or | T_map | T_lambda | T_contract as prim), l, _) ->
        error (Invalid_arity (loc, prim, 2, List.length l))
    | expr ->
        error @@ unexpected expr [] Type_namespace
          [ T_pair ; T_or ; T_set ; T_map ;
            T_list ; T_option  ; T_lambda ;
            T_unit ; T_signature  ; T_contract ;
            T_int ; T_nat ; T_operation ;
            T_string ; T_mutez ; T_bool ;
            T_key ; T_key_hash ; T_timestamp ]

let rec unparse_stack
  : type a. a stack_ty -> (Script.expr * Script.annot) list
  = function
    | Empty_t -> []
    | Item_t (ty, rest, annot) ->
        (strip_locations (unparse_ty ty), unparse_var_annot annot) :: unparse_stack rest

type ex_script = Ex_script : ('a, 'c) script -> ex_script

let parse_var_annot
  : int -> ?default:var_annot option -> string list ->
    var_annot option tzresult Lwt.t
  = fun loc ?default annot ->
    Lwt.return (annots_of_strings loc annot) >>=? fun annot ->
    begin match annot, default with
      | [], None -> ok None
      | [], Some d -> ok d
      | [ `Var_annot _ as a ], _ -> ok (Some a)
      | _ -> error (Unexpected_annotation loc) (* (Invalid_var_annotation (loc, annot)) *)
    end |> Lwt.return

let parse_field_annot loc annot =
  Lwt.return (parse_field_annot loc annot)

let classify_annot
  : annot list -> var_annot list * type_annot list * field_annot list
  = fun l ->
    let rv, rt, rf = List.fold_left (fun (rv, rt, rf) -> function
        | `Var_annot _ as a -> a :: rv, rt, rf
        | `Type_annot _ as a -> rv, a :: rt, rf
        | `Field_annot _ as a -> rv, rt, a :: rf
      ) ([], [], []) l in
    List.rev rv, List.rev rt, List.rev rf

let get_one_annot loc = function
  | [] -> Lwt.return (ok None)
  | [ a ] -> Lwt.return (ok (Some a))
  | _ -> Lwt.return (error (Unexpected_annotation loc))

let get_two_annot loc = function
  | [] -> Lwt.return (ok (None, None))
  | [ a ] -> Lwt.return (ok (Some a, None))
  | [ a; b ] -> Lwt.return (ok (Some a, Some b))
  | _ -> Lwt.return (error (Unexpected_annotation loc))

let parse_constr_annot
  : int -> string list ->
    (var_annot option * type_annot option * field_annot option * field_annot option) tzresult Lwt.t
  = fun loc annot ->
    Lwt.return (annots_of_strings loc annot) >>=? fun annot ->
    let vars, types, fields = classify_annot annot in
    get_one_annot loc vars >>=? fun v ->
    get_one_annot loc types >>=? fun t ->
    get_two_annot loc fields >>|? fun (f1, f2) ->
    (v, t, f1, f2)

let parse_two_var_annot
  : int -> string list -> (var_annot option * var_annot option) tzresult Lwt.t
  = fun loc annot ->
    Lwt.return (annots_of_strings loc annot) >>=? fun annot ->
    let vars, types, fields = classify_annot annot in
    fail_unexpected_annot loc types >>=? fun () ->
    fail_unexpected_annot loc fields >>=? fun () ->
    get_two_annot loc vars

let parse_two_field_annot
  : int -> string list -> (field_annot option * field_annot option) tzresult Lwt.t
  = fun loc annot ->
    Lwt.return (annots_of_strings loc annot) >>=? fun annot ->
    let vars, types, fields = classify_annot annot in
    fail_unexpected_annot loc vars >>=? fun () ->
    fail_unexpected_annot loc types >>=? fun () ->
    get_two_annot loc fields


let parse_var_field_annot
  : int -> string list -> (var_annot option * field_annot option) tzresult Lwt.t
  = fun loc annot ->
    Lwt.return (annots_of_strings loc annot) >>=? fun annot ->
    let vars, types, fields = classify_annot annot in
    fail_unexpected_annot loc types >>=? fun () ->
    get_one_annot loc vars >>=? fun v ->
    get_one_annot loc fields >>|? fun f ->
    (v, f)

let parse_var_type_annot
  : int -> string list -> (var_annot option * type_annot option) tzresult Lwt.t
  = fun loc annot ->
    Lwt.return (annots_of_strings loc annot) >>=? fun annot ->
    let vars, types, fields = classify_annot annot in
    fail_unexpected_annot loc fields >>=? fun () ->
    get_one_annot loc vars >>=? fun v ->
    get_one_annot loc types >>|? fun t ->
    (v, t)

(* let check_and_remove_type_annot
 *   : type a. Script.node -> a stack_ty -> Script.node tzresult Lwt.t
 *   = fun instr stack ->
 *     match instr, stack with
 *     | Prim (loc, prim, args, annot), Item_t (ty, _, _) ->
 *         let type_annots, annot = List.partition (fun s ->
 *             match s.[0] with
 *             | ':' -> true
 *             | _ -> false
 *             | exception Invalid_argument _ -> false
 *           ) annot in
 *         check_const_type_annot loc type_annots (name_of_ty ty) >>|? fun () ->
 *         Prim (loc, prim, args, annot)
 *     | _ -> Lwt.return @@ ok @@ instr *)

let field_to_var_annot : field_annot option -> var_annot option =
  function
  | None -> None
  | Some (`Field_annot s) -> Some (`Var_annot s)

let type_to_field_annot : type_annot option -> field_annot option =
  function
  | None -> None
  | Some (`Type_annot s) -> Some (`Field_annot s)

let var_to_field_annot : var_annot option -> field_annot option =
  function
  | None -> None
  | Some (`Var_annot s) -> Some (`Field_annot s)

let public_key_hash_size =
  match Data_encoding.Binary.fixed_length Signature.Public_key_hash.encoding with
  | None -> assert false
  | Some size -> size

let signature_size =
  match Data_encoding.Binary.fixed_length Signature.encoding with
  | None -> assert false
  | Some size -> size

let address_size =
  match Data_encoding.Binary.fixed_length Contract.encoding with
  | None -> assert false
  | Some size -> size

let rec parse_data
  : type a.
    ?type_logger: type_logger ->
    context -> a ty -> Script.node -> (a * context) tzresult Lwt.t
  = fun  ?type_logger ctxt ty script_data ->
    Lwt.return (Gas.consume ctxt Typecheck_costs.cycle) >>=? fun ctxt ->
    let error () =
      Invalid_constant (location script_data, strip_locations script_data, ty) in
    let traced body =
      trace (error ()) body in
    let parse_items ?type_logger loc ctxt expr key_type value_type items item_wrapper =
      fold_left_s
        (fun (last_value, map, ctxt) item ->
           Lwt.return (Gas.consume ctxt Typecheck_costs.cycle) >>=? fun ctxt ->
           match item with
           | Prim (_, D_Elt, [ k; v ], _) ->
               parse_comparable_data ?type_logger ctxt key_type k >>=? fun (k, ctxt) ->
               parse_data ?type_logger ctxt value_type v >>=? fun (v, ctxt) ->
               begin match last_value with
                 | Some value ->
                     if Compare.Int.(0 <= (compare_comparable key_type value k))
                     then
                       if Compare.Int.(0 = (compare_comparable key_type value k))
                       then fail (Duplicate_map_keys (loc, strip_locations expr))
                       else fail (Unordered_map_keys (loc, strip_locations expr))
                     else return ()
                 | None -> return ()
               end >>=? fun () ->
               return (Some k, map_update k (Some (item_wrapper v)) map, ctxt)
           | Prim (loc, D_Elt, l, _) ->
               fail @@ Invalid_arity (loc, D_Elt, 2, List.length l)
           | Prim (loc, name, _, _) ->
               fail @@ Invalid_primitive (loc, [ D_Elt ], name)
           | Int _ | String _ | Seq _ ->
               fail (error ()))
        (None, empty_map key_type, ctxt) items |> traced >>|? fun (_, items, ctxt) ->
      (items, ctxt) in
    let bytes_of_padded_z z =
      let bytes = Z.to_bits z in
      let len = MBytes.length bytes in
      if Compare.Int.(MBytes.length bytes = 0)
      || Compare.Char.(MBytes.get_char bytes (MBytes.length bytes - 1) <> '\xFF') then
        fail (error ())
      else
        return (MBytes.sub bytes 0 (len - 1)) in
    match ty, script_data with
    (* Unit *)
    | Unit_t ty_name, Prim (loc, D_Unit, [], annot) ->
        check_const_type_annot loc annot ty_name >>=? fun () ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.unit) >>|? fun ctxt ->
        ((() : a), ctxt)
    | Unit_t _, Prim (loc, D_Unit, l, _) ->
        traced (fail (Invalid_arity (loc, D_Unit, 0, List.length l)))
    | Unit_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Unit ]))
    (* Booleans *)
    | Bool_t ty_name, Prim (loc, D_True, [], annot) ->
        check_const_type_annot loc annot ty_name >>=? fun () ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.bool) >>|? fun ctxt ->
        (true, ctxt)
    | Bool_t ty_name, Prim (loc, D_False, [], annot) ->
        check_const_type_annot loc annot ty_name >>=? fun () ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.bool) >>|? fun ctxt ->
        (false, ctxt)
    | Bool_t _, Prim (loc, (D_True | D_False as c), l, _) ->
        traced (fail (Invalid_arity (loc, c, 0, List.length l)))
    | Bool_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_True ; D_False ]))
    (* Strings *)
    | String_t _, String (_, v) ->
        Lwt.return (Gas.consume ctxt (Typecheck_costs.string (String.length v))) >>=? fun ctxt ->
        let rec check_printable_ascii i =
          if Compare.Int.(i < 0) then true
          else match String.get v i with
            | '\n' | '\x20'..'\x7E' -> check_printable_ascii (i - 1)
            | _ -> false in
        if check_printable_ascii (String.length v - 1) then
          return (v, ctxt)
        else
          fail (error ())
    | String_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Integers *)
    | Int_t _, Int (_, v) ->
        return (Script_int.of_zint v, ctxt)
    | Nat_t _, Int (_, v) ->
        let v = Script_int.of_zint v in
        if Compare.Int.(Script_int.compare v Script_int.zero >= 0) then
          return (Script_int.abs v, ctxt)
        else fail (error ())
    | Int_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Int_kind ], kind expr)))
    | Nat_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Int_kind ], kind expr)))
    (* Tez amounts *)
    | Mutez_t _, Int (_, v) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.tez) >>=? fun ctxt ->
        begin try
            match Tez.of_mutez (Z.to_int64 v) with
            | None -> raise Exit
            | Some tez -> return (tez, ctxt)
          with _ ->
            fail @@ error ()
        end
    | Mutez_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Timestamps *)
    | Timestamp_t _, (Int (_, v)) (* As unparsed with [Optimized] or out of bounds [Readable]. *) ->
        return (Script_timestamp.of_zint v, ctxt)
    | Timestamp_t _, String (_, s) (* As unparsed with [Redable]. *) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.string_timestamp) >>=? fun ctxt ->
        begin try
            match Script_timestamp.of_string s with
            | Some v -> return (v, ctxt)
            | None -> fail (error ())
          with _ -> fail (error ())
        end
    | Timestamp_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ; Int_kind ], kind expr)))
    (* IDs *)
    | Key_t _, Int (_, z) -> (* As unparsed with [Optimized]. *)
        Lwt.return (Gas.consume ctxt Typecheck_costs.key) >>=? fun ctxt ->
        bytes_of_padded_z z >>=? fun bytes ->
        begin match Data_encoding.Binary.of_bytes Signature.Public_key.encoding bytes with
          | Some k -> return (k, ctxt)
          | None -> fail (error ())
        end
    | Key_t _, String (_, s) -> (* As unparsed with [Readable]. *)
        Lwt.return (Gas.consume ctxt Typecheck_costs.key) >>=? fun ctxt ->
        begin
          try
            return (Signature.Public_key.of_b58check_exn s, ctxt)
          with _ -> fail (error ())
        end
    | Key_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    | Key_hash_t _, Int (_, z) -> (* As unparsed with [Optimized]. *)
        Lwt.return (Gas.consume ctxt Typecheck_costs.key_hash) >>=? fun ctxt ->
        begin
          let bytes = Z.to_bits ~pad_to:public_key_hash_size z in
          match Data_encoding.Binary.of_bytes Signature.Public_key_hash.encoding bytes with
          | Some k -> return (k, ctxt)
          | None -> fail (error ())
        end
    | Key_hash_t _, String (_, s) (* As unparsed with [Readable]. *) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.key_hash) >>=? fun ctxt ->
        begin
          try
            return (Signature.Public_key_hash.of_b58check_exn s, ctxt)
          with _ -> fail (error ())
        end
    | Key_hash_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Signatures *)
    | Signature_t _, Int (_, z) (* As unparsed with [Optimized]. *) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.signature) >>=? fun ctxt ->
        begin
          let bytes = Z.to_bits ~pad_to:signature_size z in
          match Data_encoding.Binary.of_bytes Signature.encoding bytes with
          | Some k -> return (k, ctxt)
          | None -> fail (error ())
        end
    | Signature_t _, String (_, s) (* As unparsed with [Readable]. *) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.signature) >>=? fun ctxt ->
        begin
          try
            return (Signature.of_b58check_exn s, ctxt)
          with _ -> fail (error ())
        end
    | Signature_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Operations *)
    | Operation_t _, String (_, s) -> begin try
          Lwt.return (Gas.consume ctxt (Typecheck_costs.operation s)) >>=? fun ctxt ->
          match Data_encoding.Binary.of_bytes
                  Operation.internal_operation_encoding
                  (MBytes.of_hex (`Hex s)) with
          | Some op -> return (op, ctxt)
          | None -> raise Not_found
        with _ ->
          fail (error ())
      end
    | Operation_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Addresses *)
    | Address_t _, Int (_, z) (* As unparsed with [O[ptimized]. *) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.contract) >>=? fun ctxt ->
        begin
          let bytes = Z.to_bits ~pad_to:address_size z in
          match Data_encoding.Binary.of_bytes Contract.encoding bytes with
          | Some c -> return (c, ctxt)
          | None -> fail (error ())
        end
    | Address_t _, String (_, s) (* As unparsed with [Readable]. *) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.contract) >>=? fun ctxt ->
        traced (Lwt.return (Contract.of_b58check s)) >>=? fun c ->
        return (c, ctxt)
    | Address_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Contracts *)
    | Contract_t (ty, _), Int (loc, z) (* As unparsed with [Optimized]. *) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.contract) >>=? fun ctxt ->
        begin
          let bytes = Z.to_bits ~pad_to:address_size z in
          match Data_encoding.Binary.of_bytes Contract.encoding bytes with
          | Some c ->
              traced (parse_contract ctxt loc ty c) >>=? fun (ctxt, _) ->
              return ((ty, c), ctxt)
          | None -> fail (error ())
        end
    | Contract_t (ty, _), String (loc, s) (* As unparsed with [Readable]. *) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.contract) >>=? fun ctxt ->
        traced @@
        Lwt.return (Contract.of_b58check s) >>=? fun c ->
        parse_contract ctxt loc ty c >>=? fun (ctxt, _) ->
        return ((ty, c), ctxt)
    | Contract_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Pairs *)
    | Pair_t ((ta, _), (tb, _), ty_name), Prim (loc, D_Pair, [ va; vb ], annot) ->
        check_const_type_annot loc annot ty_name >>=? fun () ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.pair) >>=? fun ctxt ->
        traced @@
        parse_data ?type_logger ctxt ta va >>=? fun (va, ctxt) ->
        parse_data ?type_logger ctxt tb vb >>=? fun (vb, ctxt) ->
        return ((va, vb), ctxt)
    | Pair_t _, Prim (loc, D_Pair, l, _) ->
        fail @@ Invalid_arity (loc, D_Pair, 2, List.length l)
    | Pair_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Pair ]))
    (* Unions *)
    | Union_t ((tl, _), _, ty_name), Prim (loc, D_Left, [ v ], annot) ->
        check_const_type_annot loc annot ty_name >>=? fun () ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.union) >>=? fun ctxt ->
        traced @@
        parse_data ?type_logger ctxt tl v >>=? fun (v, ctxt) ->
        return (L v, ctxt)
    | Union_t _, Prim (loc, D_Left, l, _) ->
        fail @@ Invalid_arity (loc, D_Left, 1, List.length l)
    | Union_t (_, (tr, _), ty_name), Prim (loc, D_Right, [ v ], annot) ->
        check_const_type_annot loc annot ty_name >>=? fun () ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.union) >>=? fun ctxt ->
        traced @@
        parse_data ?type_logger ctxt tr v >>=? fun (v, ctxt) ->
        return (R v, ctxt)
    | Union_t _, Prim (loc, D_Right, l, _) ->
        fail @@ Invalid_arity (loc, D_Right, 1, List.length l)
    | Union_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Left ; D_Right ]))
    (* Lambdas *)
    | Lambda_t (ta, tr, _ty_name), (Seq (_loc, _) as script_instr) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.lambda) >>=? fun ctxt ->
        traced @@
        parse_returning Lambda ?type_logger ctxt (ta, Some (`Var_annot "@arg")) tr script_instr
    | Lambda_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))
    (* Options *)
    | Option_t ((t, _), _, ty_name), Prim (loc, D_Some, [ v ], annot) ->
        check_const_type_annot loc annot ty_name >>=? fun () ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.some) >>=? fun ctxt ->
        traced @@
        parse_data ?type_logger ctxt t v >>=? fun (v, ctxt) ->
        return (Some v, ctxt)
    | Option_t _, Prim (loc, D_Some, l, _) ->
        fail @@ Invalid_arity (loc, D_Some, 1, List.length l)
    | Option_t (_, _, ty_name), Prim (loc, D_None, [], annot) ->
        check_const_type_annot loc annot ty_name >>=? fun () ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.none) >>=? fun ctxt ->
        return (None, ctxt)
    | Option_t _, Prim (loc, D_None, l, _) ->
        fail @@ Invalid_arity (loc, D_None, 0, List.length l)
    | Option_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Some ; D_None ]))
    (* Lists *)
    | List_t (t, _ty_name), Seq (_loc, items) ->
        traced @@
        fold_right_s
          (fun v (rest, ctxt) ->
             Lwt.return (Gas.consume ctxt Typecheck_costs.list_element) >>=? fun ctxt ->
             parse_data ?type_logger ctxt  t v >>=? fun (v, ctxt) ->
             return ((v :: rest), ctxt))
          items ([], ctxt)
    | List_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))
    (* Sets *)
    | Set_t (t, _ty_name), (Seq (loc, vs) as expr) ->
        traced @@
        fold_left_s
          (fun (last_value, set, ctxt) v ->
             Lwt.return (Gas.consume ctxt Typecheck_costs.set_element) >>=? fun ctxt ->
             parse_comparable_data ?type_logger ctxt t v >>=? fun (v, ctxt) ->
             begin match last_value with
               | Some value ->
                   if Compare.Int.(0 <= (compare_comparable t value v))
                   then
                     if Compare.Int.(0 = (compare_comparable t value v))
                     then fail (Duplicate_set_values (loc, strip_locations expr))
                     else fail (Unordered_set_values (loc, strip_locations expr))
                   else return ()
               | None -> return ()
             end >>=? fun () ->
             Lwt.return (Gas.consume ctxt (Michelson_v1_gas.Cost_of.set_update v false set)) >>=? fun ctxt ->
             return (Some v, set_update v true set, ctxt))
          (None, empty_set t, ctxt) vs >>|? fun (_, set, ctxt) ->
        (set, ctxt)
    | Set_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))
    (* Maps *)
    | Map_t (tk, tv, _ty_name), (Seq (loc, vs) as expr) ->
        parse_items ?type_logger loc ctxt expr tk tv vs (fun x -> x)
    | Map_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))
    | Big_map_t (tk, tv, _ty_name), (Seq (loc, vs) as expr) ->
        parse_items ?type_logger loc ctxt expr tk tv vs (fun x -> Some x) >>|? fun (diff, ctxt) ->
        ({ diff ; key_type = ty_of_comparable_ty tk ; value_type = tv }, ctxt)
    | Big_map_t (_tk, _tv, _), expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))

and parse_comparable_data
  : type a.
    ?type_logger:type_logger ->
    context -> a comparable_ty -> Script.node -> (a * context) tzresult Lwt.t
  = fun ?type_logger ctxt ty script_data ->
    parse_data ?type_logger ctxt (ty_of_comparable_ty ty) script_data

and parse_returning
  : type arg ret.
    ?type_logger: type_logger ->
    tc_context -> context ->
    arg ty * var_annot option -> ret ty -> Script.node ->
    ((arg, ret) lambda * context) tzresult Lwt.t =
  fun ?type_logger tc_context ctxt (arg, arg_annot) ret script_instr ->
    parse_instr ?type_logger tc_context ctxt
      script_instr (Item_t (arg, Empty_t, arg_annot)) >>=? function
    | (Typed ({ loc ; aft = (Item_t (ty, Empty_t, _) as stack_ty) ; _ } as descr), gas) ->
        trace
          (Bad_return (loc, stack_ty, ret))
          (Lwt.return (ty_eq ty ret)) >>=? fun Eq ->
        return ((Lam (descr, strip_locations script_instr) : (arg, ret) lambda), gas)
    | (Typed { loc ; aft = stack_ty ; _ }, _gas) ->
        fail (Bad_return (loc, stack_ty, ret))
    | (Failed { descr }, gas) ->
        return ((Lam (descr (Item_t (ret, Empty_t, None)), strip_locations script_instr)
                 : (arg, ret) lambda), gas)

and parse_instr
  : type bef.
    ?type_logger: type_logger ->
    tc_context -> context ->
    Script.node -> bef stack_ty -> (bef judgement * context) tzresult Lwt.t =
  fun ?type_logger tc_context ctxt script_instr stack_ty ->
    let return :
      context -> bef judgement -> (bef judgement * context) tzresult Lwt.t = fun ctxt judgement ->
      match judgement with
      | Typed { instr ; loc ; aft ; _ } ->
          let maximum_type_size = Constants.michelson_maximum_type_size ctxt in
          let type_size =
            type_size_of_stack_head aft
              ~up_to:(number_of_generated_growing_types instr) in
          if Compare.Int.(type_size > maximum_type_size) then
            fail (Type_too_large (loc, type_size, maximum_type_size))
          else
            return (judgement, ctxt)
      | Failed _ ->
          return (judgement, ctxt) in
    let check_item check loc name n m =
      trace (Bad_stack (loc, name, m, stack_ty)) @@
      trace (Bad_stack_item n) @@
      Lwt.return check in
    let check_item_ty exp got loc n =
      check_item (ty_eq exp got) loc n in
    let typed ctxt loc instr aft =
      begin match type_logger, script_instr with
        | None, _
        | Some _, (Seq (-1, _) | Int _ | String _) -> ()
        | Some log, (Prim _ | Seq _) ->
            log loc (unparse_stack stack_ty) (unparse_stack aft)
      end ;
      return ctxt (Typed { loc ; instr ; bef = stack_ty ; aft }) in
    match script_instr, stack_ty with
    (* stack ops *)
    | Prim (loc, I_DROP, [], annot),
      Item_t (_, rest, _) ->
        fail_unexpected_annot loc annot >>=? fun () ->
        typed ctxt loc Drop
          rest
    | Prim (loc, I_DUP, [], annot),
      Item_t (v, rest, stack_annot) ->
        parse_var_annot loc annot ~default:stack_annot >>=? fun annot ->
        typed ctxt loc Dup
          (Item_t (v, Item_t (v, rest, stack_annot), annot))
    | Prim (loc, I_SWAP, [], annot),
      Item_t (v,  Item_t (w, rest, stack_annot), cur_top_annot) ->
        fail_unexpected_annot loc annot >>=? fun () ->
        typed ctxt loc Swap
          (Item_t (w, Item_t (v, rest, cur_top_annot), stack_annot))
    | Prim (loc, I_PUSH, [ t ; d ], annot),
      stack ->
        parse_var_annot loc annot >>=? fun annot ->
        (Lwt.return (parse_ty ~allow_big_map:false ~allow_operation:false t)) >>=? fun (Ex_ty t) ->
        parse_data ?type_logger ctxt t d >>=? fun (v, ctxt) ->
        typed ctxt loc (Const v) (Item_t (t, stack, annot))
    | Prim (loc, I_UNIT, [], annot),
      stack ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc (Const ()) (Item_t (Unit_t None, stack, annot))
    (* options *)
    | Prim (loc, I_SOME, [], annot),
      Item_t (t, rest, _) ->
        parse_constr_annot loc annot >>=? fun (annot, ty_name, some_field, none_field) ->
        typed ctxt loc Cons_some
          (Item_t (Option_t ((t, some_field), none_field, ty_name), rest, annot))
    | Prim (loc, I_NONE, [ t ], annot),
      stack ->
        (Lwt.return (parse_ty ~allow_big_map:false ~allow_operation:true t)) >>=? fun (Ex_ty t) ->
        parse_constr_annot loc annot >>=? fun (annot, ty_name, some_field, none_field) ->
        typed ctxt loc (Cons_none t)
          (Item_t (Option_t ((t, some_field), none_field, ty_name), stack, annot))
    | Prim (loc, I_IF_NONE, [ bt ; bf ], annot),
      (Item_t (Option_t ((t, some_field), _none_field, _), rest, option_annot) as bef) ->
        check_kind [ Seq_kind ] bt >>=? fun () ->
        check_kind [ Seq_kind ] bf >>=? fun () ->
        parse_var_annot loc annot ~default:(access_annot option_annot some_field)
        >>=? fun some_annot ->
        parse_instr ?type_logger tc_context ctxt bt rest >>=? fun (btr, ctxt) ->
        parse_instr ?type_logger tc_context ctxt bf (Item_t (t, rest, some_annot)) >>=? fun (bfr, ctxt) ->
        let branch ibt ibf =
          { loc ; instr = If_none (ibt, ibf) ; bef ; aft = ibt.aft } in
        merge_branches loc btr bfr { branch } >>=? fun judgement ->
        return ctxt judgement
    (* pairs *)
    | Prim (loc, I_PAIR, [], annot),
      Item_t (a, Item_t (b, rest, snd_annot), fst_annot) ->
        parse_constr_annot loc annot >>=? fun (annot, ty_name, l_field, r_field) ->
        let l_field = default_annot l_field ~default:(var_to_field_annot fst_annot) in
        let r_field = default_annot r_field ~default:(var_to_field_annot snd_annot) in
        typed ctxt loc Cons_pair
          (Item_t (Pair_t((a, l_field), (b, r_field), ty_name), rest, annot))
    | Prim (loc, I_CAR, [], annot),
      Item_t (Pair_t ((a, field_annot), _, _), rest, pair_annot) ->
        parse_var_annot loc annot ~default:(access_annot pair_annot field_annot)
        >>=? fun annot ->
        typed ctxt loc Car (Item_t (a, rest, annot))
    | Prim (loc, I_CDR, [], annot),
      Item_t (Pair_t (_, (b, field_annot), _), rest, pair_annot) ->
        parse_var_annot loc annot ~default:(access_annot pair_annot field_annot)
        >>=? fun annot ->
        typed ctxt loc Cdr (Item_t (b, rest, annot))
    (* unions *)
    | Prim (loc, I_LEFT, [ tr ], annot),
      Item_t (tl, rest, _stack_annot) ->
        (Lwt.return (parse_ty ~allow_big_map:false ~allow_operation:true tr)) >>=? fun (Ex_ty tr) ->
        parse_constr_annot loc annot >>=? fun (annot, tname, l_field, r_field) ->
        typed ctxt loc Left (Item_t (Union_t ((tl, l_field), (tr, r_field), tname), rest, annot))
    | Prim (loc, I_RIGHT, [ tl ], annot),
      Item_t (tr, rest, _stack_annot) ->
        (Lwt.return (parse_ty ~allow_big_map:false ~allow_operation:true tl)) >>=? fun (Ex_ty tl) ->
        parse_constr_annot loc annot >>=? fun (annot, tname, l_field, r_field) ->
        typed ctxt loc Right (Item_t (Union_t ((tl, l_field), (tr, r_field), tname), rest, annot))
    | Prim (loc, I_IF_LEFT, [ bt ; bf ], annot),
      (Item_t (Union_t ((tl, l_field), (tr, r_field), _), rest, union_annot) as bef) ->
        check_kind [ Seq_kind ] bt >>=? fun () ->
        check_kind [ Seq_kind ] bf >>=? fun () ->
        parse_two_var_annot loc annot >>=? fun (left_annot, right_annot) ->
        let left_annot =
          default_annot left_annot ~default:(access_annot union_annot l_field) in
        let right_annot =
          default_annot right_annot ~default:(access_annot union_annot r_field) in
        parse_instr ?type_logger tc_context ctxt bt (Item_t (tl, rest, left_annot)) >>=? fun (btr, ctxt) ->
        parse_instr ?type_logger tc_context ctxt bf (Item_t (tr, rest, right_annot)) >>=? fun (bfr, ctxt) ->
        let branch ibt ibf =
          { loc ; instr = If_left (ibt, ibf) ; bef ; aft = ibt.aft } in
        merge_branches loc btr bfr { branch } >>=? fun judgement ->
        return ctxt judgement
    (* lists *)
    | Prim (loc, I_NIL, [ t ], annot),
      stack ->
        (Lwt.return (parse_ty ~allow_big_map:false ~allow_operation:true t)) >>=? fun (Ex_ty t) ->
        parse_var_type_annot loc annot >>=? fun (annot, ty_name) ->
        typed ctxt loc Nil (Item_t (List_t (t, ty_name), stack, annot))
    | Prim (loc, I_CONS, [], annot),
      Item_t (tv, Item_t (List_t (t, ty_name), rest, _), _) ->
        check_item_ty tv t loc I_CONS 1 2 >>=? fun Eq ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Cons_list (Item_t (List_t (t, ty_name), rest, annot))
    | Prim (loc, I_IF_CONS, [ bt ; bf ], annot),
      (Item_t (List_t (t, _), rest, list_annot) as bef) ->
        check_kind [ Seq_kind ] bt >>=? fun () ->
        check_kind [ Seq_kind ] bf >>=? fun () ->
        parse_var_annot loc annot ~default:(access_annot list_annot default_hd_annot)
        >>=? fun hd_annot ->
        parse_instr ?type_logger tc_context ctxt bt
          (Item_t (t, bef, hd_annot)) >>=? fun (btr, ctxt) ->
        parse_instr ?type_logger tc_context ctxt bf
          rest >>=? fun (bfr, ctxt) ->
        let branch ibt ibf =
          { loc ; instr = If_cons (ibt, ibf) ; bef ; aft = ibt.aft } in
        merge_branches loc btr bfr { branch } >>=? fun judgement ->
        return ctxt judgement
    | Prim (loc, I_SIZE, [], annot),
      Item_t (List_t _, rest, _) ->
        parse_var_type_annot loc annot >>=? fun (annot, tname) ->
        typed ctxt loc List_size (Item_t (Nat_t tname, rest, annot))
    | Prim (loc, I_MAP, [ body ], annot),
      (Item_t (List_t (elt, _), starting_rest, list_annot)) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        parse_var_field_annot loc annot >>=? fun (ret_annot, elt_annot) ->
        let elt_annot = default_annot (field_to_var_annot elt_annot)
            ~default:(access_annot list_annot default_elt_annot) in
        parse_instr ?type_logger tc_context ctxt
          body (Item_t (elt, starting_rest, elt_annot)) >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Typed ({ aft = Item_t (ret, rest, _) ; _ } as ibody) ->
              trace
                (Invalid_map_body (loc, ibody.aft))
                (Lwt.return (stack_ty_eq 1 rest starting_rest)) >>=? fun Eq ->
              typed ctxt loc (List_map ibody)
                (Item_t (List_t (ret, None), rest, ret_annot))
          | Typed { aft ; _ } -> fail (Invalid_map_body (loc, aft))
          | Failed _ -> fail (Invalid_map_block_fail loc)
        end
    | Prim (loc, I_ITER, [ body ], annot),
      Item_t (List_t (elt, _), rest, list_annot) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        parse_field_annot loc annot >>=? fun elt_annot ->
        let elt_annot = default_annot (field_to_var_annot elt_annot)
            ~default:(access_annot list_annot default_elt_annot) in
        parse_instr ?type_logger tc_context ctxt
          body (Item_t (elt, rest, elt_annot)) >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Typed ({ aft ; _ } as ibody) ->
              trace
                (Invalid_iter_body (loc, rest, ibody.aft))
                (Lwt.return (stack_ty_eq 1 aft rest)) >>=? fun Eq ->
              typed ctxt loc (List_iter ibody) rest
          | Failed { descr } ->
              typed ctxt loc (List_iter (descr rest)) rest
        end
    (* sets *)
    | Prim (loc, I_EMPTY_SET, [ t ], annot),
      rest ->
        (Lwt.return (parse_comparable_ty t)) >>=? fun (Ex_comparable_ty t) ->
        parse_var_type_annot loc annot >>=? fun (annot, tname) ->
        typed ctxt loc (Empty_set t) (Item_t (Set_t (t, tname), rest, annot))
    | Prim (loc, I_ITER, [ body ], annot),
      Item_t (Set_t (comp_elt, _), rest, set_annot) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        parse_field_annot loc annot >>=? fun elt_annot ->
        let elt_annot = default_annot (field_to_var_annot elt_annot)
            ~default:(access_annot set_annot default_elt_annot) in
        let elt = ty_of_comparable_ty comp_elt in
        parse_instr ?type_logger tc_context ctxt
          body (Item_t (elt, rest, elt_annot)) >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Typed ({ aft ; _ } as ibody) ->
              trace
                (Invalid_iter_body (loc, rest, ibody.aft))
                (Lwt.return (stack_ty_eq 1 aft rest)) >>=? fun Eq ->
              typed ctxt loc (Set_iter ibody) rest
          | Failed { descr } ->
              typed ctxt loc (Set_iter (descr rest)) rest
        end
    | Prim (loc, I_MEM, [], annot),
      Item_t (v, Item_t (Set_t (elt, _), rest, _), _) ->
        let elt = ty_of_comparable_ty elt in
        parse_var_type_annot loc annot >>=? fun (annot, tname) ->
        check_item_ty elt v loc I_MEM 1 2 >>=? fun Eq ->
        typed ctxt loc Set_mem (Item_t (Bool_t tname, rest, annot))
    | Prim (loc, I_UPDATE, [], annot),
      Item_t (v, Item_t (Bool_t _, Item_t (Set_t (elt, tname), rest, set_annot), _), _) ->
        let ty = ty_of_comparable_ty elt in
        parse_var_annot loc annot ~default:set_annot >>=? fun annot ->
        check_item_ty ty v loc I_UPDATE 1 3 >>=? fun Eq ->
        typed ctxt loc Set_update (Item_t (Set_t (elt, tname), rest, annot))
    | Prim (loc, I_SIZE, [], annot),
      Item_t (Set_t _, rest, _) ->
        parse_var_type_annot loc annot >>=? fun (annot, tname) ->
        typed ctxt loc Set_size (Item_t (Nat_t tname, rest, annot))
    (* maps *)
    | Prim (loc, I_EMPTY_MAP, [ tk ; tv ], annot),
      stack ->
        (Lwt.return (parse_comparable_ty tk)) >>=? fun (Ex_comparable_ty tk) ->
        (Lwt.return (parse_ty ~allow_big_map:false ~allow_operation:true tv)) >>=? fun (Ex_ty tv) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc (Empty_map (tk, tv)) (Item_t (Map_t (tk, tv, None), stack, annot))
    | Prim (loc, I_MAP, [ body ], annot),
      Item_t (Map_t (ck, elt, _), starting_rest, _map_annot) ->
        let k = ty_of_comparable_ty ck in
        check_kind [ Seq_kind ] body >>=? fun () ->
        parse_constr_annot loc annot >>=? fun (ret_annot, ty_name, key_annot, elt_annot) ->
        let key_field = default_annot key_annot ~default:default_key_annot in
        let elt_field = default_annot elt_annot ~default:default_elt_annot in
        parse_instr ?type_logger tc_context ctxt
          body (Item_t (Pair_t ((k, key_field), (elt, elt_field), None), starting_rest, None)) >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Typed ({ aft = Item_t (ret, rest, _) ; _ } as ibody) ->
              trace
                (Invalid_map_body (loc, ibody.aft))
                (Lwt.return (stack_ty_eq 1 rest starting_rest)) >>=? fun Eq ->
              typed ctxt loc (Map_map ibody)
                (Item_t (Map_t (ck, ret, ty_name), rest, ret_annot))
          | Typed { aft ; _ } -> fail (Invalid_map_body (loc, aft))
          | Failed _ -> fail (Invalid_map_block_fail loc)
        end
    | Prim (loc, I_ITER, [ body ], annot),
      Item_t (Map_t (comp_elt, element_ty, _), rest, _) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        parse_two_field_annot loc annot >>=? fun (key_annot, elt_annot) ->
        let key_field = default_annot key_annot ~default:default_key_annot in
        let elt_field = default_annot elt_annot ~default:default_elt_annot in
        let key = ty_of_comparable_ty comp_elt in
        parse_instr ?type_logger tc_context ctxt body
          (Item_t (Pair_t ((key, key_field), (element_ty, elt_field), None), rest, None))
        >>=? begin fun (judgement, ctxt) -> match judgement with
          | Typed ({ aft ; _ } as ibody) ->
              trace
                (Invalid_iter_body (loc, rest, ibody.aft))
                (Lwt.return (stack_ty_eq 1 aft rest)) >>=? fun Eq ->
              typed ctxt loc (Map_iter ibody) rest
          | Failed { descr } ->
              typed ctxt loc (Map_iter (descr rest)) rest
        end
    | Prim (loc, I_MEM, [], annot),
      Item_t (vk, Item_t (Map_t (ck, _, _), rest, _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty vk k loc I_MEM 1 2 >>=? fun Eq ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Map_mem (Item_t (Bool_t None, rest, annot))
    | Prim (loc, I_GET, [], annot),
      Item_t (vk, Item_t (Map_t (ck, elt, _), rest, _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty vk k loc I_GET 1 2 >>=? fun Eq ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Map_get (Item_t (Option_t ((elt, None), None, None), rest, annot))
    | Prim (loc, I_UPDATE, [], annot),
      Item_t (vk, Item_t (Option_t ((vv, _), _, _),
                          Item_t (Map_t (ck, v, map_name), rest, map_annot), _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty vk k loc I_UPDATE 1 3 >>=? fun Eq ->
        check_item_ty vv v loc I_UPDATE 2 3 >>=? fun Eq ->
        parse_var_annot loc annot ~default:map_annot >>=? fun annot ->
        typed ctxt loc Map_update (Item_t (Map_t (ck, v, map_name), rest, annot))
    | Prim (loc, I_SIZE, [], annot),
      Item_t (Map_t (_, _, _), rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Map_size (Item_t (Nat_t None, rest, annot))
    (* big_map *)
    | Prim (loc, I_MEM, [], annot),
      Item_t (set_key, Item_t (Big_map_t (map_key, _, _), rest, _), _) ->
        let k = ty_of_comparable_ty map_key in
        check_item_ty set_key k loc I_MEM 1 2 >>=? fun Eq ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Big_map_mem (Item_t (Bool_t None, rest, annot))
    | Prim (loc, I_GET, [], annot),
      Item_t (vk, Item_t (Big_map_t (ck, elt, _), rest, _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty vk k loc I_GET 1 2 >>=? fun Eq ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Big_map_get (Item_t (Option_t ((elt, None), None, None), rest, annot))
    | Prim (loc, I_UPDATE, [], annot),
      Item_t (set_key,
              Item_t (Option_t ((set_value, _), _, _),
                      Item_t (Big_map_t (map_key, map_value, map_name), rest, map_annot), _), _) ->
        let k = ty_of_comparable_ty map_key in
        check_item_ty set_key k loc I_UPDATE 1 3 >>=? fun Eq ->
        check_item_ty set_value map_value loc I_UPDATE 2 3 >>=? fun Eq ->
        parse_var_annot loc annot ~default:map_annot >>=? fun annot ->
        typed ctxt loc Big_map_update (Item_t (Big_map_t (map_key, map_value, map_name), rest, annot))
    (* control *)
    | Seq (loc, []),
      stack ->
        typed ctxt loc Nop stack
    | Seq (loc, [ single ]),
      stack ->
        parse_instr ?type_logger tc_context ctxt single
          stack >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Typed ({ aft ; _ } as instr) ->
              let nop = { bef = aft ; loc = loc ; aft ; instr = Nop } in
              typed ctxt loc (Seq (instr, nop)) aft
          | Failed { descr ; _ } ->
              let descr aft =
                let nop = { bef = aft ; loc = loc ; aft ; instr = Nop } in
                let descr = descr aft in
                { descr with instr = Seq (descr, nop) } in
              return ctxt (Failed { descr })
        end
    | Seq (loc, hd :: tl),
      stack ->
        parse_instr ?type_logger tc_context ctxt hd
          stack >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Failed _ ->
              fail (Fail_not_in_tail_position (Micheline.location hd))
          | Typed ({ aft = middle ; _ } as ihd) ->
              parse_instr ?type_logger tc_context ctxt (Seq (-1, tl))
                middle >>=? fun (judgement, ctxt) ->
              match judgement with
              | Failed { descr } ->
                  let descr ret =
                    { loc ; instr = Seq (ihd, descr ret) ;
                      bef = stack ; aft = ret } in
                  return ctxt (Failed { descr })
              | Typed itl ->
                  typed ctxt loc (Seq (ihd, itl)) itl.aft
        end
    | Prim (loc, I_IF, [ bt ; bf ], annot),
      (Item_t (Bool_t _, rest, _) as bef) ->
        check_kind [ Seq_kind ] bt >>=? fun () ->
        check_kind [ Seq_kind ] bf >>=? fun () ->
        fail_unexpected_annot loc annot >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt bt rest >>=? fun (btr, ctxt) ->
        parse_instr ?type_logger tc_context ctxt bf rest >>=? fun (bfr, ctxt) ->
        let branch ibt ibf =
          { loc ; instr = If (ibt, ibf) ; bef ; aft = ibt.aft } in
        merge_branches loc btr bfr { branch } >>=? fun judgement ->
        return ctxt judgement
    | Prim (loc, I_LOOP, [ body ], annot),
      (Item_t (Bool_t _, rest, _stack_annot) as stack) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        fail_unexpected_annot loc annot >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt body
          rest >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Typed ibody ->
              trace
                (Unmatched_branches (loc, ibody.aft, stack))
                (Lwt.return (stack_ty_eq 1 ibody.aft stack)) >>=? fun Eq ->
              typed ctxt loc (Loop ibody) rest
          | Failed { descr } ->
              let ibody = descr stack in
              typed ctxt loc (Loop ibody) rest
        end
    | Prim (loc, I_LOOP_LEFT, [ body ], annot),
      (Item_t (Union_t ((tl, l_field), (tr, _), _), rest, union_annot) as stack) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        fail_unexpected_annot loc annot >>=? fun () ->
        parse_var_field_annot loc annot >>=? fun (r_annot, l_annot) ->
        let l_annot = default_annot (field_to_var_annot l_annot)
            ~default:(access_annot union_annot l_field) in
        parse_instr ?type_logger tc_context ctxt body
          (Item_t (tl, rest, l_annot)) >>=? begin fun (judgement, ctxt) -> match judgement with
          | Typed ibody ->
              trace
                (Unmatched_branches (loc, ibody.aft, stack))
                (Lwt.return (stack_ty_eq 1 ibody.aft stack)) >>=? fun Eq ->
              typed ctxt loc (Loop_left ibody) (Item_t (tr, rest, r_annot))
          | Failed { descr } ->
              let ibody = descr stack in
              typed ctxt loc (Loop_left ibody) (Item_t (tr, rest, r_annot))
        end
    | Prim (loc, I_LAMBDA, [ arg ; ret ; code ], annot),
      stack ->
        (Lwt.return (parse_ty ~allow_big_map:false ~allow_operation:true arg))
        >>=? fun (Ex_ty arg) ->
        (Lwt.return (parse_ty ~allow_big_map:false ~allow_operation:true ret))
        >>=? fun (Ex_ty ret) ->
        check_kind [ Seq_kind ] code >>=? fun () ->
        parse_var_field_annot loc annot >>=? fun (annot, arg_annot) ->
        let arg_annot = default_annot (field_to_var_annot arg_annot) ~default:default_arg_annot in
        parse_returning Lambda ?type_logger ctxt
          (arg, arg_annot) ret code >>=? fun (lambda, ctxt) ->
        typed ctxt loc (Lambda lambda) (Item_t (Lambda_t (arg, ret, None), stack, annot))
    | Prim (loc, I_EXEC, [], annot),
      Item_t (arg, Item_t (Lambda_t (param, ret, _), rest, _), _) ->
        check_item_ty arg param loc I_EXEC 1 2 >>=? fun Eq ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Exec (Item_t (ret, rest, annot))
    | Prim (loc, I_DIP, [ code ], annot),
      Item_t (v, rest, stack_annot) ->
        fail_unexpected_annot loc annot >>=? fun () ->
        check_kind [ Seq_kind ] code >>=? fun () ->
        parse_instr ?type_logger (add_dip v stack_annot tc_context) ctxt code
          rest >>=? begin fun (judgement, ctxt) -> match judgement with
          | Typed descr ->
              typed ctxt loc (Dip descr) (Item_t (v, descr.aft, stack_annot))
          | Failed _ ->
              fail (Fail_not_in_tail_position loc)
        end
    | Prim (loc, I_FAIL, [], annot),
      bef ->
        fail_unexpected_annot loc annot >>=? fun () ->
        let descr aft = { loc ; instr = Fail ; bef ; aft } in
        return ctxt (Failed { descr })
    (* timestamp operations *)
    | Prim (loc, I_ADD, [], annot),
      Item_t (Timestamp_t tn1, Item_t (Int_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Add_timestamp_to_seconds
          (Item_t (Timestamp_t tname, rest, annot))
    | Prim (loc, I_ADD, [], annot),
      Item_t (Int_t tn1, Item_t (Timestamp_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Add_seconds_to_timestamp
          (Item_t (Timestamp_t tname, rest, annot))
    | Prim (loc, I_SUB, [], annot),
      Item_t (Timestamp_t tn1, Item_t (Int_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Sub_timestamp_seconds
          (Item_t (Timestamp_t tname, rest, annot))
    | Prim (loc, I_SUB, [], annot),
      Item_t (Timestamp_t tn1, Item_t (Timestamp_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Diff_timestamps
          (Item_t (Int_t tname, rest, annot))
    (* string operations *)
    | Prim (loc, I_CONCAT, [], annot),
      Item_t (String_t tn1, Item_t (String_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Concat
          (Item_t (String_t tname, rest, annot))
    (* currency operations *)
    | Prim (loc, I_ADD, [], annot),
      Item_t (Mutez_t tn1, Item_t (Mutez_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Add_tez
          (Item_t (Mutez_t tname, rest, annot))
    | Prim (loc, I_SUB, [], annot),
      Item_t (Mutez_t tn1, Item_t (Mutez_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Sub_tez
          (Item_t (Mutez_t tname, rest, annot))
    | Prim (loc, I_MUL, [], annot),
      Item_t (Mutez_t tname, Item_t (Nat_t _, rest, _), _) -> (* no type name check *)
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Mul_teznat
          (Item_t (Mutez_t tname, rest, annot))
    | Prim (loc, I_MUL, [], annot),
      Item_t (Nat_t _, Item_t (Mutez_t tname, rest, _), _) -> (* no type name check *)
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Mul_nattez
          (Item_t (Mutez_t tname, rest, annot))
    (* boolean operations *)
    | Prim (loc, I_OR, [], annot),
      Item_t (Bool_t tn1, Item_t (Bool_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Or
          (Item_t (Bool_t tname, rest, annot))
    | Prim (loc, I_AND, [], annot),
      Item_t (Bool_t tn1, Item_t (Bool_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc And
          (Item_t (Bool_t tname, rest, annot))
    | Prim (loc, I_XOR, [], annot),
      Item_t (Bool_t tn1, Item_t (Bool_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Xor
          (Item_t (Bool_t tname, rest, annot))
    | Prim (loc, I_NOT, [], annot),
      Item_t (Bool_t tname, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Not
          (Item_t (Bool_t tname, rest, annot))
    (* integer operations *)
    | Prim (loc, I_ABS, [], annot),
      Item_t (Int_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Abs_int
          (Item_t (Nat_t None, rest, annot))
    | Prim (loc, I_ISNAT, [], annot),
      Item_t (Int_t _, rest, int_annot) ->
        parse_var_annot loc annot ~default:int_annot >>=? fun annot ->
        typed ctxt loc Is_nat
          (Item_t (Option_t ((Nat_t None, None), None, None), rest, annot))
    | Prim (loc, I_INT, [], annot),
      Item_t (Nat_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Int_nat
          (Item_t (Int_t None, rest, annot))
    | Prim (loc, I_NEG, [], annot),
      Item_t (Int_t tname, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Neg_int
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_NEG, [], annot),
      Item_t (Nat_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Neg_nat
          (Item_t (Int_t None, rest, annot))
    | Prim (loc, I_ADD, [], annot),
      Item_t (Int_t tn1, Item_t (Int_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Add_intint
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_ADD, [], annot),
      Item_t (Int_t tname, Item_t (Nat_t _, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Add_intnat
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_ADD, [], annot),
      Item_t (Nat_t _, Item_t (Int_t tname, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Add_natint
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_ADD, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Add_natnat
          (Item_t (Nat_t  tname, rest, annot))
    | Prim (loc, I_SUB, [], annot),
      Item_t (Int_t tn1, Item_t (Int_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Sub_int
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_SUB, [], annot),
      Item_t (Int_t tname, Item_t (Nat_t _, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Sub_int
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_SUB, [], annot),
      Item_t (Nat_t _, Item_t (Int_t tname, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Sub_int
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_SUB, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun _tname ->
        typed ctxt loc Sub_int
          (Item_t (Int_t None, rest, annot))
    | Prim (loc, I_MUL, [], annot),
      Item_t (Int_t tn1, Item_t (Int_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Mul_intint
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_MUL, [], annot),
      Item_t (Int_t tname, Item_t (Nat_t _, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Mul_intnat
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_MUL, [], annot),
      Item_t (Nat_t _, Item_t (Int_t tname, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Mul_natint
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_MUL, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Mul_natnat
          (Item_t (Nat_t tname, rest, annot))
    | Prim (loc, I_EDIV, [], annot),
      Item_t (Mutez_t tname, Item_t (Nat_t _, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Ediv_teznat
          (Item_t (Option_t
                     ((Pair_t ((Mutez_t tname, None), (Mutez_t tname, None), None), None),
                      None, None), rest, annot))
    | Prim (loc, I_EDIV, [], annot),
      Item_t (Mutez_t tn1, Item_t (Mutez_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Ediv_tez
          (Item_t (Option_t ((Pair_t ((Nat_t None, None), (Mutez_t tname, None), None), None),
                            None, None), rest, annot))
    | Prim (loc, I_EDIV, [], annot),
      Item_t (Int_t tn1, Item_t (Int_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Ediv_intint
          (Item_t (Option_t
                     ((Pair_t ((Int_t tname, None), (Nat_t None, None), None), None),
                      None, None), rest, annot))
    | Prim (loc, I_EDIV, [], annot),
      Item_t (Int_t tname, Item_t (Nat_t _, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Ediv_intnat
          (Item_t (Option_t
                     ((Pair_t ((Int_t tname, None), (Nat_t None, None), None), None),
                     None, None), rest, annot))
    | Prim (loc, I_EDIV, [], annot),
      Item_t (Nat_t tname, Item_t (Int_t _, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Ediv_natint
          (Item_t (Option_t ((Pair_t ((Int_t None, None), (Nat_t tname, None), None), None),
                             None, None), rest, annot))
    | Prim (loc, I_EDIV, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Ediv_natnat
          (Item_t (Option_t ((Pair_t ((Nat_t tname, None), (Nat_t tname, None), None), None),
                             None, None), rest, annot))
    | Prim (loc, I_LSL, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Lsl_nat
          (Item_t (Nat_t tname, rest, annot))
    | Prim (loc, I_LSR, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Lsr_nat
          (Item_t (Nat_t tname, rest, annot))
    | Prim (loc, I_OR, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Or_nat
          (Item_t (Nat_t tname, rest, annot))
    | Prim (loc, I_AND, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc And_nat
          (Item_t (Nat_t tname, rest, annot))
    | Prim (loc, I_AND, [], annot),
      Item_t (Int_t _, Item_t (Nat_t tname, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc And_int_nat
          (Item_t (Nat_t tname, rest, annot))
    | Prim (loc, I_XOR, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc Xor_nat
          (Item_t (Nat_t tname, rest, annot))
    | Prim (loc, I_NOT, [], annot),
      Item_t (Int_t tname, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Not_int
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_NOT, [], annot),
      Item_t (Nat_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Not_nat
          (Item_t (Int_t None, rest, annot))
    (* comparison *)
    | Prim (loc, I_COMPARE, [], annot),
      Item_t (Int_t tn1, Item_t (Int_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc (Compare (Int_key tname))
          (Item_t (Int_t None, rest, annot))
    | Prim (loc, I_COMPARE, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc (Compare (Nat_key tname))
          (Item_t (Int_t None, rest, annot))
    | Prim (loc, I_COMPARE, [], annot),
      Item_t (Bool_t tn1, Item_t (Bool_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc (Compare (Bool_key tname))
          (Item_t (Int_t None, rest, annot))
    | Prim (loc, I_COMPARE, [], annot),
      Item_t (String_t tn1, Item_t (String_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc (Compare (String_key tname))
          (Item_t (Int_t None, rest, annot))
    | Prim (loc, I_COMPARE, [], annot),
      Item_t (Mutez_t tn1, Item_t (Mutez_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc (Compare (Mutez_key tname))
          (Item_t (Int_t None, rest, annot))
    | Prim (loc, I_COMPARE, [], annot),
      Item_t (Key_hash_t tn1, Item_t (Key_hash_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc (Compare (Key_hash_key tname))
          (Item_t (Int_t None, rest, annot))
    | Prim (loc, I_COMPARE, [], annot),
      Item_t (Timestamp_t tn1, Item_t (Timestamp_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc (Compare (Timestamp_key tname))
          (Item_t (Int_t None, rest, annot))
    | Prim (loc, I_COMPARE, [], annot),
      Item_t (Address_t tn1, Item_t (Address_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return (merge_type_annot tn1 tn2) >>=? fun tname ->
        typed ctxt loc (Compare (Address_key tname))
          (Item_t (Int_t None, rest, annot))
    (* comparators *)
    | Prim (loc, I_EQ, [], annot),
      Item_t (Int_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Eq
          (Item_t (Bool_t None, rest, annot))
    | Prim (loc, I_NEQ, [], annot),
      Item_t (Int_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Neq
          (Item_t (Bool_t None, rest, annot))
    | Prim (loc, I_LT, [], annot),
      Item_t (Int_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Lt
          (Item_t (Bool_t None, rest, annot))
    | Prim (loc, I_GT, [], annot),
      Item_t (Int_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Gt
          (Item_t (Bool_t None, rest, annot))
    | Prim (loc, I_LE, [], annot),
      Item_t (Int_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Le
          (Item_t (Bool_t None, rest, annot))
    | Prim (loc, I_GE, [], annot),
      Item_t (Int_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Ge
          (Item_t (Bool_t None, rest, annot))
    (* protocol *)
    | Prim (loc, I_ADDRESS, [], annot),
      Item_t (Contract_t _, rest, contract_annot) ->
        parse_var_annot loc annot ~default:(access_annot contract_annot default_addr_annot)
        >>=? fun annot ->
        typed ctxt loc Address
          (Item_t (Address_t None, rest, annot))
    | Prim (loc, I_CONTRACT, [ ty ], annot),
      Item_t (Address_t _, rest, addr_annot) ->
        Lwt.return (parse_ty ~allow_big_map:false ~allow_operation:false ty) >>=? fun (Ex_ty t) ->
        parse_var_annot loc annot ~default:(access_annot addr_annot default_contract_annot)
        >>=? fun annot ->
        typed ctxt loc (Contract t)
          (Item_t (Option_t ((Contract_t (t, None), None), None, None), rest, annot))
    | Prim (loc, I_MANAGER, [], annot),
      Item_t (Contract_t _, rest, contract_annot) ->
        parse_var_annot loc annot ~default:(access_annot contract_annot default_manager_annot)
        >>=? fun annot ->
        typed ctxt loc Manager
          (Item_t (Key_hash_t None, rest, annot))
    | Prim (loc, I_MANAGER, [], annot),
      Item_t (Address_t _, rest, addr_annot) ->
        parse_var_annot loc annot ~default:(access_annot addr_annot default_manager_annot)
        >>=? fun annot ->
        typed ctxt loc Address_manager
          (Item_t (Option_t ((Key_hash_t None, None), None, None), rest, annot))
    | Prim (loc, I_TRANSFER_TOKENS, [], annot),
      Item_t (p, Item_t
                (Mutez_t _, Item_t
                   (Contract_t (cp, _), rest, _), _), _) ->
        check_item_ty p cp loc I_TRANSFER_TOKENS 1 4 >>=? fun Eq ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Transfer_tokens (Item_t (Operation_t None, rest, annot))
    | Prim (loc, I_SET_DELEGATE, [], annot),
      Item_t (Option_t ((Key_hash_t _, _), _, _), rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Set_delegate (Item_t (Operation_t None, rest, annot))
    | Prim (loc, I_CREATE_ACCOUNT, [], annot),
      Item_t
        (Key_hash_t _, Item_t
           (Option_t ((Key_hash_t _, _), _, _), Item_t
              (Bool_t _, Item_t
                 (Mutez_t _, rest, _), _), _), _) ->
        parse_two_var_annot loc annot >>=? fun (op_annot, addr_annot) ->
        typed ctxt loc Create_account
          (Item_t (Operation_t None, Item_t (Address_t None, rest, addr_annot), op_annot))
    | Prim (loc, I_IMPLICIT_ACCOUNT, [], annot),
      Item_t (Key_hash_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Implicit_account
          (Item_t (Contract_t (Unit_t None, None), rest, annot))
    | Prim (loc, I_CREATE_CONTRACT, [ (Seq _ as code)], annot),
      Item_t
        (Key_hash_t _, Item_t
           (Option_t ((Key_hash_t _, _), _, _), Item_t
              (Bool_t _, Item_t
                 (Bool_t _, Item_t
                    (Mutez_t _, Item_t
                       (ginit, rest, _), _), _), _), _), _) ->
        parse_two_var_annot loc annot >>=? fun (op_annot, addr_annot) ->
        let cannonical_code = fst @@ Micheline.extract_locations code in
        Lwt.return (parse_toplevel cannonical_code) >>=? fun (arg_type, storage_type, code_field) ->
        trace
          (Ill_formed_type (Some "parameter", cannonical_code, location arg_type))
          (Lwt.return (parse_ty ~allow_big_map:false ~allow_operation:false arg_type))
        >>=? fun (Ex_ty arg_type) ->
        trace
          (Ill_formed_type (Some "storage", cannonical_code, location storage_type))
          (Lwt.return (parse_ty ~allow_big_map:true ~allow_operation:false storage_type))
        >>=? fun (Ex_ty storage_type) ->
        let arg_field = default_annot (type_to_field_annot (name_of_ty arg_type))
            ~default:default_param_annot in
        let storage_field = default_annot (type_to_field_annot (name_of_ty storage_type))
            ~default:default_storage_annot in
        let arg_type_full = Pair_t ((arg_type, arg_field), (storage_type, storage_field), None) in
        let ret_type_full =
          Pair_t ((List_t (Operation_t None, None), None), (storage_type, None), None) in
        trace
          (Ill_typed_contract (cannonical_code, []))
          (parse_returning (Toplevel { storage_type ; param_type = arg_type })
             ctxt ?type_logger (arg_type_full, None) ret_type_full code_field) >>=?
        fun (Lam ({ bef = Item_t (arg, Empty_t, _) ;
                    aft = Item_t (ret, Empty_t, _) ; _ }, _) as lambda, ctxt) ->
        Lwt.return @@ ty_eq arg arg_type_full >>=? fun Eq ->
        Lwt.return @@ ty_eq ret ret_type_full >>=? fun Eq ->
        Lwt.return @@ ty_eq storage_type ginit >>=? fun Eq ->
        typed ctxt loc (Create_contract (storage_type, arg_type, lambda))
          (Item_t (Operation_t None, Item_t (Address_t None, rest, addr_annot), op_annot))
    | Prim (loc, I_NOW, [], annot),
      stack ->
        parse_var_annot loc annot ~default:default_now_annot >>=? fun annot ->
        typed ctxt loc Now (Item_t (Timestamp_t None, stack, annot))
    | Prim (loc, I_AMOUNT, [], annot),
      stack ->
        parse_var_annot loc annot ~default:default_amount_annot >>=? fun annot ->
        typed ctxt loc Amount
          (Item_t (Mutez_t None, stack, annot))
    | Prim (loc, I_BALANCE, [], annot),
      stack ->
        parse_var_annot loc annot ~default:default_balance_annot >>=? fun annot ->
        typed ctxt loc Balance
          (Item_t (Mutez_t None, stack, annot))
    | Prim (loc, I_HASH_KEY, [], annot),
      Item_t (Key_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Hash_key
          (Item_t (Key_hash_t None, rest, annot))
    | Prim (loc, I_CHECK_SIGNATURE, [], annot),
      Item_t (Key_t _, Item_t (Signature_t _, Item_t (String_t _, rest, _), _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Check_signature
          (Item_t (Bool_t None, rest, annot))
    | Prim (loc, I_H, [], annot),
      Item_t (t, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc (H t)
          (Item_t (String_t None, rest, annot))
    | Prim (loc, I_STEPS_TO_QUOTA, [], annot),
      stack ->
        parse_var_annot loc annot ~default:default_steps_annot >>=? fun annot ->
        typed ctxt loc Steps_to_quota
          (Item_t (Nat_t None, stack, annot))
    | Prim (loc, I_SOURCE, [], annot),
      stack ->
        parse_var_annot loc annot ~default:default_source_annot >>=? fun annot ->
        typed ctxt loc Source
          (Item_t (Address_t None, stack, annot))
    | Prim (loc, I_SELF, [], annot),
      stack ->
        parse_var_annot loc annot ~default:default_self_annot >>=? fun annot ->
        let rec get_toplevel_type : tc_context -> (bef judgement * context) tzresult Lwt.t = function
          | Lambda -> fail (Self_in_lambda loc)
          | Dip (_, prev) -> get_toplevel_type prev
          | Toplevel { param_type ; _ } ->
              typed ctxt loc (Self param_type)
                (Item_t (Contract_t (param_type, None), stack, annot)) in
        get_toplevel_type tc_context
    (* Primitive parsing errors *)
    | Prim (loc, (I_DROP | I_DUP | I_SWAP | I_SOME | I_UNIT
                 | I_PAIR | I_CAR | I_CDR | I_CONS
                 | I_MEM | I_UPDATE | I_MAP
                 | I_GET | I_EXEC | I_FAIL | I_SIZE
                 | I_CONCAT | I_ADD | I_SUB
                 | I_MUL | I_EDIV | I_OR | I_AND | I_XOR
                 | I_NOT
                 | I_ABS | I_NEG | I_LSL | I_LSR
                 | I_COMPARE | I_EQ | I_NEQ
                 | I_LT | I_GT | I_LE | I_GE
                 | I_MANAGER | I_TRANSFER_TOKENS | I_CREATE_ACCOUNT
                 | I_CREATE_CONTRACT | I_SET_DELEGATE | I_NOW
                 | I_IMPLICIT_ACCOUNT | I_AMOUNT | I_BALANCE
                 | I_CHECK_SIGNATURE | I_HASH_KEY | I_SOURCE
                 | I_H | I_STEPS_TO_QUOTA | I_ADDRESS
                 as name), (_ :: _ as l), _), _ ->
        fail (Invalid_arity (loc, name, 0, List.length l))
    | Prim (loc, (I_NONE | I_LEFT | I_RIGHT | I_NIL | I_MAP | I_ITER
                 | I_EMPTY_SET | I_DIP | I_LOOP | I_LOOP_LEFT | I_CONTRACT
                 as name), ([]
                           | _ :: _ :: _ as l), _), _ ->
        fail (Invalid_arity (loc, name, 1, List.length l))
    | Prim (loc, (I_PUSH | I_IF_NONE | I_IF_LEFT | I_IF_CONS
                 | I_EMPTY_MAP | I_IF
                 as name), ([] | [ _ ]
                           | _ :: _ :: _ :: _ as l), _), _ ->
        fail (Invalid_arity (loc, name, 2, List.length l))
    | Prim (loc, I_LAMBDA, ([] | [ _ ]
                           | _ :: _ :: _ :: _ :: _ as l), _), _ ->
        fail (Invalid_arity (loc, I_LAMBDA, 3, List.length l))
    (* Stack errors *)
    | Prim (loc, (I_ADD | I_SUB | I_MUL | I_EDIV
                 | I_AND | I_OR | I_XOR | I_LSL | I_LSR
                 | I_CONCAT | I_COMPARE as name), [], _),
      Item_t (ta, Item_t (tb, _, _), _) ->
        fail (Undefined_binop (loc, name, ta, tb))
    | Prim (loc, (I_NEG | I_ABS | I_NOT
                 | I_EQ | I_NEQ | I_LT | I_GT | I_LE | I_GE as name),
            [], _),
      Item_t (t, _, _) ->
        fail (Undefined_unop (loc, name, t))
    | Prim (loc, I_UPDATE, [], _),
      stack ->
        fail (Bad_stack (loc, I_UPDATE, 3, stack))
    | Prim (loc, I_CREATE_CONTRACT, [], _),
      stack ->
        fail (Bad_stack (loc, I_CREATE_CONTRACT, 7, stack))
    | Prim (loc, I_CREATE_ACCOUNT, [], _),
      stack ->
        fail (Bad_stack (loc, I_CREATE_ACCOUNT, 4, stack))
    | Prim (loc, I_TRANSFER_TOKENS, [], _),
      stack ->
        fail (Bad_stack (loc, I_TRANSFER_TOKENS, 4, stack))
    | Prim (loc, (I_DROP | I_DUP | I_CAR | I_CDR | I_SOME | I_H | I_DIP
                 | I_IF_NONE | I_LEFT | I_RIGHT | I_IF_LEFT | I_IF
                 | I_LOOP | I_IF_CONS | I_MANAGER | I_IMPLICIT_ACCOUNT
                 | I_NEG | I_ABS | I_INT | I_NOT
                 | I_EQ | I_NEQ | I_LT | I_GT | I_LE | I_GE as name), _, _),
      stack ->
        fail (Bad_stack (loc, name, 1, stack))
    | Prim (loc, (I_SWAP | I_PAIR | I_CONS
                 | I_GET | I_MEM | I_EXEC
                 | I_CHECK_SIGNATURE | I_ADD | I_SUB | I_MUL
                 | I_EDIV | I_AND | I_OR | I_XOR
                 | I_LSL | I_LSR | I_CONCAT as name), _, _),
      stack ->
        fail (Bad_stack (loc, name, 2, stack))
    (* Generic parsing errors *)
    | expr, _ ->
        fail @@ unexpected expr [ Seq_kind ] Instr_namespace
          [ I_DROP ; I_DUP ; I_SWAP ; I_SOME ; I_UNIT ;
            I_PAIR ; I_CAR ; I_CDR ; I_CONS ;
            I_MEM ; I_UPDATE ; I_MAP ; I_ITER ;
            I_GET ; I_EXEC ; I_FAIL ; I_SIZE ;
            I_CONCAT ; I_ADD ; I_SUB ;
            I_MUL ; I_EDIV ; I_OR ; I_AND ; I_XOR ;
            I_NOT ;
            I_ABS ; I_INT; I_NEG ; I_LSL ; I_LSR ;
            I_COMPARE ; I_EQ ; I_NEQ ;
            I_LT ; I_GT ; I_LE ; I_GE ;
            I_MANAGER ; I_TRANSFER_TOKENS ; I_CREATE_ACCOUNT ;
            I_CREATE_CONTRACT ; I_NOW ; I_AMOUNT ; I_BALANCE ;
            I_IMPLICIT_ACCOUNT ; I_CHECK_SIGNATURE ; I_H ; I_HASH_KEY ;
            I_STEPS_TO_QUOTA ;
            I_PUSH ; I_NONE ; I_LEFT ; I_RIGHT ; I_NIL ;
            I_EMPTY_SET ; I_DIP ; I_LOOP ;
            I_IF_NONE ; I_IF_LEFT ; I_IF_CONS ;
            I_EMPTY_MAP ; I_IF ; I_SOURCE ; I_SELF ; I_LAMBDA ]

and parse_contract
  : type arg. context -> Script.location -> arg ty -> Contract.t ->
    (context * arg typed_contract) tzresult Lwt.t
  = fun ctxt loc arg contract ->
    Lwt.return (Gas.consume ctxt Typecheck_costs.contract_exists) >>=? fun ctxt ->
    Contract.exists ctxt contract >>=? function
    | false -> fail (Invalid_contract (loc, contract))
    | true ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.get_script) >>=? fun ctxt ->
        trace
          (Invalid_contract (loc, contract)) @@
        Contract.get_script ctxt contract >>=? fun (ctxt, script) -> match script with
        | None ->
            Lwt.return
              (ty_eq arg (Unit_t None)>>? fun Eq ->
               let contract : arg typed_contract = (arg, contract) in
               ok (ctxt, contract))
        | Some { code ; _ } ->
            Lwt.return
              (Script.force_decode code >>? fun code ->
               parse_toplevel code >>? fun (arg_type, _, _) ->
               parse_ty ~allow_big_map:false ~allow_operation:false arg_type >>? fun (Ex_ty targ) ->
               ty_eq targ arg >>? fun Eq ->
               let contract : arg typed_contract = (arg, contract) in
               ok (ctxt, contract))

and parse_toplevel
  : Script.expr -> (Script.node * Script.node * Script.node) tzresult
  = fun toplevel ->
    record_trace (Ill_typed_contract (toplevel, [])) @@
    match root toplevel with
    | Int (loc, _) -> error (Invalid_kind (loc, [ Seq_kind ], Int_kind))
    | String (loc, _) -> error (Invalid_kind (loc, [ Seq_kind ], String_kind))
    | Prim (loc, _, _, _) -> error (Invalid_kind (loc, [ Seq_kind ], Prim_kind))
    | Seq (_, fields) ->
        let rec find_fields p s c fields =
          match fields with
          | [] -> ok (p, s, c)
          | Int (loc, _) :: _ -> error (Invalid_kind (loc, [ Prim_kind ], Int_kind))
          | String (loc, _) :: _ -> error (Invalid_kind (loc, [ Prim_kind ], String_kind))
          | Seq (loc, _) :: _ -> error (Invalid_kind (loc, [ Prim_kind ], Seq_kind))
          | Prim (loc, K_parameter, [ arg ], _) :: rest ->
              begin match p with
                | None -> find_fields (Some arg) s c rest
                | Some _ -> error (Duplicate_field (loc, K_parameter))
              end
          | Prim (loc, K_storage, [ arg ], _) :: rest ->
              begin match s with
                | None -> find_fields p (Some arg) c rest
                | Some _ -> error (Duplicate_field (loc, K_storage))
              end
          | Prim (loc, K_code, [ arg ], _) :: rest ->
              begin match c with
                | None -> find_fields p s (Some arg) rest
                | Some _ -> error (Duplicate_field (loc, K_code))
              end
          | Prim (loc, (K_parameter | K_storage | K_code as name), args, _) :: _ ->
              error (Invalid_arity (loc, name, 1, List.length args))
          | Prim (loc, name, _, _) :: _ ->
              let allowed = [ K_parameter ; K_storage ; K_code ] in
              error (Invalid_primitive (loc, allowed, name))
        in
        find_fields None None None fields >>? function
        | (None, _, _) -> error (Missing_field K_parameter)
        | (Some _, None, _) -> error (Missing_field K_storage)
        | (Some _, Some _, None) -> error (Missing_field K_code)
        | (Some p, Some s, Some c) -> ok (p, s, c)

let parse_script
  : ?type_logger: type_logger ->
    context -> Script.t -> (ex_script * context) tzresult Lwt.t
  = fun ?type_logger ctxt { code ; storage } ->
    Lwt.return (Script.force_decode code) >>=? fun code ->
    Lwt.return (Script.force_decode storage) >>=? fun storage ->
    Lwt.return (parse_toplevel code) >>=? fun (arg_type, storage_type, code_field) ->
    trace
      (Ill_formed_type (Some "parameter", code, location arg_type))
      (Lwt.return (parse_ty ~allow_big_map:false ~allow_operation:false arg_type))
    >>=? fun (Ex_ty arg_type) ->
    trace
      (Ill_formed_type (Some "storage", code, location storage_type))
      (Lwt.return (parse_ty ~allow_big_map:true ~allow_operation:false storage_type))
    >>=? fun (Ex_ty storage_type) ->
    let arg_field = default_annot (type_to_field_annot (name_of_ty arg_type))
        ~default:default_param_annot in
    let storage_field = default_annot (type_to_field_annot (name_of_ty storage_type))
        ~default:default_storage_annot in
    let arg_type_full = Pair_t ((arg_type, arg_field), (storage_type, storage_field), None) in
    let ret_type_full =
      Pair_t ((List_t (Operation_t None, None), None), (storage_type, None), None) in
    trace
      (Ill_typed_data (None, storage, storage_type))
      (parse_data ?type_logger ctxt storage_type (root storage)) >>=? fun (storage, ctxt) ->
    trace
      (Ill_typed_contract (code, []))
      (parse_returning (Toplevel { storage_type ; param_type = arg_type })
         ctxt ?type_logger (arg_type_full, None) ret_type_full code_field) >>=? fun (code, ctxt) ->
    return (Ex_script { code ; arg_type ; storage ; storage_type }, ctxt)

let typecheck_code
  : context -> Script.expr -> (type_map * context) tzresult Lwt.t
  = fun ctxt code ->
    Lwt.return (parse_toplevel code) >>=? fun (arg_type, storage_type, code_field) ->
    let type_map = ref [] in
    (* TODO: annotation checking *)
    trace
      (Ill_formed_type (Some "parameter", code, location arg_type))
      (Lwt.return (parse_ty ~allow_big_map:false ~allow_operation:false arg_type))
    >>=? fun (Ex_ty arg_type) ->
    trace
      (Ill_formed_type (Some "storage", code, location storage_type))
      (Lwt.return (parse_ty ~allow_big_map:true ~allow_operation:false storage_type))
    >>=? fun (Ex_ty storage_type) ->
    let arg_field = default_annot (type_to_field_annot (name_of_ty arg_type))
        ~default:default_param_annot in
    let storage_field = default_annot (type_to_field_annot (name_of_ty storage_type))
        ~default:default_storage_annot in
    let arg_type_full = Pair_t ((arg_type, arg_field), (storage_type, storage_field), None) in
    let ret_type_full =
      Pair_t ((List_t (Operation_t None, None), None), (storage_type, None), None) in
    let result =
      parse_returning
        (Toplevel { storage_type ; param_type = arg_type })
        ctxt
        ~type_logger: (fun loc bef aft -> type_map := (loc, (bef, aft)) :: !type_map)
        (arg_type_full, None) ret_type_full code_field in
    trace
      (Ill_typed_contract (code, !type_map))
      result >>=? fun (Lam _, ctxt) ->
    return (!type_map, ctxt)

let typecheck_data
  : ?type_logger: type_logger ->
    context -> Script.expr * Script.expr -> context tzresult Lwt.t
  = fun ?type_logger ctxt (data, exp_ty) ->
    trace
      (Ill_formed_type (None, exp_ty, 0))
      (Lwt.return (parse_ty ~allow_big_map:true ~allow_operation:false (root exp_ty)))
    >>=? fun (Ex_ty exp_ty) ->
    trace
      (Ill_typed_data (None, data, exp_ty))
      (parse_data ?type_logger ctxt exp_ty (root data)) >>=? fun (_, ctxt) ->
    return ctxt

(* ---- Unparsing (Typed IR -> Untyped expressions) --------------------------*)

module Unparse_costs = Michelson_v1_gas.Cost_of.Unparse

let rec unparse_data
  : type a. context -> unparsing_mode -> a ty -> a -> (Script.node * context) tzresult Lwt.t
  = fun ctxt mode ty a ->
    let padded_z_of_bytes bytes =
      let bytes = MBytes.concat "" [ bytes ; MBytes.of_string "\xFF" ] in
      Z.of_bits bytes in
    Lwt.return (Gas.consume ctxt Unparse_costs.cycle) >>=? fun ctxt ->
    match ty, a with
    | Unit_t _, () ->
        Lwt.return (Gas.consume ctxt Unparse_costs.unit) >>=? fun ctxt ->
        return (Prim (-1, D_Unit, [], []), ctxt)
    | Int_t _, v ->
        Lwt.return (Gas.consume ctxt (Unparse_costs.int v)) >>=? fun ctxt ->
        return (Int (-1, Script_int.to_zint v), ctxt)
    | Nat_t _, v ->
        Lwt.return (Gas.consume ctxt (Unparse_costs.int v)) >>=? fun ctxt ->
        return (Int (-1, Script_int.to_zint v), ctxt)
    | String_t _, s ->
        Lwt.return (Gas.consume ctxt (Unparse_costs.string s)) >>=? fun ctxt ->
        return (String (-1, s), ctxt)
    | Bool_t _, true ->
        Lwt.return (Gas.consume ctxt Unparse_costs.bool) >>=? fun ctxt ->
        return (Prim (-1, D_True, [], []), ctxt)
    | Bool_t _, false ->
        Lwt.return (Gas.consume ctxt Unparse_costs.bool) >>=? fun ctxt ->
        return (Prim (-1, D_False, [], []), ctxt)
    | Timestamp_t _, t ->
        Lwt.return (Gas.consume ctxt (Unparse_costs.timestamp t)) >>=? fun ctxt ->
        begin
          match mode with
          | Optimized -> return (Int (-1, Script_timestamp.to_zint t), ctxt)
          | Readable ->
              match Script_timestamp.to_notation t with
              | None -> return (Int (-1, Script_timestamp.to_zint t), ctxt)
              | Some s -> return (String (-1, s), ctxt)
        end
    | Address_t _, c  ->
        Lwt.return (Gas.consume ctxt Unparse_costs.contract) >>=? fun ctxt ->
        begin
          match mode with
          | Optimized ->
              let bytes = Data_encoding.Binary.to_bytes_exn Contract.encoding c in
              return (Int (-1, Z.of_bits bytes), ctxt)
          | Readable -> return (String (-1, Contract.to_b58check c), ctxt)
        end
    | Contract_t _, (_, c)  ->
        Lwt.return (Gas.consume ctxt Unparse_costs.contract) >>=? fun ctxt ->
        begin
          match mode with
          | Optimized ->
              let bytes = Data_encoding.Binary.to_bytes_exn Contract.encoding c in
              return (Int (-1, Z.of_bits bytes), ctxt)
          | Readable -> return (String (-1, Contract.to_b58check c), ctxt)
        end
    | Signature_t _, s ->
        Lwt.return (Gas.consume ctxt Unparse_costs.signature) >>=? fun ctxt ->
        begin
          match mode with
          | Optimized ->
              let bytes = Data_encoding.Binary.to_bytes_exn Signature.encoding s in
              return (Int (-1, Z.of_bits bytes), ctxt)
          | Readable ->
              return (String (-1, Signature.to_b58check s), ctxt)
        end
    | Mutez_t _, v ->
        Lwt.return (Gas.consume ctxt Unparse_costs.tez) >>=? fun ctxt ->
        return (Int (-1, Z.of_int64 (Tez.to_mutez v)), ctxt)
    | Key_t _, k ->
        Lwt.return (Gas.consume ctxt Unparse_costs.key) >>=? fun ctxt ->
        begin
          match mode with
          | Optimized ->
              let bytes = Data_encoding.Binary.to_bytes_exn Signature.Public_key.encoding k in
              return (Int (-1, padded_z_of_bytes bytes), ctxt)
          | Readable ->
              return (String (-1, Signature.Public_key.to_b58check k), ctxt)
        end
    | Key_hash_t _, k ->
        Lwt.return (Gas.consume ctxt Unparse_costs.key_hash) >>=? fun ctxt ->
        begin
          match mode with
          | Optimized ->
              let bytes = Data_encoding.Binary.to_bytes_exn Signature.Public_key_hash.encoding k in
              return (Int (-1, Z.of_bits bytes), ctxt)
          | Readable ->
              return (String (-1, Signature.Public_key_hash.to_b58check k), ctxt)
        end
    | Operation_t _, op ->
        let bytes = Data_encoding.Binary.to_bytes_exn Operation.internal_operation_encoding op in
        let `Hex text = MBytes.to_hex bytes in
        Lwt.return (Gas.consume ctxt (Unparse_costs.operation bytes)) >>=? fun ctxt ->
        return (String (-1, text), ctxt)
    | Pair_t ((tl, _), (tr, _), _), (l, r) ->
        Lwt.return (Gas.consume ctxt Unparse_costs.pair) >>=? fun ctxt ->
        unparse_data ctxt mode tl l >>=? fun (l, ctxt) ->
        unparse_data ctxt mode tr r >>=? fun (r, ctxt) ->
        return (Prim (-1, D_Pair, [ l; r ], []), ctxt)
    | Union_t ((tl, _), _, _), L l ->
        Lwt.return (Gas.consume ctxt Unparse_costs.union) >>=? fun ctxt ->
        unparse_data ctxt mode tl l >>=? fun (l, ctxt) ->
        return (Prim (-1, D_Left, [ l ], []), ctxt)
    | Union_t (_, (tr, _), _), R r ->
        Lwt.return (Gas.consume ctxt Unparse_costs.union) >>=? fun ctxt ->
        unparse_data ctxt mode tr r >>=? fun (r, ctxt) ->
        return (Prim (-1, D_Right, [ r ], []), ctxt)
    | Option_t ((t, _), _, _), Some v ->
        Lwt.return (Gas.consume ctxt Unparse_costs.some) >>=? fun ctxt ->
        unparse_data ctxt mode t v >>=? fun (v, ctxt) ->
        return (Prim (-1, D_Some, [ v ], []), ctxt)
    | Option_t _, None ->
        Lwt.return (Gas.consume ctxt Unparse_costs.none) >>=? fun ctxt ->
        return (Prim (-1, D_None, [], []), ctxt)
    | List_t (t, _), items ->
        fold_left_s
          (fun (l, ctxt) element ->
             Lwt.return (Gas.consume ctxt Unparse_costs.list_element) >>=? fun ctxt ->
             unparse_data ctxt mode t element >>=? fun (unparsed, ctxt) ->
             return (unparsed :: l, ctxt))
          ([], ctxt)
          items >>=? fun (items, ctxt) ->
        return (Micheline.Seq (-1, List.rev items), ctxt)
    | Set_t (t, _), set ->
        let t = ty_of_comparable_ty t in
        fold_left_s
          (fun (l, ctxt) item ->
             Lwt.return (Gas.consume ctxt Unparse_costs.set_element) >>=? fun ctxt ->
             unparse_data ctxt mode t item >>=? fun (item, ctxt) ->
             return (item :: l, ctxt))
          ([], ctxt)
          (set_fold (fun e acc -> e :: acc) set []) >>=? fun (items, ctxt) ->
        return (Micheline.Seq (-1, items), ctxt)
    | Map_t (kt, vt, _), map ->
        let kt = ty_of_comparable_ty kt in
        fold_left_s
          (fun (l, ctxt) (k, v) ->
             Lwt.return (Gas.consume ctxt Unparse_costs.map_element) >>=? fun ctxt ->
             unparse_data ctxt mode kt k >>=? fun (key, ctxt) ->
             unparse_data ctxt mode vt v >>=? fun (value, ctxt) ->
             return (Prim (-1, D_Elt, [ key ; value ], []) :: l, ctxt))
          ([], ctxt)
          (map_fold (fun k v acc -> (k, v) :: acc) map []) >>=? fun (items, ctxt) ->
        return (Micheline.Seq (-1, items), ctxt)
    | Big_map_t (_kt, _kv, _), _map ->
        return (Micheline.Seq (-1, []), ctxt)
    | Lambda_t _, Lam (_, original_code) ->
        unparse_code ctxt mode (root original_code)

and unparse_code ctxt mode = function
  | Prim (loc, I_PUSH, [ ty ; data ], annot) ->
      Lwt.return (parse_ty ~allow_big_map:false ~allow_operation:false ty) >>=? fun (Ex_ty t) ->
      parse_data ctxt t data >>=? fun (data, ctxt) ->
      unparse_data ctxt mode t data >>=? fun (data, ctxt) ->
      return (Prim (loc, I_PUSH, [ ty ; data ], annot), ctxt)
  | Seq (loc, items) ->
      fold_left_s
        (fun (l, ctxt) item ->
           unparse_code ctxt mode item >>=? fun (item, ctxt) ->
           return (item :: l, ctxt))
        ([], ctxt) items >>=? fun (items, ctxt) ->
      return (Micheline.Seq (loc, List.rev items), ctxt)
  | Prim (loc, prim, items, annot) ->
      fold_left_s
        (fun (l, ctxt) item ->
           unparse_code ctxt mode item >>=? fun (item, ctxt) ->
           return (item :: l, ctxt))
        ([], ctxt) items >>=? fun (items, ctxt) ->
      return (Prim (loc, prim, List.rev items, annot), ctxt)
  | Int _ | String _ as atom -> return (atom, ctxt)

let unparse_script ctxt mode { code ; arg_type ; storage ; storage_type } =
  let Lam (_, original_code) = code in
  unparse_code ctxt mode (root original_code) >>=? fun (code, ctxt) ->
  unparse_data ctxt mode storage_type storage >>=? fun (storage, ctxt) ->
  let arg_type = unparse_ty arg_type in
  let storage_type = unparse_ty storage_type in
  let open Micheline in
  let code =
    Seq (-1, [ Prim (-1, K_parameter, [ arg_type ], []) ;
               Prim (-1, K_storage, [ storage_type ], []) ;
               Prim (-1, K_code, [ code ], []) ]) in
  return ({ code = lazy_expr (strip_locations code) ;
            storage = lazy_expr (strip_locations storage) }, ctxt)

let hash_data ctxt typ data =
  unparse_data ctxt Optimized typ data >>=? fun (data, ctxt) ->
  let unparsed = strip_annotations @@ data in
  let bytes = Data_encoding.Binary.to_bytes_exn expr_encoding (Micheline.strip_locations unparsed) in
  return (Script_expr_hash.(hash_bytes [ bytes ] |> to_b58check), ctxt)

(* ---------------- Big map -------------------------------------------------*)

let big_map_mem ctxt contract key { diff ; key_type ; _ } =
  match map_get key diff with
  | None -> hash_data ctxt key_type key >>=? fun (hash, ctxt) ->
      Alpha_context.Contract.Big_map.mem ctxt contract hash >>=? fun (ctxt, res) ->
      return (res, ctxt)
  | Some None -> return (false, ctxt)
  | Some (Some _) -> return (true, ctxt)

let big_map_get ctxt contract key { diff ; key_type ; value_type } =
  match map_get key diff with
  | Some x -> return (x, ctxt)
  | None ->
      hash_data ctxt key_type key >>=? fun (hash, ctxt) ->
      Alpha_context.Contract.Big_map.get_opt
        ctxt contract hash >>=? begin function
        | (ctxt, None) -> return (None, ctxt)
        | (ctxt, Some value) ->
            parse_data ctxt value_type
              (Micheline.root value) >>=? fun (x, ctxt) ->
            return (Some x, ctxt)
      end

let big_map_update key value ({ diff ; _ } as map) =
  { map with diff = map_set key value diff }

let diff_of_big_map ctxt mode (Ex_bm { key_type ; value_type ; diff }) =
  Lwt.return (Gas.consume ctxt (Michelson_v1_gas.Cost_of.map_to_list diff)) >>=? fun ctxt ->
  let pairs = map_fold (fun key value acc -> (key, value) :: acc) diff [] in
  fold_left_s
    (fun (acc, ctxt) (key, value) ->
       Lwt.return (Gas.consume ctxt Typecheck_costs.cycle) >>=? fun ctxt ->
       hash_data ctxt key_type key >>=? fun (hash, ctxt) ->
       begin
         match value with
         | None -> return (None, ctxt)
         | Some x ->
             begin
               unparse_data ctxt mode value_type x >>=? fun (node, ctxt) ->
               return (Some (Micheline.strip_locations node), ctxt)
             end
       end >>=? fun (value, ctxt) ->
       return ((hash, value) :: acc, ctxt))
    ([], ctxt) pairs

(* Get the big map from a contract's storage if one exists *)
let extract_big_map : type a. a ty -> a -> ex_big_map option = fun ty x ->
  match (ty, x) with
  | Pair_t ((Big_map_t (_, _, _), _), _, _), (map, _) -> Some (Ex_bm map)
  | _, _ -> None

let erase_big_map_initialization ctxt mode ({ code ; storage } : Script.t) =
  Lwt.return (Script.force_decode code) >>=? fun code ->
  Lwt.return (Script.force_decode storage) >>=? fun storage ->
  Lwt.return @@ parse_toplevel code >>=? fun (_, storage_type, _) ->
  Lwt.return @@ parse_ty ~allow_big_map:true ~allow_operation:false storage_type >>=? fun (Ex_ty ty) ->
  parse_data ctxt ty
    (Micheline.root storage) >>=? fun (storage, ctxt) ->
  begin
    match extract_big_map ty storage with
    | None -> return (None, ctxt)
    | Some bm -> diff_of_big_map ctxt mode bm >>=? fun (bm, ctxt) ->
        return (Some bm, ctxt)
  end >>=? fun (bm, ctxt) ->
  unparse_data ctxt mode ty storage >>=? fun (storage, ctxt) ->
  return ({ code = Script.lazy_expr code ;
            storage = Script.lazy_expr (Micheline.strip_locations storage) }, bm, ctxt)
