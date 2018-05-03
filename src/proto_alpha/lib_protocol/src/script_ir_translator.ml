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

let add_dip ty annot prev =
  match prev with
  | Lambda | Toplevel _ -> Dip (Item_t (ty, Empty_t, annot), prev)
  | Dip (stack, _) -> Dip (Item_t (ty, stack, annot), prev)

let default_param_annot = Some "@parameter"
let default_storage_annot = Some "@storage"
let default_arg_annot = Some "@arg"

let default_annot ~default = function
  | None -> default
  | annot -> annot

(* ---- Type size accounting ------------------------------------------------*)

let comparable_type_size : type t. t comparable_ty -> int = function
  (* No wildcard to force the update when comparable_ty chages. *)
  | Int_key -> 1
  | Nat_key -> 1
  | String_key -> 1
  | Tez_key -> 1
  | Bool_key -> 1
  | Key_hash_key -> 1
  | Timestamp_key -> 1
  | Address_key -> 1

let rec type_size : type t. t ty -> int = function
  | Unit_t -> 1
  | Int_t -> 1
  | Nat_t -> 1
  | Signature_t -> 1
  | String_t -> 1
  | Tez_t -> 1
  | Key_hash_t -> 1
  | Key_t -> 1
  | Timestamp_t -> 1
  | Address_t -> 1
  | Bool_t -> 1
  | Operation_t -> 1
  | Pair_t ((l, _), (r, _)) ->
      1 + type_size l + type_size r
  | Union_t ((l, _), (r, _)) ->
      1 + type_size l + type_size r
  | Lambda_t (arg, ret) ->
      1 + type_size arg + type_size ret
  | Option_t t ->
      1 + type_size t
  | List_t t ->
      1 + type_size t
  | Set_t k ->
      1 + comparable_type_size k
  | Map_t (k, v) ->
      1 + comparable_type_size k + type_size v
  | Big_map_t (k, v) ->
      1 + comparable_type_size k + type_size v
  | Contract_t arg ->
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
  | List_map -> 1
  | List_map_body _ -> 1
  | List_reduce -> 0
  | List_size -> 0
  | List_iter _ -> 1
  | Empty_set _ -> 1
  | Set_reduce -> 0
  | Set_iter _ -> 0
  | Set_mem -> 0
  | Set_update -> 0
  | Set_size -> 0
  | Empty_map _ -> 1
  | Map_map -> 1
  | Map_reduce -> 0
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
  | Create_contract_literal _ -> 1
  | Now -> 0
  | Balance -> 0
  | Check_signature -> 0
  | Hash_key -> 0
  | H _ -> 0
  | Steps_to_quota -> 0
  | Source -> 0
  | Self _ -> 1
  | Amount -> 0

(* ---- Error helpers -------------------------------------------------------*)

let location = function
  | Prim (loc, _, _, _)
  | Int (loc, _)
  | String (loc, _)
  | Seq (loc, _, _) -> loc

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
  | I_REDUCE
  | I_RIGHT
  | I_SIZE
  | I_SOME
  | I_SOURCE
  | I_SELF
  | I_STEPS_TO_QUOTA
  | I_SUB
  | I_SWAP
  | I_TRANSFER_TOKENS
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
  | T_tez
  | T_timestamp
  | T_unit
  | T_operation
  | T_address -> Type_namespace


let unexpected expr exp_kinds exp_ns exp_prims =
  match expr with
  | Int (loc, _) -> Invalid_kind (loc, Prim_kind :: exp_kinds, Int_kind)
  | String (loc, _ ) -> Invalid_kind (loc, Prim_kind :: exp_kinds, String_kind)
  | Seq (loc, _, _) -> Invalid_kind (loc, Prim_kind :: exp_kinds, Seq_kind)
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
    | String_key -> Compare.String.compare x y
    | Bool_key -> Compare.Bool.compare x y
    | Tez_key -> Tez.compare x y
    | Key_hash_key -> Signature.Public_key_hash.compare x y
    | Int_key ->
        let res = (Script_int.compare x y) in
        if Compare.Int.(res = 0) then 0
        else if Compare.Int.(res > 0) then 1
        else -1

    | Nat_key ->
        let res = (Script_int.compare x y) in
        if Compare.Int.(res = 0) then 0
        else if Compare.Int.(res > 0) then 1
        else -1
    | Timestamp_key -> Script_timestamp.compare x y
    | Address_key -> Contract.compare x y

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

(* ---- Unparsing (Typed IR -> Untyped expressions) --------------------------*)

let ty_of_comparable_ty
  : type a. a comparable_ty -> a ty = function
  | Int_key -> Int_t
  | Nat_key -> Nat_t
  | String_key -> String_t
  | Tez_key -> Tez_t
  | Bool_key -> Bool_t
  | Key_hash_key -> Key_hash_t
  | Timestamp_key -> Timestamp_t
  | Address_key -> Address_t

let unparse_comparable_ty
  : type a. a comparable_ty -> Script.node = function
  | Int_key -> Prim (-1, T_int, [], None)
  | Nat_key -> Prim (-1, T_nat, [], None)
  | String_key -> Prim (-1, T_string, [], None)
  | Tez_key -> Prim (-1, T_tez, [], None)
  | Bool_key -> Prim (-1, T_bool, [], None)
  | Key_hash_key -> Prim (-1, T_key_hash, [], None)
  | Timestamp_key -> Prim (-1, T_timestamp, [], None)
  | Address_key -> Prim (-1, T_address, [], None)

let rec unparse_ty
  : type a. annot -> a ty -> Script.node = fun annot ->
  function
  | Unit_t -> Prim (-1, T_unit, [], annot)
  | Int_t -> Prim (-1, T_int, [], annot)
  | Nat_t -> Prim (-1, T_nat, [], annot)
  | String_t -> Prim (-1, T_string, [], annot)
  | Tez_t -> Prim (-1, T_tez, [], annot)
  | Bool_t -> Prim (-1, T_bool, [], annot)
  | Key_hash_t -> Prim (-1, T_key_hash, [], annot)
  | Key_t -> Prim (-1, T_key, [], annot)
  | Timestamp_t -> Prim (-1, T_timestamp, [], annot)
  | Address_t -> Prim (-1, T_address, [], annot)
  | Signature_t -> Prim (-1, T_signature, [], annot)
  | Operation_t -> Prim (-1, T_operation, [], annot)
  | Contract_t ut ->
      let t = unparse_ty None ut in
      Prim (-1, T_contract, [ t ], annot)
  | Pair_t ((utl, left_annot), (utr, right_annot)) ->
      let tl = unparse_ty left_annot utl in
      let tr = unparse_ty right_annot utr in
      Prim (-1, T_pair, [ tl; tr ], annot)
  | Union_t ((utl, left_annot), (utr, right_annot)) ->
      let tl = unparse_ty left_annot utl in
      let tr = unparse_ty right_annot utr in
      Prim (-1, T_or, [ tl; tr ], annot)
  | Lambda_t (uta, utr) ->
      let ta = unparse_ty None uta in
      let tr = unparse_ty None utr in
      Prim (-1, T_lambda, [ ta; tr ], annot)
  | Option_t ut ->
      let t = unparse_ty None ut in
      Prim (-1, T_option, [ t ], annot)
  | List_t ut ->
      let t = unparse_ty None ut in
      Prim (-1, T_list, [ t ], annot)
  | Set_t ut ->
      let t = unparse_comparable_ty ut in
      Prim (-1, T_set, [ t ], None)
  | Map_t (uta, utr) ->
      let ta = unparse_comparable_ty uta in
      let tr = unparse_ty None utr in
      Prim (-1, T_map, [ ta; tr ], None)
  | Big_map_t (uta, utr) ->
      let ta = unparse_comparable_ty uta in
      let tr = unparse_ty None utr in
      Prim (-1, T_big_map, [ ta; tr ], None)

module Unparse_costs = Michelson_v1_gas.Cost_of.Unparse

let rec unparse_data
  : type a. context -> a ty -> a -> (Script.node * context) tzresult
  = fun ctxt ty a ->
    Gas.consume ctxt Unparse_costs.cycle >>? fun gas ->
    match ty, a with
    | Unit_t, () ->
        Gas.consume ctxt Unparse_costs.unit >|? fun gas ->
        (Prim (-1, D_Unit, [], None), gas)
    | Int_t, v ->
        Gas.consume ctxt (Unparse_costs.int v) >|? fun gas ->
        (Int (-1, Script_int.to_zint v), gas)
    | Nat_t, v ->
        Gas.consume ctxt (Unparse_costs.int v) >|? fun gas ->
        (Int (-1, Script_int.to_zint v), gas)
    | String_t, s ->
        Gas.consume ctxt (Unparse_costs.string s) >|? fun gas ->
        (String (-1, s), gas)
    | Bool_t, true ->
        Gas.consume ctxt Unparse_costs.bool >|? fun gas ->
        (Prim (-1, D_True, [], None), gas)
    | Bool_t, false ->
        Gas.consume ctxt Unparse_costs.bool >|? fun gas ->
        (Prim (-1, D_False, [], None), gas)
    | Timestamp_t, t ->
        Gas.consume ctxt (Unparse_costs.timestamp t) >>? fun gas ->
        begin
          match Script_timestamp.to_notation t with
          | None -> ok @@ (Int (-1, Script_timestamp.to_zint t), gas)
          | Some s -> ok @@ (String (-1, s), gas)
        end
    | Address_t, c  ->
        Gas.consume ctxt Unparse_costs.contract >|? fun gas ->
        (String (-1, Contract.to_b58check c), gas)
    | Contract_t _, (_, c)  ->
        Gas.consume ctxt Unparse_costs.contract >|? fun gas ->
        (String (-1, Contract.to_b58check c), gas)
    | Signature_t, s ->
        Gas.consume ctxt Unparse_costs.signature >|? fun gas ->
        let `Hex text =
          MBytes.to_hex
            (Data_encoding.Binary.to_bytes_exn Signature.encoding s) in
        (String (-1, text), gas)
    | Tez_t, v ->
        Gas.consume ctxt Unparse_costs.tez >|? fun gas ->
        (String (-1, Tez.to_string v), gas)
    | Key_t, k ->
        Gas.consume ctxt Unparse_costs.key >|? fun gas ->
        (String (-1, Signature.Public_key.to_b58check k), gas)
    | Key_hash_t, k ->
        Gas.consume ctxt Unparse_costs.key_hash >|? fun gas ->
        (String (-1, Signature.Public_key_hash.to_b58check k), gas)
    | Operation_t, op ->
        let bytes = Data_encoding.Binary.to_bytes_exn Operation.internal_operation_encoding op in
        let `Hex text = MBytes.to_hex bytes in
        Gas.consume ctxt (Unparse_costs.operation bytes) >>? fun ctxt ->
        ok (String (-1, text), ctxt)
    | Pair_t ((tl, _), (tr, _)), (l, r) ->
        Gas.consume ctxt Unparse_costs.pair >>? fun gas ->
        unparse_data gas tl l >>? fun (l, gas) ->
        unparse_data gas tr r >|? fun (r, gas) ->
        (Prim (-1, D_Pair, [ l; r ], None), gas)
    | Union_t ((tl, _), _), L l ->
        Gas.consume ctxt Unparse_costs.union >>? fun gas ->
        unparse_data gas tl l >|? fun (l, gas) ->
        (Prim (-1, D_Left, [ l ], None), gas)
    | Union_t (_, (tr, _)), R r ->
        Gas.consume ctxt Unparse_costs.union >>? fun gas ->
        unparse_data gas tr r >|? fun (r, gas) ->
        (Prim (-1, D_Right, [ r ], None), gas)
    | Option_t t, Some v ->
        Gas.consume ctxt Unparse_costs.some >>? fun gas ->
        unparse_data gas t v >|? fun (v, gas) ->
        (Prim (-1, D_Some, [ v ], None), gas)
    | Option_t _, None ->
        Gas.consume ctxt Unparse_costs.none >|? fun gas ->
        (Prim (-1, D_None, [], None), gas)
    | List_t t, items ->
        List.fold_right
          (fun element acc ->
             acc >>? fun (l, ctxt) ->
             Gas.consume ctxt Unparse_costs.list_element >>? fun ctxt ->
             unparse_data ctxt t element >>? fun (unparsed, ctxt) ->
             ok (unparsed :: l, ctxt))
          items
          (ok ([], ctxt)) >|? fun (items, gas) ->
        (Micheline.Seq (-1, items, None), gas)
    | Set_t t, set ->
        let t = ty_of_comparable_ty t in
        set_fold
          (fun item acc ->
             acc >>? fun (l, ctxt) ->
             Gas.consume ctxt Unparse_costs.set_element >>? fun ctxt ->
             unparse_data ctxt t item >>? fun (item, ctxt) ->
             ok (item :: l, ctxt))
          set (ok ([], ctxt)) >|? fun (items, gas) ->
        (Micheline.Seq (-1, List.rev items, None), gas)
    | Map_t (kt, vt), map ->
        let kt = ty_of_comparable_ty kt in
        map_fold
          (fun k v acc ->
             acc >>? fun (l, ctxt) ->
             Gas.consume ctxt Unparse_costs.map_element >>? fun ctxt ->
             unparse_data ctxt kt k >>? fun (key, ctxt) ->
             unparse_data ctxt vt v >>? fun (value, ctxt) ->
             ok (Prim (-1, D_Elt, [ key ; value ], None) :: l, ctxt))
          map (ok ([], ctxt)) >|? fun (items, gas) ->
        (Micheline.Seq (-1, List.rev items, None), gas)
    | Big_map_t (_kt, _kv), _map ->
        ok (Micheline.Seq (-1, [], None), gas)
    | Lambda_t _, Lam (_, original_code) ->
        ok (root original_code, gas)

(* ---- Equality witnesses --------------------------------------------------*)

type ('ta, 'tb) eq = Eq : ('same, 'same) eq

let comparable_ty_eq
  : type ta tb.
    ta comparable_ty -> tb comparable_ty ->
    (ta comparable_ty, tb comparable_ty) eq tzresult
  = fun ta tb -> match ta, tb with
    | Int_key, Int_key -> Ok Eq
    | Nat_key, Nat_key -> Ok Eq
    | String_key, String_key -> Ok Eq
    | Tez_key, Tez_key -> Ok Eq
    | Bool_key, Bool_key -> Ok Eq
    | Key_hash_key, Key_hash_key -> Ok Eq
    | Timestamp_key, Timestamp_key -> Ok Eq
    | Address_key, Address_key -> Ok Eq
    | _, _ -> error (Inconsistent_types (ty_of_comparable_ty ta, ty_of_comparable_ty tb))

let rec ty_eq
  : type ta tb. ta ty -> tb ty -> (ta ty, tb ty) eq tzresult
  = fun ta tb ->
    match ta, tb with
    | Unit_t, Unit_t -> Ok Eq
    | Int_t, Int_t -> Ok Eq
    | Nat_t, Nat_t -> Ok Eq
    | Key_t, Key_t -> Ok Eq
    | Key_hash_t, Key_hash_t -> Ok Eq
    | String_t, String_t -> Ok Eq
    | Signature_t, Signature_t -> Ok Eq
    | Tez_t, Tez_t -> Ok Eq
    | Timestamp_t, Timestamp_t -> Ok Eq
    | Address_t, Address_t -> Ok Eq
    | Bool_t, Bool_t -> Ok Eq
    | Operation_t, Operation_t -> Ok Eq
    | Map_t (tal, tar), Map_t (tbl, tbr) ->
        (comparable_ty_eq tal tbl >>? fun Eq ->
         ty_eq tar tbr >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Big_map_t (tal, tar), Big_map_t (tbl, tbr) ->
        (comparable_ty_eq tal tbl >>? fun Eq ->
         ty_eq tar tbr >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Set_t ea, Set_t eb ->
        (comparable_ty_eq ea eb >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Pair_t ((tal, _), (tar, _)),
      Pair_t ((tbl, _), (tbr, _)) ->
        (ty_eq tal tbl >>? fun Eq ->
         ty_eq tar tbr >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Union_t ((tal, _), (tar, _)), Union_t ((tbl, _), (tbr, _)) ->
        (ty_eq tal tbl >>? fun Eq ->
         ty_eq tar tbr >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Lambda_t (tal, tar), Lambda_t (tbl, tbr) ->
        (ty_eq tal tbl >>? fun Eq ->
         ty_eq tar tbr >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Contract_t tal, Contract_t tbl ->
        (ty_eq tal tbl >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Option_t tva, Option_t tvb ->
        (ty_eq tva tvb >>? fun Eq ->
         (Ok Eq : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | List_t tva, List_t tvb ->
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

let merge_annot annot1 annot2 =
  match annot1, annot2 with
  | None, None
  | Some _, None
  | None, Some _ -> ok None
  | Some annot1, Some annot2 ->
      if String.equal annot1 annot2
      then ok (Some annot1)
      else error (Inconsistent_annotations (annot1, annot2))

let merge_comparable_types
  : type ta.
    ta comparable_ty -> ta comparable_ty ->
    ta comparable_ty
  = fun ta tb -> match ta, tb with
    | Int_key, Int_key -> ta
    | Nat_key, Nat_key -> ta
    | String_key, String_key -> ta
    | Tez_key, Tez_key -> ta
    | Bool_key, Bool_key -> ta
    | Key_hash_key, Key_hash_key -> ta
    | Timestamp_key, Timestamp_key -> ta
    | Address_key, Address_key -> ta
    | _, _ -> assert false (* FIXME: fix injectivity of some types *)

let error_unexpected_annot loc annot =
  match annot with
  | None -> ok ()
  | Some _ -> error (Unexpected_annotation loc)

let rec strip_annotations = function
  | (Int (_,_) as i) -> i
  | (String (_,_) as s) -> s
  | Prim (loc, prim, args, _) -> Prim (loc, prim, List.map strip_annotations args, None)
  | Seq (loc, items, _) -> Seq (loc, List.map strip_annotations items, None)

let fail_unexpected_annot loc annot =
  Lwt.return (error_unexpected_annot loc annot)

let merge_types :
  type b.Script.location -> b ty -> b ty -> b ty tzresult =
  let rec help : type a.a ty -> a ty -> a ty tzresult
    = fun ty1 ty2 ->
      match ty1, ty2 with
      | Unit_t, Unit_t -> ok Unit_t
      | Int_t, Int_t -> ok Int_t
      | Nat_t, Nat_t -> ok Nat_t
      | Key_t, Key_t -> ok Key_t
      | Key_hash_t, Key_hash_t -> ok Key_hash_t
      | String_t, String_t -> ok String_t
      | Signature_t, Signature_t -> ok Signature_t
      | Tez_t, Tez_t -> ok Tez_t
      | Timestamp_t, Timestamp_t -> ok Timestamp_t
      | Address_t, Address_t -> ok Address_t
      | Bool_t, Bool_t -> ok Bool_t
      | Operation_t, Operation_t -> ok Operation_t
      | Map_t (tal, tar), Map_t (tbl, tbr) ->
          help tar tbr >>? fun value ->
          ty_eq tar value >>? fun Eq ->
          ok (Map_t (merge_comparable_types tal tbl, value))
      | Big_map_t (tal, tar), Big_map_t (tbl, tbr) ->
          help tar tbr >>? fun value ->
          ty_eq tar value >>? fun Eq ->
          ok (Big_map_t (merge_comparable_types tal tbl, value))
      | Set_t ea, Set_t eb ->
          ok (Set_t (merge_comparable_types ea eb))
      | Pair_t ((tal, left_annot1), (tar, right_annot1)),
        Pair_t ((tbl, left_annot2), (tbr, right_annot2)) ->
          merge_annot left_annot1 left_annot2 >>? fun left_annot ->
          merge_annot right_annot1 right_annot2 >>? fun right_annot ->
          help tal tbl >>? fun left_ty ->
          help tar tbr >|? fun right_ty ->
          Pair_t ((left_ty, left_annot), (right_ty, right_annot))
      | Union_t ((tal, tal_annot), (tar, tar_annot)),
        Union_t ((tbl, tbl_annot), (tbr, tbr_annot)) ->
          merge_annot tal_annot tbl_annot >>? fun left_annot ->
          merge_annot tar_annot tbr_annot >>? fun right_annot ->
          help tal tbl >>? fun left_ty ->
          help tar tbr >|? fun right_ty ->
          Union_t ((left_ty, left_annot), (right_ty, right_annot))
      | Lambda_t (tal, tar), Lambda_t (tbl, tbr) ->
          help tal tbl >>? fun left_ty ->
          help tar tbr >|? fun right_ty ->
          Lambda_t (left_ty, right_ty)
      | Contract_t tal, Contract_t tbl ->
          help tal tbl >|? fun arg_ty ->
          Contract_t arg_ty
      | Option_t tva, Option_t tvb ->
          help tva tvb >|? fun ty ->
          Option_t ty
      | List_t tva, List_t tvb ->
          help tva tvb >|? fun ty ->
          List_t ty
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
            merge_annot annot1 annot2 >>? fun annot ->
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

let rec parse_comparable_ty
  : Script.node -> ex_comparable_ty tzresult
  = function
    | Prim (_, T_int, [], _) -> ok (Ex_comparable_ty Int_key)
    | Prim (_, T_nat, [], _) -> ok (Ex_comparable_ty Nat_key)
    | Prim (_, T_string, [], _) -> ok (Ex_comparable_ty String_key)
    | Prim (_, T_tez, [], _) -> ok (Ex_comparable_ty Tez_key)
    | Prim (_, T_bool, [], _) -> ok (Ex_comparable_ty Bool_key)
    | Prim (_, T_key_hash, [], _) -> ok (Ex_comparable_ty Key_hash_key)
    | Prim (_, T_timestamp, [], _) -> ok (Ex_comparable_ty Timestamp_key)
    | Prim (_, T_address, [], _) -> ok (Ex_comparable_ty Address_key)
    | Prim (loc, (T_int | T_nat
                 | T_string | T_tez | T_bool
                 | T_key | T_address | T_timestamp as prim), l, _) ->
        error (Invalid_arity (loc, prim, 0, List.length l))
    | Prim (loc, (T_pair | T_or | T_set | T_map
                 | T_list | T_option  | T_lambda
                 | T_unit | T_signature  | T_contract), _, _) as expr ->
        parse_ty ~allow_big_map:false expr >>? fun (Ex_ty ty, _) ->
        error (Comparable_type_expected (loc, ty))
    | expr ->
        error @@ unexpected expr [] Type_namespace
          [ T_int ; T_nat ;
            T_string ; T_tez ; T_bool ;
            T_key ; T_key_hash ; T_timestamp ]

and parse_ty
  : allow_big_map: bool -> Script.node -> (ex_ty * annot) tzresult
  = fun ~allow_big_map node ->
    match node with
    | Prim (_, T_pair,
            [ Prim (big_map_loc, T_big_map, args, map_annot) ; remaining_storage ],
            storage_annot)
      when allow_big_map ->
        begin match args with
          | [ key_ty ; value_ty ] ->
              parse_comparable_ty key_ty >>? fun (Ex_comparable_ty key_ty) ->
              parse_ty ~allow_big_map:false value_ty >>? fun (Ex_ty value_ty, right_annot) ->
              error_unexpected_annot big_map_loc right_annot >>? fun () ->
              parse_ty ~allow_big_map:false remaining_storage >>? fun (Ex_ty remaining_storage, remaining_annot) ->
              ok (Ex_ty (Pair_t ((Big_map_t (key_ty, value_ty), map_annot),
                                 (remaining_storage, remaining_annot))),
                  storage_annot)
          | args -> error @@ Invalid_arity (big_map_loc, T_big_map, 2, List.length args)
        end
    | Prim (_, T_unit, [], annot) ->
        ok (Ex_ty Unit_t, annot)
    | Prim (_, T_int, [], annot) ->
        ok (Ex_ty Int_t, annot)
    | Prim (_, T_nat, [], annot) ->
        ok (Ex_ty Nat_t, annot)
    | Prim (_, T_string, [], annot) ->
        ok (Ex_ty String_t, annot)
    | Prim (_, T_tez, [], annot) ->
        ok (Ex_ty Tez_t, annot)
    | Prim (_, T_bool, [], annot) ->
        ok (Ex_ty Bool_t, annot)
    | Prim (_, T_key, [], annot) ->
        ok (Ex_ty Key_t, annot)
    | Prim (_, T_key_hash, [], annot) ->
        ok (Ex_ty Key_hash_t, annot)
    | Prim (_, T_timestamp, [], annot) ->
        ok (Ex_ty Timestamp_t, annot)
    | Prim (_, T_address, [], annot) ->
        ok (Ex_ty Address_t, annot)
    | Prim (_, T_signature, [], annot) ->
        ok (Ex_ty Signature_t, annot)
    | Prim (_, T_operation, [], annot) ->
        ok (Ex_ty Operation_t, annot)
    | Prim (loc, T_contract, [ utl ], annot) ->
        parse_ty ~allow_big_map:false utl >>? fun (Ex_ty tl, left_annot) ->
        error_unexpected_annot loc left_annot >|? fun () ->
        (Ex_ty (Contract_t tl), annot)
    | Prim (_, T_pair, [ utl; utr ], annot) ->
        parse_ty ~allow_big_map:false utl >>? fun (Ex_ty tl, left_annot) ->
        parse_ty ~allow_big_map:false utr >|? fun (Ex_ty tr, right_annot) ->
        (Ex_ty (Pair_t ((tl, left_annot), (tr, right_annot))), annot)
    | Prim (_, T_or, [ utl; utr ], annot) ->
        parse_ty ~allow_big_map:false utl >>? fun (Ex_ty tl, left_annot) ->
        parse_ty ~allow_big_map:false utr >|? fun (Ex_ty tr, right_annot) ->
        (Ex_ty (Union_t ((tl, left_annot), (tr, right_annot))), annot)
    | Prim (_, T_lambda, [ uta; utr ], annot) ->
        parse_ty ~allow_big_map:false uta >>? fun (Ex_ty ta, _) ->
        parse_ty ~allow_big_map:false utr >|? fun (Ex_ty tr, _) ->
        (Ex_ty (Lambda_t (ta, tr)), annot)
    | Prim (loc, T_option, [ ut ], annot) ->
        parse_ty ~allow_big_map:false ut >>? fun (Ex_ty t, opt_annot) ->
        error_unexpected_annot loc annot >|? fun () ->
        (Ex_ty (Option_t t), opt_annot)
    | Prim (loc, T_list, [ ut ], annot) ->
        parse_ty ~allow_big_map:false ut >>? fun (Ex_ty t, list_annot) ->
        error_unexpected_annot loc list_annot >>? fun () ->
        ok (Ex_ty (List_t t), annot)
    | Prim (_, T_set, [ ut ], annot) ->
        parse_comparable_ty ut >>? fun (Ex_comparable_ty t) ->
        ok (Ex_ty (Set_t t), annot)
    | Prim (_, T_map, [ uta; utr ], annot) ->
        parse_comparable_ty uta >>? fun (Ex_comparable_ty ta) ->
        parse_ty ~allow_big_map:false utr >>? fun (Ex_ty tr, _) ->
        ok (Ex_ty (Map_t (ta, tr)), annot)
    | Prim (loc, T_big_map, _, _) ->
        error (Unexpected_big_map loc)
    | Prim (loc, (T_unit | T_signature
                 | T_int | T_nat
                 | T_string | T_tez | T_bool
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
            T_string ; T_tez ; T_bool ;
            T_key ; T_key_hash ; T_timestamp ]

let rec unparse_stack
  : type a. a stack_ty -> Script.expr list
  = function
    | Empty_t -> []
    | Item_t (ty, rest, annot) -> strip_locations (unparse_ty annot ty) :: unparse_stack rest

type ex_script = Ex_script : ('a, 'c) script -> ex_script

let rec parse_data
  : type a.
    ?type_logger: (int -> Script.expr list -> Script.expr list -> unit) ->
    context -> check_operations: bool -> a ty -> Script.node -> (a * context) tzresult Lwt.t
  = fun  ?type_logger ctxt ~check_operations ty script_data ->
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
               parse_data ?type_logger ctxt ~check_operations value_type v >>=? fun (v, ctxt) ->
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
    match ty, script_data with
    (* Unit *)
    | Unit_t, Prim (_, D_Unit, [], _) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.unit) >>|? fun ctxt ->
        ((() : a), ctxt)
    | Unit_t, Prim (loc, D_Unit, l, _) ->
        traced (fail (Invalid_arity (loc, D_Unit, 0, List.length l)))
    | Unit_t, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Unit ]))
    (* Booleans *)
    | Bool_t, Prim (_, D_True, [], _) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.bool) >>|? fun ctxt ->
        (true, ctxt)
    | Bool_t, Prim (_, D_False, [], _) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.bool) >>|? fun ctxt ->
        (false, ctxt)
    | Bool_t, Prim (loc, (D_True | D_False as c), l, _) ->
        traced (fail (Invalid_arity (loc, c, 0, List.length l)))
    | Bool_t, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_True ; D_False ]))
    (* Strings *)
    | String_t, String (_, v) ->
        Lwt.return (Gas.consume ctxt (Typecheck_costs.string (String.length v))) >>|? fun ctxt ->
        (v, ctxt)
    | String_t, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Integers *)
    | Int_t, Int (_, v) ->
        return (Script_int.of_zint v, ctxt)
    | Nat_t, Int (_, v) ->
        let v = Script_int.of_zint v in
        if Compare.Int.(Script_int.compare v Script_int.zero >= 0) then
          return (Script_int.abs v, ctxt)
        else fail (error ())
    | Int_t, expr ->
        traced (fail (Invalid_kind (location expr, [ Int_kind ], kind expr)))
    | Nat_t, expr ->
        traced (fail (Invalid_kind (location expr, [ Int_kind ], kind expr)))
    (* Tez amounts *)
    | Tez_t, String (_, v) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.tez) >>=? fun ctxt ->
        begin try
            match Tez.of_string v with
            | None -> raise Exit
            | Some tez -> return (tez, ctxt)
          with _ ->
            fail @@ error ()
        end
    | Tez_t, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Timestamps *)
    | Timestamp_t, (Int (_, v)) ->
        return (Script_timestamp.of_zint v, ctxt)
    | Timestamp_t, String (_, s) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.string_timestamp) >>=? fun ctxt ->
        begin try
            match Script_timestamp.of_string s with
            | Some v -> return (v, ctxt)
            | None -> fail (error ())
          with _ -> fail (error ())
        end
    | Timestamp_t, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ; Int_kind ], kind expr)))
    (* IDs *)
    | Key_t, String (_, s) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.key) >>=? fun ctxt ->
        begin
          try
            return (Signature.Public_key.of_b58check_exn s, ctxt)
          with _ -> fail (error ())
        end
    | Key_t, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    | Key_hash_t, String (_, s) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.key_hash) >>=? fun ctxt ->
        begin
          try
            return (Signature.Public_key_hash.of_b58check_exn s, ctxt)
          with _ -> fail (error ()) end
    | Key_hash_t, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Signatures *)
    | Signature_t, String (_, s) -> begin try
          Lwt.return (Gas.consume ctxt Typecheck_costs.signature) >>=? fun ctxt ->
          match Data_encoding.Binary.of_bytes
                  Signature.encoding
                  (MBytes.of_hex (`Hex s)) with
          | Some s -> return (s, ctxt)
          | None -> raise Not_found
        with _ ->
          fail (error ())
      end
    | Signature_t, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Operations *)
    | Operation_t, String (_, s) -> begin try
          Lwt.return (Gas.consume ctxt (Typecheck_costs.operation s)) >>=? fun ctxt ->
          match Data_encoding.Binary.of_bytes
                  Operation.internal_operation_encoding
                  (MBytes.of_hex (`Hex s)) with
          | Some op ->
              begin match check_operations,  op.signature with
                | true, None -> fail (error ())
                | false, _ -> return (op, ctxt)
                | true, Some signature ->
                    let unsigned =
                      Data_encoding.Binary.to_bytes_exn
                        Operation.internal_operation_encoding
                        { op with signature = None } in
                    Contract.get_manager_key ctxt op.source >>=? fun public_key ->
                    if Signature.check public_key signature unsigned then
                      return (op, ctxt)
                    else
                      fail (error ())
              end
          | None -> raise Not_found
        with _ ->
          fail (error ())
      end
    | Operation_t, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Addresses *)
    | Address_t, String (_, s) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.contract) >>=? fun ctxt ->
        traced @@
        (Lwt.return (Contract.of_b58check s)) >>=? fun c ->
        return (c, ctxt)
    | Address_t, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Contracts *)
    | Contract_t ty1, String (loc, s) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.contract) >>=? fun ctxt ->
        traced @@
        (Lwt.return (Contract.of_b58check s)) >>=? fun c ->
        parse_contract ctxt ty1 loc c >>=? fun _ ->
        return ((ty1, c), ctxt)
    | Contract_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Pairs *)
    | Pair_t ((ta, _), (tb, _)), Prim (_, D_Pair, [ va; vb ], _) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.pair) >>=? fun ctxt ->
        traced @@
        parse_data ?type_logger ctxt ~check_operations ta va >>=? fun (va, ctxt) ->
        parse_data ?type_logger ctxt ~check_operations tb vb >>=? fun (vb, ctxt) ->
        return ((va, vb), ctxt)
    | Pair_t _, Prim (loc, D_Pair, l, _) ->
        fail @@ Invalid_arity (loc, D_Pair, 2, List.length l)
    | Pair_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Pair ]))
    (* Unions *)
    | Union_t ((tl, _), _), Prim (_, D_Left, [ v ], _) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.union) >>=? fun ctxt ->
        traced @@
        parse_data ?type_logger ctxt ~check_operations tl v >>=? fun (v, ctxt) ->
        return (L v, ctxt)
    | Union_t _, Prim (loc, D_Left, l, _) ->
        fail @@ Invalid_arity (loc, D_Left, 1, List.length l)
    | Union_t (_, (tr, _)), Prim (_, D_Right, [ v ], _) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.union) >>=? fun ctxt ->
        traced @@
        parse_data ?type_logger ctxt ~check_operations tr v >>=? fun (v, ctxt) ->
        return (R v, ctxt)
    | Union_t _, Prim (loc, D_Right, l, _) ->
        fail @@ Invalid_arity (loc, D_Right, 1, List.length l)
    | Union_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Left ; D_Right ]))
    (* Lambdas *)
    | Lambda_t (ta, tr), (Seq _ as script_instr) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.lambda) >>=? fun ctxt ->
        traced @@
        parse_returning Lambda ?type_logger ~check_operations ctxt (ta, Some "@arg") tr script_instr
    | Lambda_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))
    (* Options *)
    | Option_t t, Prim (_, D_Some, [ v ], _) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.some) >>=? fun ctxt ->
        traced @@
        parse_data ?type_logger ctxt ~check_operations t v >>=? fun (v, ctxt) ->
        return (Some v, ctxt)
    | Option_t _, Prim (loc, D_Some, l, _) ->
        fail @@ Invalid_arity (loc, D_Some, 1, List.length l)
    | Option_t _, Prim (_, D_None, [], _) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.none) >>=? fun ctxt ->
        return (None, ctxt)
    | Option_t _, Prim (loc, D_None, l, _) ->
        fail @@ Invalid_arity (loc, D_None, 0, List.length l)
    | Option_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Some ; D_None ]))
    (* Lists *)
    | List_t t, Seq (loc, items, annot) ->
        fail_unexpected_annot loc annot >>=? fun () ->
        traced @@
        fold_right_s
          (fun v (rest, ctxt) ->
             Lwt.return (Gas.consume ctxt Typecheck_costs.list_element) >>=? fun ctxt ->
             parse_data ?type_logger ctxt  ~check_operations t v >>=? fun (v, ctxt) ->
             return ((v :: rest), ctxt))
          items ([], ctxt)
    | List_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))
    (* Sets *)
    | Set_t t, (Seq (loc, vs, annot) as expr) ->
        fail_unexpected_annot loc annot >>=? fun () ->
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
    | Map_t (tk, tv), (Seq (loc, vs, annot) as expr) ->
        fail_unexpected_annot loc annot >>=? fun () ->
        parse_items ?type_logger loc ctxt expr tk tv vs (fun x -> x)
    | Map_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))
    | Big_map_t (tk, tv), (Seq (loc, vs, annot) as expr) ->
        fail_unexpected_annot loc annot >>=? fun () ->
        parse_items ?type_logger loc ctxt expr tk tv vs (fun x -> Some x) >>|? fun (diff, ctxt) ->
        ({ diff ; key_type = ty_of_comparable_ty tk ; value_type = tv }, ctxt)
    | Big_map_t (_tk, _tv), expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))

and parse_comparable_data
  : type a.
    ?type_logger:(int -> Script.expr list -> Script.expr list -> unit) ->
    context -> a comparable_ty -> Script.node -> (a * context) tzresult Lwt.t
  = fun ?type_logger ctxt ty script_data ->
    parse_data ?type_logger ctxt ~check_operations:false (ty_of_comparable_ty ty) script_data

and parse_returning
  : type arg ret.
    ?type_logger: (int -> Script.expr list -> Script.expr list -> unit) ->
    tc_context -> context ->
    check_operations: bool ->
  arg ty * annot -> ret ty -> Script.node -> ((arg, ret) lambda * context) tzresult Lwt.t =
  fun ?type_logger tc_context ctxt ~check_operations (arg, arg_annot) ret script_instr ->
    parse_instr ?type_logger tc_context ctxt ~check_operations
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
    ?type_logger: (int -> Script.expr list -> Script.expr list -> unit) ->
    tc_context -> context ->
    check_operations: bool ->
  Script.node -> bef stack_ty -> (bef judgement * context) tzresult Lwt.t =
  fun ?type_logger tc_context ctxt ~check_operations script_instr stack_ty ->
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
    let keep_or_rewrite_annot value_annot instr_annot =
      match value_annot, instr_annot with
      | annot, None -> annot
      | _, annot -> annot in
    let check_item check loc name n m =
      trace (Bad_stack (loc, name, m, stack_ty)) @@
      trace (Bad_stack_item n) @@
      Lwt.return check in
    let check_item_ty exp got loc n =
      check_item (ty_eq exp got) loc n in
    let typed ctxt loc instr aft =
      begin match type_logger, script_instr with
        | None, _
        | Some _, (Seq (-1, _, _) | Int _ | String _) -> ()
        | Some log, (Prim _ | Seq _) ->
            log loc (unparse_stack stack_ty) (unparse_stack aft)
      end ;
      return ctxt (Typed { loc ; instr ; bef = stack_ty ; aft }) in
    match script_instr, stack_ty with
    (* stack ops *)
    | Prim (loc, I_DROP, [], _),
      Item_t (_, rest, _) ->
        typed ctxt loc Drop
          rest
    | Prim (loc, I_DUP, [], instr_annot),
      Item_t (v, rest, stack_annot) ->
        let annot = keep_or_rewrite_annot stack_annot instr_annot in
        typed ctxt loc Dup
          (Item_t (v, Item_t (v, rest, stack_annot), annot))
    | Prim (loc, I_SWAP, [], instr_annot),
      Item_t (v,  Item_t (w, rest, stack_annot), cur_top_annot) ->
        let annot = keep_or_rewrite_annot stack_annot instr_annot in
        typed ctxt loc Swap
          (Item_t (w, Item_t (v, rest, cur_top_annot), annot))
    | Prim (loc, I_PUSH, [ t ; d ], instr_annot),
      stack ->
        (Lwt.return (parse_ty ~allow_big_map:false t)) >>=? fun (Ex_ty t, _) ->
        parse_data ?type_logger ctxt ~check_operations t d >>=? fun (v, ctxt) ->
        typed ctxt loc (Const v)
          (Item_t (t, stack, instr_annot))
    | Prim (loc, I_UNIT, [], instr_annot),
      stack ->
        typed ctxt loc (Const ())
          (Item_t (Unit_t, stack, instr_annot))
    (* options *)
    | Prim (loc, I_SOME, [], instr_annot),
      Item_t (t, rest, _) ->
        typed ctxt loc Cons_some
          (Item_t (Option_t t, rest, instr_annot))
    | Prim (loc, I_NONE, [ t ], instr_annot),
      stack ->
        (Lwt.return (parse_ty ~allow_big_map:false t)) >>=? fun (Ex_ty t, _) ->
        typed ctxt loc (Cons_none t)
          (Item_t (Option_t t, stack, instr_annot))
    | Prim (loc, I_IF_NONE, [ bt ; bf ], instr_annot),
      (Item_t (Option_t t, rest, _) as bef) ->
        check_kind [ Seq_kind ] bt >>=? fun () ->
        check_kind [ Seq_kind ] bf >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt ~check_operations bt rest >>=? fun (btr, ctxt) ->
        parse_instr ?type_logger tc_context ctxt ~check_operations bf (Item_t (t, rest, instr_annot)) >>=? fun (bfr, ctxt) ->
        let branch ibt ibf =
          { loc ; instr = If_none (ibt, ibf) ; bef ; aft = ibt.aft } in
        merge_branches loc btr bfr { branch } >>=? fun judgement ->
        return ctxt judgement
    (* pairs *)
    | Prim (loc, I_PAIR, [], instr_annot),
      Item_t (a, Item_t (b, rest, snd_annot), fst_annot) ->
        typed ctxt loc Cons_pair
          (Item_t (Pair_t((a, fst_annot), (b, snd_annot)), rest, instr_annot))
    | Prim (loc, I_CAR, [], instr_annot),
      Item_t (Pair_t ((a, value_annot), _), rest, _) ->
        let annot = keep_or_rewrite_annot value_annot instr_annot in
        typed ctxt loc Car
          (Item_t (a, rest, annot))
    | Prim (loc, I_CDR, [], instr_annot),
      Item_t (Pair_t (_, (b, value_annot)), rest, _) ->
        let annot = keep_or_rewrite_annot value_annot instr_annot in
        typed ctxt loc Cdr
          (Item_t (b, rest, annot))
    (* unions *)
    | Prim (loc, I_LEFT, [ tr ], instr_annot),
      Item_t (tl, rest, stack_annot) ->
        (Lwt.return (parse_ty ~allow_big_map:false tr)) >>=? fun (Ex_ty tr, _) ->
        typed ctxt loc Left
          (Item_t (Union_t ((tl, stack_annot), (tr, None)), rest, instr_annot))
    | Prim (loc, I_RIGHT, [ tl ], instr_annot),
      Item_t (tr, rest, stack_annot) ->
        (Lwt.return (parse_ty ~allow_big_map:false tl)) >>=? fun (Ex_ty tl, _) ->
        typed ctxt loc Right
          (Item_t (Union_t ((tl, None), (tr, stack_annot)), rest, instr_annot))
    | Prim (loc, I_IF_LEFT, [ bt ; bf ], instr_annot),
      (Item_t (Union_t ((tl, left_annot), (tr, right_annot)), rest, _) as bef) ->
        check_kind [ Seq_kind ] bt >>=? fun () ->
        check_kind [ Seq_kind ] bf >>=? fun () ->
        fail_unexpected_annot loc instr_annot >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt ~check_operations bt (Item_t (tl, rest, left_annot)) >>=? fun (btr, ctxt) ->
        parse_instr ?type_logger tc_context ctxt ~check_operations bf (Item_t (tr, rest, right_annot)) >>=? fun (bfr, ctxt) ->
        let branch ibt ibf =
          { loc ; instr = If_left (ibt, ibf) ; bef ; aft = ibt.aft } in
        merge_branches loc btr bfr { branch } >>=? fun judgement ->
        return ctxt judgement
    (* lists *)
    | Prim (loc, I_NIL, [ t ], instr_annot),
      stack ->
        (Lwt.return (parse_ty ~allow_big_map:false t)) >>=? fun (Ex_ty t, _) ->
        typed ctxt loc Nil
          (Item_t (List_t t, stack, instr_annot))
    | Prim (loc, I_CONS, [], instr_annot),
      Item_t (tv, Item_t (List_t t, rest, _), _) ->
        check_item_ty tv t loc I_CONS 1 2 >>=? fun Eq ->
        typed ctxt loc Cons_list
          (Item_t (List_t t, rest, instr_annot))
    | Prim (loc, I_IF_CONS, [ bt ; bf ], instr_annot),
      (Item_t (List_t t, rest, stack_annot) as bef) ->
        check_kind [ Seq_kind ] bt >>=? fun () ->
        check_kind [ Seq_kind ] bf >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt ~check_operations bt
          (Item_t (t, Item_t (List_t t, rest, stack_annot), instr_annot)) >>=? fun (btr, ctxt) ->
        parse_instr ?type_logger tc_context ctxt ~check_operations bf
          rest >>=? fun (bfr, ctxt) ->
        let branch ibt ibf =
          { loc ; instr = If_cons (ibt, ibf) ; bef ; aft = ibt.aft } in
        merge_branches loc btr bfr { branch } >>=? fun judgement ->
        return ctxt judgement
    | Prim (loc, I_SIZE, [], instr_annot),
      Item_t (List_t _, rest, _) ->
        typed ctxt loc List_size
          (Item_t (Nat_t, rest, instr_annot))
    | Prim (loc, I_MAP, [], instr_annot),
      Item_t (Lambda_t (param, ret), Item_t (List_t elt, rest, _), _) ->
        check_item_ty elt param loc I_MAP 2 2 >>=? fun Eq ->
        typed ctxt loc List_map
          (Item_t (List_t ret, rest, instr_annot))
    | Prim (loc, I_MAP, [ body ], instr_annot),
      (Item_t (List_t elt, starting_rest, _)) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt ~check_operations
          body (Item_t (elt, starting_rest, None)) >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Typed ({ aft = Item_t (ret, rest, _) ; _ } as ibody) ->
              trace
                (Invalid_map_body (loc, ibody.aft))
                (Lwt.return (stack_ty_eq 1 rest starting_rest)) >>=? fun Eq ->
              typed ctxt loc (List_map_body ibody)
                (Item_t (List_t ret, rest, instr_annot))
          | Typed { aft ; _ } -> fail (Invalid_map_body (loc, aft))
          | Failed _ -> fail (Invalid_map_block_fail loc)
        end
    | Prim (loc, I_REDUCE, [], instr_annot),
      Item_t (Lambda_t (Pair_t ((pelt, _), (pr, _)), r),
              Item_t (List_t elt, Item_t (init, rest, _), _), _) ->
        check_item_ty r pr loc I_REDUCE 1 3 >>=? fun Eq ->
        check_item_ty elt pelt loc I_REDUCE 2 3 >>=? fun Eq ->
        check_item_ty init r loc I_REDUCE 3 3 >>=? fun Eq ->
        typed ctxt loc List_reduce
          (Item_t (r, rest, instr_annot))
    | Prim (loc, I_ITER, [ body ], instr_annot),
      Item_t (List_t elt, rest, _) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        fail_unexpected_annot loc instr_annot >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt ~check_operations
          body (Item_t (elt, rest, None)) >>=? begin fun (judgement, ctxt) ->
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
    | Prim (loc, I_EMPTY_SET, [ t ], instr_annot),
      rest ->
        (Lwt.return (parse_comparable_ty t)) >>=? fun (Ex_comparable_ty t) ->
        typed ctxt loc (Empty_set t)
          (Item_t (Set_t t, rest, instr_annot))
    | Prim (loc, I_REDUCE, [], instr_annot),
      Item_t (Lambda_t (Pair_t ((pelt, _), (pr, _)), r),
              Item_t (Set_t elt, Item_t (init, rest, _), _), _) ->
        let elt = ty_of_comparable_ty elt in
        check_item_ty r pr loc I_REDUCE 1 3 >>=? fun Eq ->
        check_item_ty elt pelt loc I_REDUCE 2 3 >>=? fun Eq ->
        check_item_ty init r loc I_REDUCE 3 3 >>=? fun Eq ->
        typed ctxt loc Set_reduce
          (Item_t (r, rest, instr_annot))
    | Prim (loc, I_ITER, [ body ], annot),
      Item_t (Set_t comp_elt, rest, _) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        fail_unexpected_annot loc annot >>=? fun () ->
        let elt = ty_of_comparable_ty comp_elt in
        parse_instr ?type_logger tc_context ctxt ~check_operations
          body (Item_t (elt, rest, None)) >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Typed ({ aft ; _ } as ibody) ->
              trace
                (Invalid_iter_body (loc, rest, ibody.aft))
                (Lwt.return (stack_ty_eq 1 aft rest)) >>=? fun Eq ->
              typed ctxt loc (Set_iter ibody) rest
          | Failed { descr } ->
              typed ctxt loc (Set_iter (descr rest)) rest
        end
    | Prim (loc, I_MEM, [], instr_annot),
      Item_t (v, Item_t (Set_t elt, rest, _), _) ->
        let elt = ty_of_comparable_ty elt in
        check_item_ty elt v loc I_MEM 1 2 >>=? fun Eq ->
        typed ctxt loc Set_mem
          (Item_t (Bool_t, rest, instr_annot))
    | Prim (loc, I_UPDATE, [], instr_annot),
      Item_t (v, Item_t (Bool_t, Item_t (Set_t elt, rest, _), _), _) ->
        let ty = ty_of_comparable_ty elt in
        check_item_ty ty v loc I_UPDATE 1 3 >>=? fun Eq ->
        typed ctxt loc Set_update
          (Item_t (Set_t elt, rest, instr_annot))
    | Prim (loc, I_SIZE, [], instr_annot),
      Item_t (Set_t _, rest, _) ->
        typed ctxt loc Set_size
          (Item_t (Nat_t, rest, instr_annot))
    (* maps *)
    | Prim (loc, I_EMPTY_MAP, [ tk ; tv ], instr_annot),
      stack ->
        (Lwt.return (parse_comparable_ty tk)) >>=? fun (Ex_comparable_ty tk) ->
        (Lwt.return (parse_ty ~allow_big_map:false tv)) >>=? fun (Ex_ty tv, _) ->
        typed ctxt loc (Empty_map (tk, tv))
          (Item_t (Map_t (tk, tv), stack, instr_annot))
    | Prim (loc, I_MAP, [], instr_annot),
      Item_t (Lambda_t (Pair_t ((pk, _), (pv, _)), ret),
              Item_t (Map_t (ck, v), rest, _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty pk k loc I_MAP 1 2 >>=? fun Eq ->
        check_item_ty pv v loc I_MAP 1 2 >>=? fun Eq ->
        typed ctxt loc Map_map
          (Item_t (Map_t (ck, ret), rest, instr_annot))
    | Prim (loc, I_REDUCE, [], instr_annot),
      Item_t (Lambda_t (Pair_t ((Pair_t ((pk, _), (pv, _)), _), (pr, _)), r),
              Item_t (Map_t (ck, v),
                      Item_t (init, rest, _), _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty pk k loc I_REDUCE 2 3 >>=? fun Eq ->
        check_item_ty pv v loc I_REDUCE 2 3 >>=? fun Eq ->
        check_item_ty r pr loc I_REDUCE 1 3 >>=? fun Eq ->
        check_item_ty init r loc I_REDUCE 3 3 >>=? fun Eq ->
        typed ctxt loc Map_reduce
          (Item_t (r, rest, instr_annot))
    | Prim (loc, I_ITER, [ body ], instr_annot),
      Item_t (Map_t (comp_elt, element_ty), rest, _) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        fail_unexpected_annot loc instr_annot >>=? fun () ->
        let key = ty_of_comparable_ty comp_elt in
        parse_instr ?type_logger tc_context ctxt ~check_operations body
          (Item_t (Pair_t ((key, None), (element_ty, None)), rest, None))
        >>=? begin fun (judgement, ctxt) -> match judgement with
          | Typed ({ aft ; _ } as ibody) ->
              trace
                (Invalid_iter_body (loc, rest, ibody.aft))
                (Lwt.return (stack_ty_eq 1 aft rest)) >>=? fun Eq ->
              typed ctxt loc (Map_iter ibody) rest
          | Failed { descr } ->
              typed ctxt loc (Map_iter (descr rest)) rest
        end
    | Prim (loc, I_MEM, [], instr_annot),
      Item_t (vk, Item_t (Map_t (ck, _), rest, _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty vk k loc I_MEM 1 2 >>=? fun Eq ->
        typed ctxt loc Map_mem
          (Item_t (Bool_t, rest, instr_annot))
    | Prim (loc, I_GET, [], instr_annot),
      Item_t (vk, Item_t (Map_t (ck, elt), rest, _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty vk k loc I_GET 1 2 >>=? fun Eq ->
        typed ctxt loc Map_get
          (Item_t (Option_t elt, rest, instr_annot))
    | Prim (loc, I_UPDATE, [], instr_annot),
      Item_t (vk, Item_t (Option_t vv, Item_t (Map_t (ck, v), rest, _), _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty vk k loc I_UPDATE 1 3 >>=? fun Eq ->
        check_item_ty vv v loc I_UPDATE 2 3 >>=? fun Eq ->
        typed ctxt loc Map_update
          (Item_t (Map_t (ck, v), rest, instr_annot))
    | Prim (loc, I_SIZE, [], instr_annot),
      Item_t (Map_t (_, _), rest, _) ->
        typed ctxt loc Map_size
          (Item_t (Nat_t, rest, instr_annot))
    (* big_map *)
    | Prim (loc, I_MEM, [], instr_annot),
      Item_t (set_key, Item_t (Big_map_t (map_key, _), rest, _), _) ->
        let k = ty_of_comparable_ty map_key in
        check_item_ty set_key k loc I_MEM 1 2 >>=? fun Eq ->
        typed ctxt loc Big_map_mem
          (Item_t (Bool_t, rest, instr_annot))
    | Prim (loc, I_GET, [], instr_annot),
      Item_t (vk, Item_t (Big_map_t (ck, elt), rest, _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty vk k loc I_GET 1 2 >>=? fun Eq ->
        typed ctxt loc Big_map_get
          (Item_t (Option_t elt, rest, instr_annot))
    | Prim (loc, I_UPDATE, [], instr_annot),
      Item_t (set_key, Item_t (Option_t set_value, Item_t (Big_map_t (map_key, map_value), rest, _), _), _) ->
        let k = ty_of_comparable_ty map_key in
        check_item_ty set_key k loc I_UPDATE 1 3 >>=? fun Eq ->
        check_item_ty set_value map_value loc I_UPDATE 2 3 >>=? fun Eq ->
        typed ctxt loc Big_map_update
          (Item_t (Big_map_t (map_key, map_value), rest, instr_annot))
    (* control *)
    | Seq (loc, [], annot),
      stack ->
        fail_unexpected_annot loc annot >>=? fun () ->
        typed ctxt loc Nop stack
    | Seq (loc, [ single ], annot),
      stack ->
        fail_unexpected_annot loc annot >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt ~check_operations single
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
    | Seq (loc, hd :: tl, annot),
      stack ->
        fail_unexpected_annot loc annot >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt ~check_operations hd
          stack >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Failed _ ->
              fail (Fail_not_in_tail_position (Micheline.location hd))
          | Typed ({ aft = middle ; _ } as ihd) ->
              parse_instr ?type_logger tc_context ctxt ~check_operations (Seq (-1, tl, None))
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
    | Prim (loc, I_IF, [ bt ; bf ], _),
      (Item_t (Bool_t, rest, _) as bef) ->
        check_kind [ Seq_kind ] bt >>=? fun () ->
        check_kind [ Seq_kind ] bf >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt ~check_operations bt
          rest >>=? fun (btr, ctxt) ->
        parse_instr ?type_logger tc_context ctxt ~check_operations bf
          rest >>=? fun (bfr, ctxt) ->
        let branch ibt ibf =
          { loc ; instr = If (ibt, ibf) ; bef ; aft = ibt.aft } in
        merge_branches loc btr bfr { branch } >>=? fun judgement ->
        return ctxt judgement
    | Prim (loc, I_LOOP, [ body ], _),
      (Item_t (Bool_t, rest, stack_annot) as stack) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt ~check_operations body
          rest >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Typed ibody ->
              trace
                (Unmatched_branches (loc, ibody.aft, stack))
                (Lwt.return (stack_ty_eq 1 ibody.aft stack)) >>=? fun Eq ->
              typed ctxt loc (Loop ibody) rest
          | Failed { descr } ->
              let ibody = descr (Item_t (Bool_t, rest, stack_annot)) in
              typed ctxt loc (Loop ibody) rest
        end
    | Prim (loc, I_LOOP_LEFT, [ body ], instr_annot),
      (Item_t (Union_t ((tl, tl_annot), (tr, tr_annot)), rest, _) as stack) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        fail_unexpected_annot loc instr_annot >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt ~check_operations body
          (Item_t (tl, rest, tl_annot)) >>=? begin fun (judgement, ctxt) -> match judgement with
          | Typed ibody ->
              trace
                (Unmatched_branches (loc, ibody.aft, stack))
                (Lwt.return (stack_ty_eq 1 ibody.aft stack)) >>=? fun Eq ->
              typed ctxt loc (Loop_left ibody) (Item_t (tr, rest, tr_annot))
          | Failed { descr } ->
              let ibody = descr (Item_t (Union_t ((tl, tl_annot), (tr, tr_annot)), rest, None)) in
              typed ctxt loc (Loop_left ibody) (Item_t (tr, rest, tr_annot))
        end
    | Prim (loc, I_LAMBDA, [ arg ; ret ; code ], instr_annot),
      stack ->
        (Lwt.return (parse_ty ~allow_big_map:false arg)) >>=? fun (Ex_ty arg, arg_annot) ->
        (Lwt.return (parse_ty ~allow_big_map:false ret)) >>=? fun (Ex_ty ret, _) ->
        check_kind [ Seq_kind ] code >>=? fun () ->
        parse_returning Lambda ?type_logger ctxt ~check_operations
          (arg, default_annot ~default:default_arg_annot arg_annot)
          ret code >>=? fun (lambda, ctxt) ->
        typed ctxt loc (Lambda lambda)
          (Item_t (Lambda_t (arg, ret), stack, instr_annot))
    | Prim (loc, I_EXEC, [], instr_annot),
      Item_t (arg, Item_t (Lambda_t (param, ret), rest, _), _) ->
        check_item_ty arg param loc I_EXEC 1 2 >>=? fun Eq ->
        typed ctxt loc Exec
          (Item_t (ret, rest, instr_annot))
    | Prim (loc, I_DIP, [ code ], instr_annot),
      Item_t (v, rest, stack_annot) ->
        fail_unexpected_annot loc instr_annot >>=? fun () ->
        check_kind [ Seq_kind ] code >>=? fun () ->
        parse_instr ?type_logger (add_dip v stack_annot tc_context) ctxt ~check_operations code
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
    | Prim (loc, I_ADD, [], instr_annot),
      Item_t (Timestamp_t, Item_t (Int_t, rest, _), _) ->
        typed ctxt loc Add_timestamp_to_seconds
          (Item_t (Timestamp_t, rest, instr_annot))
    | Prim (loc, I_ADD, [], instr_annot),
      Item_t (Int_t, Item_t (Timestamp_t, rest, _), _) ->
        typed ctxt loc Add_seconds_to_timestamp
          (Item_t (Timestamp_t, rest, instr_annot))
    | Prim (loc, I_SUB, [], instr_annot),
      Item_t (Timestamp_t, Item_t (Int_t, rest, _), _) ->
        typed ctxt loc Sub_timestamp_seconds
          (Item_t (Timestamp_t, rest, instr_annot))
    | Prim (loc, I_SUB, [], instr_annot),
      Item_t (Timestamp_t, Item_t (Timestamp_t, rest, _), _) ->
        typed ctxt loc Diff_timestamps
          (Item_t (Int_t, rest, instr_annot))
    (* string operations *)
    | Prim (loc, I_CONCAT, [], instr_annot),
      Item_t (String_t, Item_t (String_t, rest, _), _) ->
        typed ctxt loc Concat
          (Item_t (String_t, rest, instr_annot))
    (* currency operations *)
    | Prim (loc, I_ADD, [], instr_annot),
      Item_t (Tez_t, Item_t (Tez_t, rest, _), _) ->
        typed ctxt loc Add_tez
          (Item_t (Tez_t, rest, instr_annot))
    | Prim (loc, I_SUB, [], instr_annot),
      Item_t (Tez_t, Item_t (Tez_t, rest, _), _) ->
        typed ctxt loc Sub_tez
          (Item_t (Tez_t, rest, instr_annot))
    | Prim (loc, I_MUL, [], instr_annot),
      Item_t (Tez_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc Mul_teznat
          (Item_t (Tez_t, rest, instr_annot))
    | Prim (loc, I_MUL, [], instr_annot),
      Item_t (Nat_t, Item_t (Tez_t, rest, _), _) ->
        typed ctxt loc Mul_nattez
          (Item_t (Tez_t, rest, instr_annot))
    (* boolean operations *)
    | Prim (loc, I_OR, [], instr_annot),
      Item_t (Bool_t, Item_t (Bool_t, rest, _), _) ->
        typed ctxt loc Or
          (Item_t (Bool_t, rest, instr_annot))
    | Prim (loc, I_AND, [], instr_annot),
      Item_t (Bool_t, Item_t (Bool_t, rest, _), _) ->
        typed ctxt loc And
          (Item_t (Bool_t, rest, instr_annot))
    | Prim (loc, I_XOR, [], instr_annot),
      Item_t (Bool_t, Item_t (Bool_t, rest, _), _) ->
        typed ctxt loc Xor
          (Item_t (Bool_t, rest, instr_annot))
    | Prim (loc, I_NOT, [], instr_annot),
      Item_t (Bool_t, rest, _) ->
        typed ctxt loc Not
          (Item_t (Bool_t, rest, instr_annot))
    (* integer operations *)
    | Prim (loc, I_ABS, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        typed ctxt loc Abs_int
          (Item_t (Nat_t, rest, instr_annot))
    | Prim (loc, I_ISNAT, [], Some instr_annot),
      Item_t (Int_t, rest, None) ->
        typed ctxt loc Is_nat
          (Item_t (Option_t Nat_t, rest, Some instr_annot))
    | Prim (loc, I_ISNAT, [], None),
      Item_t (Int_t, rest, annot) ->
        typed ctxt loc Is_nat
          (Item_t (Option_t Nat_t, rest, annot))
    | Prim (loc, I_INT, [], instr_annot),
      Item_t (Nat_t, rest, _) ->
        typed ctxt loc Int_nat
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_NEG, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        typed ctxt loc Neg_int
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_NEG, [], instr_annot),
      Item_t (Nat_t, rest, _) ->
        typed ctxt loc Neg_nat
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_ADD, [], instr_annot),
      Item_t (Int_t, Item_t (Int_t, rest, _), _) ->
        typed ctxt loc Add_intint
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_ADD, [], instr_annot),
      Item_t (Int_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc Add_intnat
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_ADD, [], instr_annot),
      Item_t (Nat_t, Item_t (Int_t, rest, _), _) ->
        typed ctxt loc Add_natint
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_ADD, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc Add_natnat
          (Item_t (Nat_t, rest, instr_annot))
    | Prim (loc, I_SUB, [], instr_annot),
      Item_t (Int_t, Item_t (Int_t, rest, _), _) ->
        typed ctxt loc Sub_int
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_SUB, [], instr_annot),
      Item_t (Int_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc Sub_int
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_SUB, [], instr_annot),
      Item_t (Nat_t, Item_t (Int_t, rest, _), _) ->
        typed ctxt loc Sub_int
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_SUB, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc Sub_int
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_MUL, [], instr_annot),
      Item_t (Int_t, Item_t (Int_t, rest, _), _) ->
        typed ctxt loc Mul_intint
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_MUL, [], instr_annot),
      Item_t (Int_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc Mul_intnat
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_MUL, [], instr_annot),
      Item_t (Nat_t, Item_t (Int_t, rest, _), _) ->
        typed ctxt loc Mul_natint
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_MUL, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc Mul_natnat
          (Item_t (Nat_t, rest, instr_annot))
    | Prim (loc, I_EDIV, [], instr_annot),
      Item_t (Tez_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc Ediv_teznat
          (Item_t (Option_t (Pair_t ((Tez_t, None), (Tez_t, None))), rest, instr_annot))
    | Prim (loc, I_EDIV, [], instr_annot),
      Item_t (Tez_t, Item_t (Tez_t, rest, _), _) ->
        typed ctxt loc Ediv_tez
          (Item_t (Option_t (Pair_t ((Nat_t, None), (Tez_t, None))), rest, instr_annot))
    | Prim (loc, I_EDIV, [], instr_annot),
      Item_t (Int_t, Item_t (Int_t, rest, _), _) ->
        typed ctxt loc Ediv_intint
          (Item_t (Option_t (Pair_t ((Int_t, None), (Nat_t, None))), rest, instr_annot))
    | Prim (loc, I_EDIV, [], instr_annot),
      Item_t (Int_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc Ediv_intnat
          (Item_t (Option_t (Pair_t ((Int_t, None), (Nat_t, None))), rest, instr_annot))
    | Prim (loc, I_EDIV, [], instr_annot),
      Item_t (Nat_t, Item_t (Int_t, rest, _), _) ->
        typed ctxt loc Ediv_natint
          (Item_t (Option_t (Pair_t ((Int_t, None), (Nat_t, None))), rest, instr_annot))
    | Prim (loc, I_EDIV, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc Ediv_natnat
          (Item_t (Option_t (Pair_t ((Nat_t, None), (Nat_t, None))), rest, instr_annot))
    | Prim (loc, I_LSL, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc Lsl_nat
          (Item_t (Nat_t, rest, instr_annot))
    | Prim (loc, I_LSR, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc Lsr_nat
          (Item_t (Nat_t, rest, instr_annot))
    | Prim (loc, I_OR, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc Or_nat
          (Item_t (Nat_t, rest, instr_annot))
    | Prim (loc, I_AND, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc And_nat
          (Item_t (Nat_t, rest, instr_annot))
    | Prim (loc, I_AND, [], instr_annot),
      Item_t (Int_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc And_int_nat
          (Item_t (Nat_t, rest, instr_annot))
    | Prim (loc, I_XOR, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc Xor_nat
          (Item_t (Nat_t, rest, instr_annot))
    | Prim (loc, I_NOT, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        typed ctxt loc Not_int
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_NOT, [], instr_annot),
      Item_t (Nat_t, rest, _) ->
        typed ctxt loc Not_nat
          (Item_t (Int_t, rest, instr_annot))
    (* comparison *)
    | Prim (loc, I_COMPARE, [], instr_annot),
      Item_t (Int_t, Item_t (Int_t, rest, _), _) ->
        typed ctxt loc (Compare Int_key)
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_COMPARE, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        typed ctxt loc (Compare Nat_key)
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_COMPARE, [], instr_annot),
      Item_t (Bool_t, Item_t (Bool_t, rest, _), _) ->
        typed ctxt loc (Compare Bool_key)
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_COMPARE, [], instr_annot),
      Item_t (String_t, Item_t (String_t, rest, _), _) ->
        typed ctxt loc (Compare String_key)
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_COMPARE, [], instr_annot),
      Item_t (Tez_t, Item_t (Tez_t, rest, _), _) ->
        typed ctxt loc (Compare Tez_key)
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_COMPARE, [], instr_annot),
      Item_t (Key_hash_t, Item_t (Key_hash_t, rest, _), _) ->
        typed ctxt loc (Compare Key_hash_key)
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_COMPARE, [], instr_annot),
      Item_t (Timestamp_t, Item_t (Timestamp_t, rest, _), _) ->
        typed ctxt loc (Compare Timestamp_key)
          (Item_t (Int_t, rest, instr_annot))
    | Prim (loc, I_COMPARE, [], instr_annot),
      Item_t (Address_t, Item_t (Address_t, rest, _), _) ->
        typed ctxt loc (Compare Address_key)
          (Item_t (Int_t, rest, instr_annot))
    (* comparators *)
    | Prim (loc, I_EQ, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        typed ctxt loc Eq
          (Item_t (Bool_t, rest, instr_annot))
    | Prim (loc, I_NEQ, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        typed ctxt loc Neq
          (Item_t (Bool_t, rest, instr_annot))
    | Prim (loc, I_LT, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        typed ctxt loc Lt
          (Item_t (Bool_t, rest, instr_annot))
    | Prim (loc, I_GT, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        typed ctxt loc Gt
          (Item_t (Bool_t, rest, instr_annot))
    | Prim (loc, I_LE, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        typed ctxt loc Le
          (Item_t (Bool_t, rest, instr_annot))
    | Prim (loc, I_GE, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        typed ctxt loc Ge
          (Item_t (Bool_t, rest, instr_annot))
    (* protocol *)
    | Prim (loc, I_ADDRESS, [], _),
      Item_t (Contract_t _, rest, instr_annot) ->
        typed ctxt loc Address
          (Item_t (Address_t, rest, instr_annot))
    | Prim (loc, I_CONTRACT, [ ty ], _),
      Item_t (Address_t, rest, instr_annot) ->
        Lwt.return (parse_ty ~allow_big_map:false ty) >>=? fun (Ex_ty t, annot) ->
        fail_unexpected_annot loc annot >>=? fun () ->
        typed ctxt loc (Contract t)
          (Item_t (Option_t (Contract_t t), rest, instr_annot))
    | Prim (loc, I_MANAGER, [], instr_annot),
      Item_t (Contract_t _, rest, _) ->
        typed ctxt loc Manager
          (Item_t (Key_hash_t, rest, instr_annot))
    | Prim (loc, I_MANAGER, [], instr_annot),
      Item_t (Address_t, rest, _) ->
        typed ctxt loc Address_manager
          (Item_t (Option_t Key_hash_t, rest, instr_annot))
    | Prim (loc, I_TRANSFER_TOKENS, [], instr_annot),
      Item_t (p, Item_t
                (Tez_t, Item_t
                   (Contract_t cp, rest, _), _), _) ->
        check_item_ty p cp loc I_TRANSFER_TOKENS 1 4 >>=? fun Eq ->
        typed ctxt loc Transfer_tokens (Item_t (Operation_t, rest, instr_annot))
    | Prim (loc, I_CREATE_ACCOUNT, [], instr_annot),
      Item_t
        (Key_hash_t, Item_t
           (Option_t Key_hash_t, Item_t
              (Bool_t, Item_t
                 (Tez_t, rest, _), _), _), _) ->
        typed ctxt loc Create_account
          (Item_t (Contract_t Unit_t, rest, instr_annot))
    | Prim (loc, I_IMPLICIT_ACCOUNT, [], instr_annot),
      Item_t (Key_hash_t, rest, _) ->
        typed ctxt loc Implicit_account
          (Item_t (Contract_t Unit_t, rest, instr_annot))
    | Prim (loc, I_CREATE_CONTRACT, [], instr_annot),
      Item_t
        (Key_hash_t, Item_t
           (Option_t Key_hash_t, Item_t
              (Bool_t, Item_t
                 (Bool_t, Item_t
                    (Tez_t, Item_t
                       (Lambda_t (Pair_t ((p, _), (gp, _)),
                                  Pair_t ((List_t Operation_t, _), (gr, _))), Item_t
                          (ginit, rest, _), _), _), _), _), _), _) ->
        check_item_ty gp gr loc I_CREATE_CONTRACT 5 7 >>=? fun Eq ->
        check_item_ty ginit gp loc I_CREATE_CONTRACT 6 7 >>=? fun Eq ->
        typed ctxt loc (Create_contract (gp, p))
          (Item_t (Contract_t p, rest, instr_annot))
    | Prim (loc, I_CREATE_CONTRACT, [ (Seq (seq_loc, _, annot) as code)], instr_annot),
      Item_t
        (Key_hash_t, Item_t
           (Option_t Key_hash_t, Item_t
              (Bool_t, Item_t
                 (Bool_t, Item_t
                    (Tez_t, Item_t
                       (ginit, rest, _), _), _), _), _), _) ->
        fail_unexpected_annot seq_loc annot >>=? fun () ->
        let cannonical_code = fst @@ Micheline.extract_locations code in
        Lwt.return (parse_toplevel cannonical_code) >>=? fun (arg_type, storage_type, code_field) ->
        trace
          (Ill_formed_type (Some "parameter", cannonical_code, location arg_type))
          (Lwt.return (parse_ty ~allow_big_map:false arg_type)) >>=? fun (Ex_ty arg_type, param_annot) ->
        trace
          (Ill_formed_type (Some "storage", cannonical_code, location storage_type))
          (Lwt.return (parse_ty ~allow_big_map:true storage_type)) >>=? fun (Ex_ty storage_type, storage_annot) ->
        let arg_type_full = Pair_t ((arg_type, default_annot ~default:default_param_annot param_annot),
                                    (storage_type, default_annot ~default:default_storage_annot storage_annot)) in
        let ret_type_full = Pair_t ((List_t Operation_t, None), (storage_type, None)) in
        trace
          (Ill_typed_contract (cannonical_code, []))
          (parse_returning (Toplevel { storage_type ; param_type = arg_type })
             ctxt ?type_logger ~check_operations (arg_type_full, None) ret_type_full code_field) >>=?
        fun (Lam ({ bef = Item_t (arg, Empty_t, _) ;
                    aft = Item_t (ret, Empty_t, _) ; _ }, _) as lambda, ctxt) ->
        Lwt.return @@ ty_eq arg arg_type_full >>=? fun Eq ->
        Lwt.return @@ ty_eq ret ret_type_full >>=? fun Eq ->
        Lwt.return @@ ty_eq storage_type ginit >>=? fun Eq ->
        typed ctxt loc (Create_contract_literal (storage_type, arg_type, lambda))
          (Item_t (Contract_t arg_type, rest, instr_annot))
    | Prim (loc, I_NOW, [], instr_annot),
      stack ->
        typed ctxt loc Now
          (Item_t (Timestamp_t, stack, instr_annot))
    | Prim (loc, I_AMOUNT, [], instr_annot),
      stack ->
        typed ctxt loc Amount
          (Item_t (Tez_t, stack, instr_annot))
    | Prim (loc, I_BALANCE, [], instr_annot),
      stack ->
        typed ctxt loc Balance
          (Item_t (Tez_t, stack, instr_annot))
    | Prim (loc, I_HASH_KEY, [], instr_annot),
      Item_t (Key_t, rest, _) ->
        typed ctxt loc Hash_key
          (Item_t (Key_hash_t, rest, instr_annot))
    | Prim (loc, I_CHECK_SIGNATURE, [], instr_annot),
      Item_t (Key_t, Item_t (Pair_t ((Signature_t, _), (String_t, _)), rest, _), _) ->
        typed ctxt loc Check_signature
          (Item_t (Bool_t, rest, instr_annot))
    | Prim (loc, I_H, [], instr_annot),
      Item_t (t, rest, _) ->
        typed ctxt loc (H t)
          (Item_t (String_t, rest, instr_annot))
    | Prim (loc, I_STEPS_TO_QUOTA, [], instr_annot),
      stack ->
        typed ctxt loc Steps_to_quota
          (Item_t (Nat_t, stack, instr_annot))
    | Prim (loc, I_SOURCE, [], instr_annot),
      stack ->
        typed ctxt loc Source
          (Item_t (Address_t, stack, instr_annot))
    | Prim (loc, I_SELF, [], instr_annot),
      stack ->
        let rec get_toplevel_type : tc_context -> (bef judgement * context) tzresult Lwt.t = function
          | Lambda -> fail (Self_in_lambda loc)
          | Dip (_, prev) -> get_toplevel_type prev
          | Toplevel { param_type ; _ } ->
              typed ctxt loc (Self param_type)
                (Item_t (Contract_t param_type, stack, instr_annot)) in
        get_toplevel_type tc_context
    (* Primitive parsing errors *)
    | Prim (loc, (I_DROP | I_DUP | I_SWAP | I_SOME | I_UNIT
                 | I_PAIR | I_CAR | I_CDR | I_CONS
                 | I_MEM | I_UPDATE | I_MAP | I_REDUCE
                 | I_GET | I_EXEC | I_FAIL | I_SIZE
                 | I_CONCAT | I_ADD | I_SUB
                 | I_MUL | I_EDIV | I_OR | I_AND | I_XOR
                 | I_NOT
                 | I_ABS | I_NEG | I_LSL | I_LSR
                 | I_COMPARE | I_EQ | I_NEQ
                 | I_LT | I_GT | I_LE | I_GE
                 | I_MANAGER | I_TRANSFER_TOKENS | I_CREATE_ACCOUNT
                 | I_CREATE_CONTRACT | I_NOW
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
    | Prim (loc, (I_REDUCE | I_UPDATE as name), [], _),
      stack ->
        fail (Bad_stack (loc, name, 3, stack))
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
            I_MEM ; I_UPDATE ; I_MAP ; I_REDUCE ; I_ITER ;
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
  : type arg. context -> arg ty -> Script.location -> Contract.t ->
    (arg typed_contract * context) tzresult Lwt.t
  = fun ctxt arg loc contract ->
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
              (ty_eq arg Unit_t >>? fun Eq ->
               let contract : arg typed_contract = (arg, contract) in
               ok (contract, ctxt))
        | Some { code ; _ } ->
            Lwt.return
              (parse_toplevel code >>? fun (arg_type, _, _) ->
               parse_ty ~allow_big_map:false arg_type >>? fun (Ex_ty targ, _) ->
               ty_eq targ arg >>? fun Eq ->
               let contract : arg typed_contract = (arg, contract) in
               ok (contract, ctxt))

and parse_toplevel
  : Script.expr -> (Script.node * Script.node * Script.node) tzresult
  = fun toplevel ->
    match root toplevel with
    | Int (loc, _) -> error (Invalid_kind (loc, [ Seq_kind ], Int_kind))
    | String (loc, _) -> error (Invalid_kind (loc, [ Seq_kind ], String_kind))
    | Prim (loc, _, _, _) -> error (Invalid_kind (loc, [ Seq_kind ], Prim_kind))
    | Seq (_, fields, _) ->
        let rec find_fields p s c fields =
          match fields with
          | [] -> ok (p, s, c)
          | Int (loc, _) :: _ -> error (Invalid_kind (loc, [ Prim_kind ], Int_kind))
          | String (loc, _) :: _ -> error (Invalid_kind (loc, [ Prim_kind ], String_kind))
          | Seq (loc, _, _) :: _ -> error (Invalid_kind (loc, [ Prim_kind ], Seq_kind))
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
  : ?type_logger: (int -> Script.expr list -> Script.expr list -> unit) ->
    context -> check_operations:bool -> Script.t -> (ex_script * context) tzresult Lwt.t
  = fun ?type_logger ctxt ~check_operations { code ; storage } ->
    Lwt.return (parse_toplevel code) >>=? fun (arg_type, storage_type, code_field) ->
    trace
      (Ill_formed_type (Some "parameter", code, location arg_type))
      (Lwt.return (parse_ty ~allow_big_map:false arg_type)) >>=? fun (Ex_ty arg_type, param_annot) ->
    trace
      (Ill_formed_type (Some "storage", code, location storage_type))
      (Lwt.return (parse_ty ~allow_big_map:true storage_type)) >>=? fun (Ex_ty storage_type, storage_annot) ->
    let arg_type_full = Pair_t ((arg_type, default_annot ~default:default_param_annot param_annot),
                                (storage_type, default_annot ~default:default_storage_annot storage_annot)) in
    let ret_type_full = Pair_t ((List_t Operation_t, None), (storage_type, None)) in
    trace
      (Ill_typed_data (None, storage, storage_type))
      (parse_data ?type_logger ctxt ~check_operations storage_type (root storage)) >>=? fun (storage, ctxt) ->
    trace
      (Ill_typed_contract (code, []))
      (parse_returning (Toplevel { storage_type ; param_type = arg_type })
         ctxt ?type_logger ~check_operations (arg_type_full, None) ret_type_full code_field) >>=? fun (code, ctxt) ->
    return (Ex_script { code ; arg_type ; storage ; storage_type }, ctxt)

let parse_contract :
  type t. context -> Script.location -> t Script_typed_ir.ty -> Contract.t ->
  (context * t Script_typed_ir.typed_contract) tzresult Lwt.t
  = fun ctxt loc ty contract ->
    Contract.get_script ctxt contract >>=? fun (ctxt, script) -> match script with
    | None ->
        begin match ty with
          | Unit_t -> return (ctxt, (ty, contract))
          | _ -> fail (Invalid_contract (loc, contract))
        end
    | Some script ->
        Lwt.return @@ parse_toplevel script.code >>=? fun (arg_type, _, _) ->
        let arg_type = Micheline.strip_locations arg_type in
        Lwt.return (parse_ty ~allow_big_map:false (Micheline.root arg_type)) >>=? fun (Ex_ty arg_type, _) ->
        Lwt.return (ty_eq ty arg_type) >>=? fun Eq ->
        return (ctxt, (ty, contract))

let typecheck_code
  : context -> Script.expr -> (type_map * context) tzresult Lwt.t
  = fun ctxt code ->
    Lwt.return (parse_toplevel code) >>=? fun (arg_type, storage_type, code_field) ->
    let type_map = ref [] in
    (* TODO: annotation checking *)
    trace
      (Ill_formed_type (Some "parameter", code, location arg_type))
      (Lwt.return (parse_ty ~allow_big_map:false arg_type)) >>=? fun (Ex_ty arg_type, param_annot) ->
    trace
      (Ill_formed_type (Some "storage", code, location storage_type))
      (Lwt.return (parse_ty ~allow_big_map:true storage_type)) >>=? fun (Ex_ty storage_type, storage_annot) ->
    let arg_type_full = Pair_t ((arg_type, default_annot ~default:default_param_annot param_annot),
                                (storage_type, default_annot ~default:default_storage_annot storage_annot)) in
    let ret_type_full = Pair_t ((List_t Operation_t, None), (storage_type, None)) in
    let result =
      parse_returning
        (Toplevel { storage_type ; param_type = arg_type })
        ctxt
        ~type_logger: (fun loc bef aft -> type_map := (loc, (bef, aft)) :: !type_map)
        ~check_operations: true
        (arg_type_full, None) ret_type_full code_field in
    trace
      (Ill_typed_contract (code, !type_map))
      result >>=? fun (Lam _, ctxt) ->
    return (!type_map, ctxt)

let typecheck_data
  : ?type_logger: (int -> Script.expr list -> Script.expr list -> unit) ->
    context -> check_operations:bool -> Script.expr * Script.expr -> context tzresult Lwt.t
  = fun ?type_logger ctxt ~check_operations (data, exp_ty) ->
    trace
      (Ill_formed_type (None, exp_ty, 0))
      (Lwt.return (parse_ty ~allow_big_map:true (root exp_ty))) >>=? fun (Ex_ty exp_ty, _) ->
    trace
      (Ill_typed_data (None, data, exp_ty))
      (parse_data ?type_logger ctxt ~check_operations exp_ty (root data)) >>=? fun (_, ctxt) ->
    return ctxt

let hash_data ctxt typ data =
  unparse_data ctxt typ data >|? fun (data, ctxt) ->
  let unparsed = strip_annotations @@ data in
  let bytes = Data_encoding.Binary.to_bytes_exn expr_encoding (Micheline.strip_locations unparsed) in
  (Script_expr_hash.(hash_bytes [ bytes ] |> to_b58check), ctxt)

(* ---------------- Big map -------------------------------------------------*)

let big_map_mem ctxt contract key { diff ; key_type ; _ } =
  match map_get key diff with
  | None -> Lwt.return @@ hash_data ctxt key_type key >>=? fun (hash, ctxt) ->
      Alpha_context.Contract.Big_map.mem ctxt contract hash >>=? fun (ctxt, res) ->
      return (res, ctxt)
  | Some None -> return (false, ctxt)
  | Some (Some _) -> return (true, ctxt)

let big_map_get ctxt contract key { diff ; key_type ; value_type } =
  match map_get key diff with
  | Some x -> return (x, ctxt)
  | None ->
      Lwt.return @@ hash_data ctxt key_type key >>=? fun (hash, ctxt) ->
      Alpha_context.Contract.Big_map.get_opt
        ctxt contract hash >>=? begin function
        | (ctxt, None) -> return (None, ctxt)
        | (ctxt, Some value) ->
            parse_data ctxt ~check_operations:false value_type
              (Micheline.root value) >>=? fun (x, ctxt) ->
            return (Some x, ctxt)
      end

let big_map_update key value ({ diff ; _ } as map) =
  { map with diff = map_set key value diff }

let to_big_map_diff_list ctxt { key_type ; value_type ; diff } =
  Lwt.return (Gas.consume ctxt (Michelson_v1_gas.Cost_of.map_to_list diff)) >>=? fun ctxt ->
  let pairs = map_fold (fun key value acc -> (key, value) :: acc) diff [] in
  fold_left_s
    (fun (acc, ctxt) (key, value) ->
       Lwt.return (Gas.consume ctxt Typecheck_costs.cycle) >>=? fun ctxt ->
       Lwt.return @@ hash_data ctxt key_type key >>=? fun (hash, ctxt) ->
       begin
         match value with
         | None -> return (None, ctxt)
         | Some x ->
             begin
               Lwt.return @@ unparse_data ctxt value_type x >>=? fun (node, ctxt) ->
               return (Some (Micheline.strip_locations node), ctxt)
             end
       end >>=? fun (value, ctxt) ->
       return ((hash, value) :: acc, ctxt))
    ([], ctxt) pairs

(* Get the big map from a contract's storage if one exists *)
let extract_big_map : type a. a ty -> a -> ex_big_map option = fun ty x ->
  match (ty, x) with
  | Pair_t ((Big_map_t (_, _), _), _), (map, _) -> Some (Ex_bm map)
  | _, _ -> None

let to_serializable_big_map gas (Ex_bm bm) =
  to_big_map_diff_list gas bm

let to_printable_big_map ctxt (Ex_bm { diff ; key_type ; value_type }) =
  let un_error = function
    | Ok x -> x
    | Error _ -> Pervasives.failwith "Raise to_printiable_big_map gas limit" in
  let ctxt = Gas.set_unlimited ctxt in
  let unparse ty value =
    fst @@ un_error @@ unparse_data ctxt ty value in
  let pairs = map_fold (fun key value acc -> (key, value) :: acc) diff [] in
  List.fold_left
    (fun acc (key, value) ->
       ((Micheline.strip_locations @@ unparse key_type key,
         Option.map ~f:(fun x -> Micheline.strip_locations @@ unparse value_type x) value) :: acc)) [] pairs

let erase_big_map_initialization ctxt ({ code ; storage } : Script.t) =
  Lwt.return @@ parse_toplevel code >>=? fun (_, storage_type, _) ->
  Lwt.return @@ parse_ty ~allow_big_map:true storage_type >>=? fun (Ex_ty ty, _) ->
  parse_data ctxt ~check_operations:true ty
    (Micheline.root storage) >>=? fun (storage, ctxt) ->
  begin
    match extract_big_map ty storage with
    | None -> return (None, ctxt)
    | Some bm -> to_serializable_big_map ctxt bm >>=? fun (bm, ctxt) ->
        return (Some bm, ctxt)
  end >>=? fun (bm, ctxt) ->
  Lwt.return @@ unparse_data ctxt ty storage >>=? fun (storage, ctxt) ->
  return ({ code ; storage = Micheline.strip_locations storage }, bm, ctxt)
