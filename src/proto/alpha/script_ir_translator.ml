(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_context
open Micheline
open Script
open Script_typed_ir

(* ---- Error definitions ---------------------------------------------------*)

(* Auxiliary types for error documentation *)
type namespace = Type_namespace | Constant_namespace | Instr_namespace | Keyword_namespace
type kind = Int_kind | String_kind | Prim_kind | Seq_kind
type type_map = (int * (Script.expr list * Script.expr list)) list

(* Structure errors *)
type error += Invalid_arity of Script.location * prim * int * int
type error += Invalid_namespace of Script.location * prim * namespace * namespace
type error += Invalid_primitive of Script.location * prim list * prim
type error += Invalid_kind of Script.location * kind list * kind
type error += Missing_field of prim
type error += Duplicate_field of Script.location * prim

(* Instruction typing errors *)
type error += Fail_not_in_tail_position of Script.location
type error += Undefined_binop : Script.location * prim * _ ty * _ ty -> error
type error += Undefined_unop : Script.location * prim * _ ty -> error
type error += Bad_return : Script.location * _ stack_ty * _ ty -> error
type error += Bad_stack : Script.location * prim * int * _ stack_ty -> error
type error += Unmatched_branches : Script.location * _ stack_ty * _ stack_ty -> error
type error += Transfer_in_lambda of Script.location
type error += Transfer_in_dip of Script.location
type error += Bad_stack_length
type error += Bad_stack_item of int
type error += Inconsistent_annotations of string * string
type error += Inconsistent_type_annotations : Script.location * _ ty * _ ty -> error
type error += Unexpected_annotation of Script.location
type error += Invalid_map_body : Script.location * _ stack_ty -> error
type error += Invalid_map_block_fail of Script.location
type error += Invalid_iter_body : Script.location * _ stack_ty * _ stack_ty -> error

(* Value typing errors *)
type error += Invalid_constant : Script.location * Script.expr * _ ty -> error
type error += Invalid_contract of Script.location * Contract.t
type error += Comparable_type_expected : Script.location * _ ty -> error
type error += Inconsistent_types : _ ty * _ ty -> error
type error += Unordered_map_keys of Script.location * Script.expr
type error += Unordered_set_values of Script.location * Script.expr
type error += Duplicate_map_keys of Script.location * Script.expr
type error += Duplicate_set_values of Script.location * Script.expr

(* Toplevel errors *)
type error += Ill_typed_data : string option * Script.expr * _ ty -> error
type error += Ill_formed_type of string option * Script.expr * Script.location
type error += Ill_typed_contract : Script.expr * type_map -> error

type ex_comparable_ty = Ex_comparable_ty : 'a comparable_ty -> ex_comparable_ty
type ex_ty = Ex_ty : 'a ty -> ex_ty
type ex_stack_ty = Ex_stack_ty : 'a stack_ty -> ex_stack_ty

type tc_context =
  | Lambda : tc_context
  | Dip : 'a stack_ty -> tc_context
  | Toplevel : { storage_type : 'a ty } -> tc_context

let add_dip ty annot = function
  | Lambda | Toplevel _ -> Dip (Item_t (ty, Empty_t, annot))
  | Dip stack -> Dip (Item_t (ty, stack, annot))

let default_param_annot = Some "@parameter"
let default_storage_annot = Some "@storage"
let default_arg_annot = Some "@arg"

let default_annot ~default = function
  | None -> default
  | annot -> annot

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
  | K_return
  | K_storage
  | K_code -> Keyword_namespace
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
  | I_LOOP_LEFT -> Instr_namespace
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
  | T_unit -> Type_namespace


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
    | Key_hash_key -> Ed25519.Public_key_hash.compare x y
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
    end)

let set_update
  : type a. a -> bool -> a set -> a set
  = fun v b (module Box) ->
    (module struct
      type elt = a
      module OPS = Box.OPS
      let boxed =
        if b then
          Box.OPS.add v Box.boxed
        else
          Box.OPS.remove v Box.boxed
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
    Script_int.(abs (of_int (Box.OPS.cardinal Box.boxed)))

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
      let boxed = OPS.empty
    end)

let map_get
  : type key value. key -> (key, value) map -> value option
  = fun k (module Box) ->
    try Some (Box.OPS.find k Box.boxed) with Not_found -> None

let map_update
  : type a b. a -> b option -> (a, b) map -> (a, b) map
  = fun k v (module Box) ->
    (module struct
      type key = a
      type value = b
      let key_ty = Box.key_ty
      module OPS = Box.OPS
      let boxed =
        match v with
        | Some v -> Box.OPS.add k v Box.boxed
        | None -> Box.OPS.remove k Box.boxed
    end)

let map_mem
  : type key value. key -> (key, value) map -> bool
  = fun k (module Box) ->
    Box.OPS.mem k Box.boxed

let map_fold
  : type key value acc. (key -> value -> acc -> acc) -> (key, value) map -> acc -> acc
  = fun f (module Box) ->
    Box.OPS.fold f Box.boxed

let map_size
  : type key value. (key, value) map -> Script_int.n Script_int.num =
  fun (module Box) ->
    Script_int.(abs (of_int (Box.OPS.cardinal Box.boxed)))

(* ---- Unparsing (Typed IR -> Untyped epressions) --------------------------*)

let ty_of_comparable_ty
  : type a. a comparable_ty -> a ty = function
  | Int_key -> Int_t
  | Nat_key -> Nat_t
  | String_key -> String_t
  | Tez_key -> Tez_t
  | Bool_key -> Bool_t
  | Key_hash_key -> Key_hash_t
  | Timestamp_key -> Timestamp_t

let unparse_comparable_ty
  : type a. a comparable_ty -> Script.node = function
  | Int_key -> Prim (-1, T_int, [], None)
  | Nat_key -> Prim (-1, T_nat, [], None)
  | String_key -> Prim (-1, T_string, [], None)
  | Tez_key -> Prim (-1, T_tez, [], None)
  | Bool_key -> Prim (-1, T_bool, [], None)
  | Key_hash_key -> Prim (-1, T_key_hash, [], None)
  | Timestamp_key -> Prim (-1, T_timestamp, [], None)

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
  | Signature_t -> Prim (-1, T_signature, [], annot)
  | Contract_t (utl, utr) ->
      let tl = unparse_ty None utl in
      let tr = unparse_ty None utr in
      Prim (-1, T_contract, [ tl; tr ], annot)
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

let rec unparse_data
  : type a. a ty -> a -> Script.node
  = fun ty a -> match ty, a with
    | Unit_t, () ->
        Prim (-1, D_Unit, [], None)
    | Int_t, v ->
        Int (-1, Script_int.to_string v)
    | Nat_t, v ->
        Int (-1, Script_int.to_string v)
    | String_t, s ->
        String (-1, s)
    | Bool_t, true ->
        Prim (-1, D_True, [], None)
    | Bool_t, false ->
        Prim (-1, D_False, [], None)
    | Timestamp_t, t ->
        begin
          match Script_timestamp.to_notation t with
          | None -> Int (-1, Script_timestamp.to_num_str t)
          | Some s -> String (-1, s)
        end
    | Contract_t _, (_, _, c)  ->
        String (-1, Contract.to_b58check c)
    | Signature_t, s ->
        let text =
          Hex_encode.hex_encode
            (MBytes.to_string (Data_encoding.Binary.to_bytes Ed25519.Signature.encoding s)) in
        String (-1, text)
    | Tez_t, v ->
        String (-1, Tez.to_string v)
    | Key_t, k ->
        String (-1, Ed25519.Public_key.to_b58check k)
    | Key_hash_t, k ->
        String (-1, Ed25519.Public_key_hash.to_b58check k)
    | Pair_t ((tl, _), (tr, _)), (l, r) ->
        let l = unparse_data tl l in
        let r = unparse_data tr r in
        Prim (-1, D_Pair, [ l; r ], None)
    | Union_t ((tl, _), _), L l ->
        let l = unparse_data tl l in
        Prim (-1, D_Left, [ l ], None)
    | Union_t (_, (tr, _)), R r ->
        let r = unparse_data tr r in
        Prim (-1, D_Right, [ r ], None)
    | Option_t t, Some v ->
        let v = unparse_data t v in
        Prim (-1, D_Some, [ v ], None)
    | Option_t _, None ->
        Prim (-1, D_None, [], None)
    | List_t t, items ->
        let items = List.map (unparse_data t) items in
        Prim (-1, D_List, items, None)
    | Set_t t, set ->
        let t = ty_of_comparable_ty t in
        let items =
          set_fold
            (fun item acc ->
               unparse_data t item :: acc )
            set [] in
        Prim (-1, D_Set, List.rev items, None)
    | Map_t (kt, vt), map ->
        let kt = ty_of_comparable_ty kt in
        let items =
          map_fold (fun k v acc ->
              Prim (-1, D_Item,
                    [ unparse_data kt k;
                      unparse_data vt v ],
                    None)
              :: acc)
            map [] in
        Prim (-1, D_Map, List.rev items, None)
    | Lambda_t _, Lam (_, original_code) ->
        root original_code

(* ---- Equality witnesses --------------------------------------------------*)

type ('ta, 'tb) eq = Eq : 'same * 'same -> ('same, 'same) eq

let eq
  : type t. t -> t -> (t, t) eq tzresult
  = fun ta tb -> Ok (Eq (ta, tb))

let comparable_ty_eq
  : type ta tb.
    ta comparable_ty -> tb comparable_ty ->
    (ta comparable_ty, tb comparable_ty) eq tzresult
  = fun ta tb -> match ta, tb with
    | Int_key, Int_key -> eq ta tb
    | Nat_key, Nat_key -> eq ta tb
    | String_key, String_key -> eq ta tb
    | Tez_key, Tez_key -> eq ta tb
    | Bool_key, Bool_key -> eq ta tb
    | Key_hash_key, Key_hash_key -> eq ta tb
    | Timestamp_key, Timestamp_key -> eq ta tb
    | _, _ -> error (Inconsistent_types (ty_of_comparable_ty ta, ty_of_comparable_ty tb))

let rec ty_eq
  : type ta tb. ta ty -> tb ty -> (ta ty, tb ty) eq tzresult
  = fun ta tb ->
    match ta, tb with
    | Unit_t, Unit_t -> eq ta tb
    | Int_t, Int_t -> eq ta tb
    | Nat_t, Nat_t -> eq ta tb
    | Key_t, Key_t -> eq ta tb
    | Key_hash_t, Key_hash_t -> eq ta tb
    | String_t, String_t -> eq ta tb
    | Signature_t, Signature_t -> eq ta tb
    | Tez_t, Tez_t -> eq ta tb
    | Timestamp_t, Timestamp_t -> eq ta tb
    | Bool_t, Bool_t -> eq ta tb
    | Map_t (tal, tar), Map_t (tbl, tbr) ->
        (comparable_ty_eq tal tbl >>? fun (Eq _) ->
         ty_eq tar tbr >>? fun (Eq _) ->
         (eq ta tb : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Set_t ea, Set_t eb ->
        (comparable_ty_eq ea eb >>? fun (Eq _) ->
         (eq ta tb : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Pair_t ((tal, _), (tar, _)),
      Pair_t ((tbl, _), (tbr, _)) ->
        (ty_eq tal tbl >>? fun (Eq _) ->
         ty_eq tar tbr >>? fun (Eq _) ->
         (eq ta tb : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Union_t ((tal, _), (tar, _)), Union_t ((tbl, _), (tbr, _)) ->
        (ty_eq tal tbl >>? fun (Eq _) ->
         ty_eq tar tbr >>? fun (Eq _) ->
         (eq ta tb : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Lambda_t (tal, tar), Lambda_t (tbl, tbr) ->
        (ty_eq tal tbl >>? fun (Eq _) ->
         ty_eq tar tbr >>? fun (Eq _) ->
         (eq ta tb : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Contract_t (tal, tar), Contract_t (tbl, tbr) ->
        (ty_eq tal tbl >>? fun (Eq _) ->
         ty_eq tar tbr >>? fun (Eq _) ->
         (eq ta tb : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | Option_t tva, Option_t tvb ->
        (ty_eq tva tvb >>? fun (Eq _) ->
         (eq ta tb : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | List_t tva, List_t tvb ->
        (ty_eq tva tvb >>? fun (Eq _) ->
         (eq ta tb : (ta ty, tb ty) eq tzresult)) |>
        record_trace (Inconsistent_types (ta, tb))
    | _, _ -> error (Inconsistent_types (ta, tb))

let rec stack_ty_eq
  : type ta tb. int -> ta stack_ty -> tb stack_ty ->
    (ta stack_ty, tb stack_ty) eq tzresult = fun lvl ta tb ->
  match ta, tb with
  | Item_t (tva, ra, _), Item_t (tvb, rb, _) ->
      ty_eq tva tvb |>
      record_trace (Bad_stack_item lvl) >>? fun  (Eq _) ->
      stack_ty_eq (lvl + 1) ra rb >>? fun (Eq _) ->
      (eq ta tb : (ta stack_ty, tb stack_ty) eq tzresult)
  | Empty_t, Empty_t -> eq ta tb
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
    | _, _ -> assert false

let error_unexpected_annot loc annot =
  match annot with
  | None -> ok ()
  | Some _ -> error (Unexpected_annotation loc)

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
      | Bool_t, Bool_t -> ok Bool_t
      | Map_t (tal, tar), Map_t (tbl, tbr) ->
          help tar tbr >>? fun value ->
          ty_eq tar value >>? fun (Eq _) ->
          ok (Map_t (merge_comparable_types tal tbl, value))
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
      | Contract_t (tal, tar), Contract_t (tbl, tbr) ->
          help tal tbl >>? fun left_ty ->
          help tar tbr >|? fun right_ty ->
          Contract_t (left_ty, right_ty)
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

(* ---- Type checker resuls -------------------------------------------------*)

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
    | Typed ({ aft = aftbt } as dbt), Typed ({ aft = aftbf } as dbf) ->
        let unmatched_branches = (Unmatched_branches (loc, aftbt, aftbf)) in
        trace
          unmatched_branches
          (Lwt.return (stack_ty_eq 1 aftbt aftbf) >>=? fun (Eq _) ->
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

let rec parse_comparable_ty : Script.node -> ex_comparable_ty tzresult = function
  | Prim (_, T_int, [], _) -> ok (Ex_comparable_ty Int_key)
  | Prim (_, T_nat, [], _) -> ok (Ex_comparable_ty Nat_key)
  | Prim (_, T_string, [], _) -> ok (Ex_comparable_ty String_key)
  | Prim (_, T_tez, [], _) -> ok (Ex_comparable_ty Tez_key)
  | Prim (_, T_bool, [], _) -> ok (Ex_comparable_ty Bool_key)
  | Prim (_, T_key_hash, [], _) -> ok (Ex_comparable_ty Key_hash_key)
  | Prim (_, T_timestamp, [], _) -> ok (Ex_comparable_ty Timestamp_key)
  | Prim (loc, (T_int | T_nat
               | T_string | T_tez | T_bool
               | T_key | T_timestamp as prim), l, _) ->
      error (Invalid_arity (loc, prim, 0, List.length l))
  | Prim (loc, (T_pair | T_or | T_set | T_map
               | T_list | T_option  | T_lambda
               | T_unit | T_signature  | T_contract), _, _) as expr ->
      parse_ty expr >>? fun (Ex_ty ty, _) ->
      error (Comparable_type_expected (loc, ty))
  | expr ->
      error @@ unexpected expr [] Type_namespace
        [ T_int ; T_nat ;
          T_string ; T_tez ; T_bool ;
          T_key ; T_key_hash ; T_timestamp ]

and parse_ty : Script.node -> (ex_ty * annot) tzresult = function
  | Prim (_, T_unit, [], annot) -> ok (Ex_ty Unit_t, annot)
  | Prim (_, T_int, [], annot) -> ok (Ex_ty (Int_t), annot)
  | Prim (_, T_nat, [], annot) -> ok (Ex_ty (Nat_t), annot)
  | Prim (_, T_string, [], annot) -> ok (Ex_ty String_t, annot)
  | Prim (_, T_tez, [], annot) -> ok (Ex_ty Tez_t, annot)
  | Prim (_, T_bool, [], annot) -> ok (Ex_ty Bool_t, annot)
  | Prim (_, T_key, [], annot) -> ok (Ex_ty Key_t, annot)
  | Prim (_, T_key_hash, [], annot) -> ok (Ex_ty Key_hash_t, annot)
  | Prim (_, T_timestamp, [], annot) -> ok (Ex_ty Timestamp_t, annot)
  | Prim (_, T_signature, [], annot) -> ok (Ex_ty Signature_t, annot)
  | Prim (loc, T_contract, [ utl; utr ], annot) ->
      parse_ty utl >>? fun (Ex_ty tl, left_annot) ->
      parse_ty utr >>? fun (Ex_ty tr, right_annot) ->
      error_unexpected_annot loc left_annot >>? fun () ->
      error_unexpected_annot loc right_annot >|? fun () ->
      (Ex_ty (Contract_t (tl, tr)), annot)
  | Prim (_, T_pair, [ utl; utr ], annot) ->
      parse_ty utl >>? fun (Ex_ty tl, left_annot) ->
      parse_ty utr >>? fun (Ex_ty tr, right_annot) ->
      ok (Ex_ty (Pair_t ((tl, left_annot), (tr, right_annot))), annot)
  | Prim (_, T_or, [ utl; utr ], annot) ->
      parse_ty utl >>? fun (Ex_ty tl, left_annot) ->
      parse_ty utr >|? fun (Ex_ty tr, right_annot) ->
      (Ex_ty (Union_t ((tl, left_annot), (tr, right_annot))), annot)
  | Prim (_, T_lambda, [ uta; utr ], annot) ->
      parse_ty uta >>? fun (Ex_ty ta, _) ->
      parse_ty utr >>? fun (Ex_ty tr, _) ->
      ok (Ex_ty (Lambda_t (ta, tr)), annot)
  | Prim (loc, T_option, [ ut ], annot) ->
      parse_ty ut >>? fun (Ex_ty t, opt_annot) ->
      error_unexpected_annot loc annot >|? fun () ->
      (Ex_ty (Option_t t), opt_annot)
  | Prim (loc, T_list, [ ut ], annot) ->
      parse_ty ut >>? fun (Ex_ty t, list_annot) ->
      error_unexpected_annot loc list_annot >>? fun () ->
      (ok (Ex_ty (List_t t), annot))
  | Prim (_, T_set, [ ut ], annot) ->
      parse_comparable_ty ut >>? fun (Ex_comparable_ty t) ->
      ok (Ex_ty (Set_t t), annot)
  | Prim (_, T_map, [ uta; utr ], annot) ->
      parse_comparable_ty uta >>? fun (Ex_comparable_ty ta) ->
      parse_ty utr >>? fun (Ex_ty tr, _) ->
      ok (Ex_ty (Map_t (ta, tr)), annot)
  | Prim (loc, (T_unit | T_signature
               | T_int | T_nat
               | T_string | T_tez | T_bool
               | T_key | T_key_hash | T_timestamp as prim), l, _) ->
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
          T_int ; T_nat ;
          T_string ; T_tez ; T_bool ;
          T_key ; T_key_hash ; T_timestamp ]

let comparable_ty_of_ty
  : type a. int -> a ty -> a comparable_ty tzresult
  = fun loc ty -> match ty with
    | Int_t -> ok Int_key
    | Nat_t -> ok Nat_key
    | String_t -> ok String_key
    | Tez_t -> ok Tez_key
    | Bool_t -> ok Bool_key
    | Key_hash_t -> ok Key_hash_key
    | Timestamp_t -> ok Timestamp_key
    | ty -> error (Comparable_type_expected (loc, ty))

let rec unparse_stack
  : type a. a stack_ty -> Script.expr list
  = function
    | Empty_t -> []
    | Item_t (ty, rest, annot) -> strip_locations (unparse_ty annot ty) :: unparse_stack rest

let rec parse_data
  : type a.
    ?type_logger: (int -> Script.expr list -> Script.expr list -> unit) ->
    context -> a ty -> Script.node -> a tzresult Lwt.t
  = fun ?type_logger ctxt ty script_data ->
    let error () =
      Invalid_constant (location script_data, strip_locations script_data, ty) in
    let traced body =
      trace (error ()) body in
    match ty, script_data with
    (* Unit *)
    | Unit_t, Prim (_, D_Unit, [], _) -> return ()
    | Unit_t, Prim (loc, D_Unit, l, _) ->
        traced (fail (Invalid_arity (loc, D_Unit, 0, List.length l)))
    | Unit_t, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Unit ]))
    (* Booleans *)
    | Bool_t, Prim (_, D_True, [], _) -> return true
    | Bool_t, Prim (_, D_False, [], _) -> return false
    | Bool_t, Prim (loc, (D_True | D_False as c), l, _) ->
        traced (fail (Invalid_arity (loc, c, 0, List.length l)))
    | Bool_t, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_True ; D_False ]))
    (* Strings *)
    | String_t, String (_, v) -> return v
    | String_t, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Integers *)
    | Int_t, Int (_, v) ->
        begin match Script_int.of_string v with
          | None -> fail (error ())
          | Some v -> return v
        end
    | Nat_t, Int (_, v) ->
        begin match Script_int.of_string v with
          | None -> fail (error ())
          | Some v ->
              if Compare.Int.(Script_int.compare v Script_int.zero >= 0) then
                return (Script_int.abs v)
              else fail (error ())
        end
    | Int_t, expr ->
        traced (fail (Invalid_kind (location expr, [ Int_kind ], kind expr)))
    | Nat_t, expr ->
        traced (fail (Invalid_kind (location expr, [ Int_kind ], kind expr)))
    (* Tez amounts *)
    | Tez_t, String (_, v) -> begin try
          match Tez.of_string v with
          | None -> raise Exit
          | Some tez -> return tez
        with _ ->
          fail @@ error ()
      end
    | Tez_t, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Timestamps *)
    | Timestamp_t, (Int (_, v)) -> begin
        match Script_timestamp.of_string v with
        | Some v -> return v
        | None -> fail (error ())
      end
    | Timestamp_t, String (_, s) -> begin try
          match Script_timestamp.of_string s with
          | Some v -> return v
          | None -> fail (error ())
        with _ -> fail (error ())
      end
    | Timestamp_t, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ; Int_kind ], kind expr)))
    (* IDs *)
    | Key_t, String (_, s) ->
        begin
          try
            return (Ed25519.Public_key.of_b58check_exn s)
          with _ -> fail (error ())
        end
    | Key_t, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    | Key_hash_t, String (_, s) ->
        begin
          try
            return (Ed25519.Public_key_hash.of_b58check_exn s)
          with _ -> fail (error ()) end
    | Key_hash_t, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Signatures *)
    | Signature_t, String (_, s) -> begin try
          match Data_encoding.Binary.of_bytes
                  Ed25519.Signature.encoding
                  (MBytes.of_string (Hex_encode.hex_decode s)) with
          | Some s -> return s
          | None -> raise Not_found
        with _ ->
          fail (error ())
      end
    | Signature_t, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Contracts *)
    | Contract_t (ty1, ty2), String (loc, s) ->
        traced @@
        (Lwt.return (Contract.of_b58check s)) >>=? fun c ->
        parse_contract ctxt ty1 ty2 loc c >>=? fun _ ->
        return (ty1, ty2, c)
    | Contract_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Pairs *)
    | Pair_t ((ta, _), (tb, _)), Prim (_, D_Pair, [ va; vb ], _) ->
        traced @@
        parse_data ?type_logger ctxt ta va >>=? fun va ->
        parse_data ?type_logger ctxt tb vb >>=? fun vb ->
        return (va, vb)
    | Pair_t _, Prim (loc, D_Pair, l, _) ->
        fail @@ Invalid_arity (loc, D_Pair, 2, List.length l)
    | Pair_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Pair ]))
    (* Unions *)
    | Union_t ((tl, _), _), Prim (_, D_Left, [ v ], _) ->
        traced @@
        parse_data ?type_logger ctxt tl v >>=? fun v ->
        return (L v)
    | Union_t _, Prim (loc, D_Left, l, _) ->
        fail @@ Invalid_arity (loc, D_Left, 1, List.length l)
    | Union_t (_, (tr, _)), Prim (_, D_Right, [ v ], _) ->
        traced @@
        parse_data ?type_logger ctxt tr v >>=? fun v ->
        return (R v)
    | Union_t _, Prim (loc, D_Right, l, _) ->
        fail @@ Invalid_arity (loc, D_Right, 1, List.length l)
    | Union_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Left ; D_Right ]))
    (* Lambdas *)
    | Lambda_t (ta, tr), (Seq _ as script_instr) ->
        traced @@
        parse_returning Lambda ?type_logger ctxt (ta, Some "@arg") tr script_instr
    | Lambda_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))
    (* Options *)
    | Option_t t, Prim (_, D_Some, [ v ], _) ->
        traced @@
        parse_data ?type_logger ctxt t v >>=? fun v ->
        return (Some v)
    | Option_t _, Prim (loc, D_Some, l, _) ->
        fail @@ Invalid_arity (loc, D_Some, 1, List.length l)
    | Option_t _, Prim (_, D_None, [], _) ->
        return None
    | Option_t _, Prim (loc, D_None, l, _) ->
        fail @@ Invalid_arity (loc, D_None, 0, List.length l)
    | Option_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Some ; D_None ]))
    (* Lists *)
    | List_t t, Prim (_, D_List, vs, _) ->
        traced @@
        fold_right_s
          (fun v rest ->
             parse_data ?type_logger ctxt t v >>=? fun v ->
             return (v :: rest))
          vs []
    | List_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_List ]))
    (* Sets *)
    | Set_t t, (Prim (loc, D_Set, vs, _) as expr) ->
        fold_left_s
          (fun (last_value, set) v ->
             parse_comparable_data ?type_logger ctxt t v >>=? fun v ->
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
             return (Some v, set_update v true set))
          (None, empty_set t) vs >>|? snd |> traced
    | Set_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Set ]))
    (* Maps *)
    | Map_t (tk, tv), (Prim (loc, D_Map, vs, _) as expr) ->
        (fold_left_s
           (fun (last_value, map) -> function
              | Prim (_, D_Item, [ k; v ], _) ->
                  parse_comparable_data ?type_logger ctxt tk k >>=? fun k ->
                  parse_data ?type_logger ctxt tv v >>=? fun v ->
                  begin match last_value with
                    | Some value ->
                        if Compare.Int.(0 <= (compare_comparable tk value k))
                        then
                          if Compare.Int.(0 = (compare_comparable tk value k))
                          then fail (Duplicate_map_keys (loc, strip_locations expr))
                          else fail (Unordered_map_keys (loc, strip_locations expr))
                        else return ()
                    | None -> return ()
                  end >>=? fun () ->
                  return (Some k, map_update k (Some v) map)
              | Prim (loc, D_Item, l, _) ->
                  fail @@ Invalid_arity (loc, D_Item, 2, List.length l)
              | Prim (loc, name, _, _) ->
                  fail @@ Invalid_primitive (loc, [ D_Item ], name)
              | Int _ | String _ | Seq _ ->
                  fail (error ()))
           (None, empty_map tk) vs) >>|? snd |> traced
    | Map_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Map ]))

and parse_comparable_data
  : type a. ?type_logger:(int -> Script.expr list -> Script.expr list -> unit) ->
    context -> a comparable_ty -> Script.node -> a tzresult Lwt.t
  = fun ?type_logger ctxt ty script_data ->
    parse_data ?type_logger ctxt (ty_of_comparable_ty ty) script_data

and parse_returning
  : type arg ret. tc_context -> context ->
    ?type_logger: (int -> Script.expr list -> Script.expr list -> unit) ->
    arg ty * annot -> ret ty -> Script.node -> (arg, ret) lambda tzresult Lwt.t =
  fun tc_context ctxt ?type_logger (arg, arg_annot) ret script_instr ->
    parse_instr tc_context ctxt ?type_logger
      script_instr (Item_t (arg, Empty_t, arg_annot)) >>=? function
    | Typed ({ loc ; aft = (Item_t (ty, Empty_t, _) as stack_ty) } as descr) ->
        trace
          (Bad_return (loc, stack_ty, ret))
          (Lwt.return (ty_eq ty ret)) >>=? fun (Eq _) ->
        return (Lam (descr, strip_locations script_instr) : (arg, ret) lambda)
    | Typed { loc ; aft = stack_ty } ->
        fail (Bad_return (loc, stack_ty, ret))
    | Failed { descr } ->
        return (Lam (descr (Item_t (ret, Empty_t, None)), strip_locations script_instr)
                : (arg, ret) lambda)

and parse_instr
  : type bef.
    tc_context ->
    context ->
    ?type_logger: (int -> Script.expr list -> Script.expr list -> unit) ->
    Script.node -> bef stack_ty -> bef judgement tzresult Lwt.t =
  fun tc_context ctxt ?type_logger script_instr stack_ty ->
    let return : bef judgement -> bef judgement tzresult Lwt.t = return in
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
    let typed loc (instr, aft) =
      begin match type_logger, script_instr with
        | None, _
        | Some _, (Seq (-1, _, _) | Int _ | String _) -> ()
        | Some log, (Prim _ | Seq _) ->
            log loc (unparse_stack stack_ty) (unparse_stack aft)
      end ;
      Typed { loc ; instr ; bef = stack_ty ; aft } in
    match script_instr, stack_ty with
    (* stack ops *)
    | Prim (loc, I_DROP, [], _),
      Item_t (_, rest, _) ->
        return (typed loc (Drop, rest))
    | Prim (loc, I_DUP, [], instr_annot),
      Item_t (v, rest, stack_annot) ->
        let annot = keep_or_rewrite_annot stack_annot instr_annot in
        return (typed loc (Dup, Item_t (v, Item_t (v, rest, stack_annot), annot)))
    | Prim (loc, I_SWAP, [], instr_annot),
      Item_t (v,  Item_t (w, rest, stack_annot), cur_top_annot) ->
        let annot = keep_or_rewrite_annot stack_annot instr_annot in
        return (typed loc (Swap, Item_t (w, Item_t (v, rest, cur_top_annot), annot)))
    | Prim (loc, I_PUSH, [ t ; d ], instr_annot),
      stack ->
        (Lwt.return (parse_ty t)) >>=? fun (Ex_ty t, _) ->
        parse_data ?type_logger ctxt t d >>=? fun v ->
        return (typed loc (Const v, Item_t (t, stack, instr_annot)))
    | Prim (loc, I_UNIT, [], instr_annot),
      stack ->
        return (typed loc (Const (), Item_t (Unit_t, stack, instr_annot)))
    (* options *)
    | Prim (loc, I_SOME, [], instr_annot),
      Item_t (t, rest, _) ->
        return (typed loc (Cons_some, Item_t (Option_t t, rest, instr_annot)))
    | Prim (loc, I_NONE, [ t ], instr_annot),
      stack ->
        (Lwt.return (parse_ty t)) >>=? fun (Ex_ty t, _) ->
        return (typed loc (Cons_none t, Item_t (Option_t t, stack, instr_annot)))
    | Prim (loc, I_IF_NONE, [ bt ; bf ], instr_annot),
      (Item_t (Option_t t, rest, _) as bef) ->
        check_kind [ Seq_kind ] bt >>=? fun () ->
        check_kind [ Seq_kind ] bf >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt bt rest >>=? fun btr ->
        parse_instr ?type_logger tc_context ctxt bf (Item_t (t, rest, instr_annot)) >>=? fun bfr ->
        let branch ibt ibf =
          { loc ; instr = If_none (ibt, ibf) ; bef ; aft = ibt.aft } in
        merge_branches loc btr bfr { branch }
    (* pairs *)
    | Prim (loc, I_PAIR, [], instr_annot),
      Item_t (a, Item_t (b, rest, snd_annot), fst_annot) ->
        return (typed loc (Cons_pair, Item_t (Pair_t((a, fst_annot), (b, snd_annot)), rest, instr_annot)))
    | Prim (loc, I_CAR, [], instr_annot),
      Item_t (Pair_t ((a, value_annot), _), rest, _) ->
        let annot = keep_or_rewrite_annot value_annot instr_annot in
        return (typed loc (Car, Item_t (a, rest, annot)))
    | Prim (loc, I_CDR, [], instr_annot),
      Item_t (Pair_t (_, (b, value_annot)), rest, _) ->
        let annot = keep_or_rewrite_annot value_annot instr_annot in
        return (typed loc (Cdr, Item_t (b, rest, annot)))
    (* unions *)
    | Prim (loc, I_LEFT, [ tr ], instr_annot),
      Item_t (tl, rest, stack_annot) ->
        (Lwt.return (parse_ty tr)) >>=? fun (Ex_ty tr, _) ->
        return (typed loc (Left, Item_t (Union_t ((tl, stack_annot), (tr, None)), rest, instr_annot)))
    | Prim (loc, I_RIGHT, [ tl ], instr_annot),
      Item_t (tr, rest, stack_annot) ->
        (Lwt.return (parse_ty tl)) >>=? fun (Ex_ty tl, _) ->
        return (typed loc (Right, Item_t (Union_t ((tl, None), (tr, stack_annot)), rest, instr_annot)))
    | Prim (loc, I_IF_LEFT, [ bt ; bf ], instr_annot),
      (Item_t (Union_t ((tl, left_annot), (tr, right_annot)), rest, _) as bef) ->
        check_kind [ Seq_kind ] bt >>=? fun () ->
        check_kind [ Seq_kind ] bf >>=? fun () ->
        fail_unexpected_annot loc instr_annot >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt bt (Item_t (tl, rest, left_annot)) >>=? fun btr ->
        parse_instr ?type_logger tc_context ctxt bf (Item_t (tr, rest, right_annot)) >>=? fun bfr ->
        let branch ibt ibf =
          { loc ; instr = If_left (ibt, ibf) ; bef ; aft = ibt.aft } in
        merge_branches loc btr bfr { branch }
    (* lists *)
    | Prim (loc, I_NIL, [ t ], instr_annot),
      stack ->
        (Lwt.return (parse_ty t)) >>=? fun (Ex_ty t, _) ->
        return (typed loc (Nil, Item_t (List_t t, stack, instr_annot)))
    | Prim (loc, I_CONS, [], instr_annot),
      Item_t (tv, Item_t (List_t t, rest, _), _) ->
        check_item_ty tv t loc I_CONS 1 2 >>=? fun (Eq _) ->
        return (typed loc (Cons_list, Item_t (List_t t, rest, instr_annot)))
    | Prim (loc, I_IF_CONS, [ bt ; bf ], instr_annot),
      (Item_t (List_t t, rest, stack_annot) as bef) ->
        check_kind [ Seq_kind ] bt >>=? fun () ->
        check_kind [ Seq_kind ] bf >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt bt
          (Item_t (t, Item_t (List_t t, rest, stack_annot), instr_annot)) >>=? fun btr ->
        parse_instr ?type_logger tc_context ctxt bf rest >>=? fun bfr ->
        let branch ibt ibf =
          { loc ; instr = If_cons (ibt, ibf) ; bef ; aft = ibt.aft } in
        merge_branches loc btr bfr { branch }
    | Prim (loc, I_SIZE, [], instr_annot),
      Item_t (List_t _, rest, _) ->
        return (typed loc (List_size, Item_t (Nat_t, rest, instr_annot)))
    | Prim (loc, I_MAP, [], instr_annot),
      Item_t (Lambda_t (param, ret), Item_t (List_t elt, rest, _), _) ->
        check_item_ty elt param loc I_MAP 2 2 >>=? fun (Eq _) ->
        return (typed loc (List_map, Item_t (List_t ret, rest, instr_annot)))
    | Prim (loc, I_MAP, [ body ], instr_annot),
      (Item_t (List_t elt, starting_rest, _)) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt body (Item_t (elt, starting_rest, None)) >>=? begin function
          | Typed ({ aft = Item_t (ret, rest, _) } as ibody) ->
              trace
                (Invalid_map_body (loc, ibody.aft))
                (Lwt.return (stack_ty_eq 1 rest starting_rest)) >>=? fun (Eq _) ->
              return (typed loc (List_map_body ibody, Item_t (List_t ret, rest, instr_annot)))
          | Typed { aft } -> fail (Invalid_map_body (loc, aft))
          | Failed _ -> fail (Invalid_map_block_fail loc)
        end
    | Prim (loc, I_REDUCE, [], instr_annot),
      Item_t (Lambda_t (Pair_t ((pelt, _), (pr, _)), r),
              Item_t (List_t elt, Item_t (init, rest, _), _), _) ->
        check_item_ty r pr loc I_REDUCE 1 3 >>=? fun (Eq _) ->
        check_item_ty elt pelt loc I_REDUCE 2 3 >>=? fun (Eq _) ->
        check_item_ty init r loc I_REDUCE 3 3 >>=? fun (Eq _) ->
        return (typed loc (List_reduce, Item_t (r, rest, instr_annot)))
    | Prim (loc, I_ITER, [ body ], instr_annot),
      Item_t (List_t elt, rest, _) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        fail_unexpected_annot loc instr_annot >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt body (Item_t (elt, rest, None)) >>=? begin function
          | Typed ({ aft } as ibody) ->
              trace
                (Invalid_iter_body (loc, rest, ibody.aft))
                (Lwt.return (stack_ty_eq 1 aft rest)) >>=? fun (Eq _) ->
              return (typed loc (List_iter ibody, rest))
          | Failed { descr } ->
              let ibody = descr rest in
              return (typed loc (List_iter ibody, rest))
        end
    (* sets *)
    | Prim (loc, I_EMPTY_SET, [ t ], instr_annot),
      rest ->
        (Lwt.return (parse_comparable_ty t)) >>=? fun (Ex_comparable_ty t) ->
        return (typed loc (Empty_set t, Item_t (Set_t t, rest, instr_annot)))
    | Prim (loc, I_MAP, [], instr_annot),
      Item_t (Lambda_t (param, ret), Item_t (Set_t elt, rest, _), _) ->
        let elt = ty_of_comparable_ty elt in
        (Lwt.return (comparable_ty_of_ty loc ret)) >>=? fun ret ->
        check_item_ty elt param loc I_MAP 1 2 >>=? fun (Eq _) ->
        return (typed loc (Set_map ret, Item_t (Set_t ret, rest, instr_annot)))
    | Prim (loc, I_REDUCE, [], instr_annot),
      Item_t (Lambda_t (Pair_t ((pelt, _), (pr, _)), r),
              Item_t (Set_t elt, Item_t (init, rest, _), _), _) ->
        let elt = ty_of_comparable_ty elt in
        check_item_ty r pr loc I_REDUCE 1 3 >>=? fun (Eq _) ->
        check_item_ty elt pelt loc I_REDUCE 2 3 >>=? fun (Eq _) ->
        check_item_ty init r loc I_REDUCE 3 3 >>=? fun (Eq _) ->
        return (typed loc (Set_reduce, Item_t (r, rest, instr_annot)))
    | Prim (loc, I_ITER, [ body ], annot),
      Item_t (Set_t comp_elt, rest, _) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        fail_unexpected_annot loc annot >>=? fun () ->
        let elt = ty_of_comparable_ty comp_elt in
        parse_instr ?type_logger tc_context ctxt body (Item_t (elt, rest, None)) >>=? begin function
          | Typed ({ aft } as ibody) ->
              trace
                (Invalid_iter_body (loc, rest, ibody.aft))
                (Lwt.return (stack_ty_eq 1 aft rest)) >>=? fun (Eq _) ->
              return (typed loc (Set_iter ibody, rest))
          | Failed { descr } ->
              let ibody = descr rest in
              return (typed loc (Set_iter ibody, rest))
        end
    | Prim (loc, I_MEM, [], instr_annot),
      Item_t (v, Item_t (Set_t elt, rest, _), _) ->
        let elt = ty_of_comparable_ty elt in
        check_item_ty elt v loc I_MEM 1 2 >>=? fun (Eq _) ->
        return (typed loc (Set_mem, Item_t (Bool_t, rest, instr_annot)))
    | Prim (loc, I_UPDATE, [], instr_annot),
      Item_t (v, Item_t (Bool_t, Item_t (Set_t elt, rest, _), _), _) ->
        let ty = ty_of_comparable_ty elt in
        check_item_ty ty v loc I_UPDATE 1 3 >>=? fun (Eq _) ->
        return (typed loc (Set_update, Item_t (Set_t elt, rest, instr_annot)))
    | Prim (loc, I_SIZE, [], instr_annot),
      Item_t (Set_t _, rest, _) ->
        return (typed loc (Set_size, Item_t (Nat_t, rest, instr_annot)))
    (* maps *)
    | Prim (loc, I_EMPTY_MAP, [ tk ; tv ], instr_annot),
      stack ->
        (Lwt.return (parse_comparable_ty tk)) >>=? fun (Ex_comparable_ty tk) ->
        (Lwt.return (parse_ty tv)) >>=? fun (Ex_ty tv, _) ->
        return (typed loc (Empty_map (tk, tv), Item_t (Map_t (tk, tv), stack, instr_annot)))
    | Prim (loc, I_MAP, [], instr_annot),
      Item_t (Lambda_t (Pair_t ((pk, _), (pv, _)), ret),
              Item_t (Map_t (ck, v), rest, _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty pk k loc I_MAP 1 2 >>=? fun (Eq _) ->
        check_item_ty pv v loc I_MAP 1 2 >>=? fun (Eq _) ->
        return (typed loc (Map_map, Item_t (Map_t (ck, ret), rest, instr_annot)))
    | Prim (loc, I_REDUCE, [], instr_annot),
      Item_t (Lambda_t (Pair_t ((Pair_t ((pk, _), (pv, _)), _), (pr, _)), r),
              Item_t (Map_t (ck, v),
                      Item_t (init, rest, _), _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty pk k loc I_REDUCE 2 3 >>=? fun (Eq _) ->
        check_item_ty pv v loc I_REDUCE 2 3 >>=? fun (Eq _) ->
        check_item_ty r pr loc I_REDUCE 1 3 >>=? fun (Eq _) ->
        check_item_ty init r loc I_REDUCE 3 3 >>=? fun (Eq _) ->
        return (typed loc (Map_reduce, Item_t (r, rest, instr_annot)))
    | Prim (loc, I_ITER, [ body ], instr_annot),
      Item_t (Map_t (comp_elt, element_ty), rest, _) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        fail_unexpected_annot loc instr_annot >>=? fun () ->
        let key = ty_of_comparable_ty comp_elt in
        parse_instr ?type_logger tc_context ctxt body
          (Item_t (Pair_t ((key, None), (element_ty, None)), rest, None))
        >>=? begin function
          | Typed ({ aft } as ibody) ->
              trace
                (Invalid_iter_body (loc, rest, ibody.aft))
                (Lwt.return (stack_ty_eq 1 aft rest)) >>=? fun (Eq _) ->
              return (typed loc (Map_iter ibody, rest))
          | Failed { descr } ->
              let ibody = descr rest in
              return (typed loc (Map_iter ibody, rest))
        end
    | Prim (loc, I_MEM, [], instr_annot),
      Item_t (vk, Item_t (Map_t (ck, _), rest, _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty vk k loc I_MEM 1 2 >>=? fun (Eq _) ->
        return (typed loc (Map_mem, Item_t (Bool_t, rest, instr_annot)))
    | Prim (loc, I_GET, [], instr_annot),
      Item_t (vk, Item_t (Map_t (ck, elt), rest, _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty vk k loc I_GET 1 2 >>=? fun (Eq _) ->
        return (typed loc (Map_get, Item_t (Option_t elt, rest, instr_annot)))
    | Prim (loc, I_UPDATE, [], instr_annot),
      Item_t (vk, Item_t (Option_t vv, Item_t (Map_t (ck, v), rest, _), _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty vk k loc I_UPDATE 1 3 >>=? fun (Eq _) ->
        check_item_ty vv v loc I_UPDATE 2 3 >>=? fun (Eq _) ->
        return (typed loc (Map_update, Item_t (Map_t (ck, v), rest, instr_annot)))
    | Prim (loc, I_SIZE, [], instr_annot),
      Item_t (Map_t (_, _), rest, _) ->
        return (typed loc (Map_size, Item_t (Nat_t, rest, instr_annot)))
    (* control *)
    | Seq (loc, [], annot),
      stack ->
        fail_unexpected_annot loc annot >>=? fun () ->
        return (typed loc (Nop, stack))
    | Seq (loc, [ single ], annot),
      stack ->
        fail_unexpected_annot loc annot >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt single stack >>=? begin function
          | Typed ({ aft } as instr) ->
              let nop = { bef = aft ; loc = loc ; aft ; instr = Nop } in
              return (typed loc (Seq (instr, nop), aft))
          | Failed { descr } ->
              let descr aft =
                let nop = { bef = aft ; loc = loc ; aft ; instr = Nop } in
                let descr = descr aft in
                { descr with instr = Seq (descr, nop) } in
              return (Failed { descr })
        end
    | Seq (loc, hd :: tl, annot),
      stack ->
        fail_unexpected_annot loc annot >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt hd stack >>=? begin function
          | Failed _ ->
              fail (Fail_not_in_tail_position (Micheline.location hd))
          | Typed ({ aft = middle } as ihd) ->
              parse_instr ?type_logger tc_context ctxt (Seq (-1, tl, None)) middle >>=? function
              | Failed { descr } ->
                  let descr ret =
                    { loc ; instr = Seq (ihd, descr ret) ;
                      bef = stack ; aft = ret } in
                  return (Failed { descr })
              | Typed itl ->
                  return (typed loc (Seq (ihd, itl), itl.aft))
        end
    | Prim (loc, I_IF, [ bt ; bf ], _),
      (Item_t (Bool_t, rest, _) as bef) ->
        check_kind [ Seq_kind ] bt >>=? fun () ->
        check_kind [ Seq_kind ] bf >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt bt rest >>=? fun btr ->
        parse_instr ?type_logger tc_context ctxt bf rest >>=? fun bfr ->
        let branch ibt ibf =
          { loc ; instr = If (ibt, ibf) ; bef ; aft = ibt.aft } in
        merge_branches loc btr bfr { branch }
    | Prim (loc, I_LOOP, [ body ], _),
      (Item_t (Bool_t, rest, stack_annot) as stack) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt body rest >>=? begin function
          | Typed ibody ->
              trace
                (Unmatched_branches (loc, ibody.aft, stack))
                (Lwt.return (stack_ty_eq 1 ibody.aft stack)) >>=? fun (Eq _) ->
              return (typed loc (Loop ibody, rest))
          | Failed { descr } ->
              let ibody = descr (Item_t (Bool_t, rest, stack_annot)) in
              return (typed loc (Loop ibody, rest))
        end
    | Prim (loc, I_LOOP_LEFT, [ body ], instr_annot),
      (Item_t (Union_t ((tl, tl_annot), (tr, tr_annot)), rest, _) as stack) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        fail_unexpected_annot loc instr_annot >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt body (Item_t (tl, rest, tl_annot)) >>=? begin function
          | Typed ibody ->
              trace
                (Unmatched_branches (loc, ibody.aft, stack))
                (Lwt.return (stack_ty_eq 1 ibody.aft stack)) >>=? fun (Eq _) ->
              return (typed loc (Loop_left ibody, (Item_t (tr, rest, tr_annot))))
          | Failed { descr } ->
              let ibody = descr (Item_t (Union_t ((tl, tl_annot), (tr, tr_annot)), rest, None)) in
              return (typed loc (Loop_left ibody, Item_t (tr, rest, tr_annot)))
        end
    | Prim (loc, I_LAMBDA, [ arg ; ret ; code ], instr_annot),
      stack ->
        (Lwt.return (parse_ty arg)) >>=? fun (Ex_ty arg, arg_annot) ->
        (Lwt.return (parse_ty ret)) >>=? fun (Ex_ty ret, _) ->
        check_kind [ Seq_kind ] code >>=? fun () ->
        parse_returning Lambda ?type_logger ctxt
          (arg, default_annot ~default:default_arg_annot arg_annot)
          ret code >>=? fun lambda ->
        return (typed loc (Lambda lambda, Item_t (Lambda_t (arg, ret), stack, instr_annot)))
    | Prim (loc, I_EXEC, [], instr_annot),
      Item_t (arg, Item_t (Lambda_t (param, ret), rest, _), _) ->
        check_item_ty arg param loc I_EXEC 1 2 >>=? fun (Eq _) ->
        return (typed loc (Exec, Item_t (ret, rest, instr_annot)))
    | Prim (loc, I_DIP, [ code ], instr_annot),
      Item_t (v, rest, stack_annot) ->
        fail_unexpected_annot loc instr_annot >>=? fun () ->
        check_kind [ Seq_kind ] code >>=? fun () ->
        parse_instr ?type_logger (add_dip v stack_annot tc_context) ctxt code rest >>=? begin function
          | Typed descr ->
              return (typed loc (Dip descr, Item_t (v, descr.aft, stack_annot)))
          | Failed _ ->
              fail (Fail_not_in_tail_position loc)
        end
    | Prim (loc, I_FAIL, [], annot),
      bef ->
        fail_unexpected_annot loc annot >>=? fun () ->
        let descr aft = { loc ; instr = Fail ; bef ; aft } in
        return (Failed { descr })
    (* timestamp operations *)
    | Prim (loc, I_ADD, [], instr_annot),
      Item_t (Timestamp_t, Item_t (Int_t, rest, _), _) ->
        return (typed loc (Add_timestamp_to_seconds, Item_t (Timestamp_t, rest, instr_annot)))
    | Prim (loc, I_ADD, [], instr_annot),
      Item_t (Int_t, Item_t (Timestamp_t, rest, _), _) ->
        return (typed loc (Add_seconds_to_timestamp, Item_t (Timestamp_t, rest, instr_annot)))
    | Prim (loc, I_SUB, [], instr_annot),
      Item_t (Timestamp_t, Item_t (Int_t, rest, _), _) ->
        return (typed loc (Sub_timestamp_seconds, Item_t (Timestamp_t, rest, instr_annot)))
    | Prim (loc, I_SUB, [], instr_annot),
      Item_t (Timestamp_t, Item_t (Timestamp_t, rest, _), _) ->
        return (typed loc (Diff_timestamps, Item_t (Int_t, rest, instr_annot)))
    (* string operations *)
    | Prim (loc, I_CONCAT, [], instr_annot),
      Item_t (String_t, Item_t (String_t, rest, _), _) ->
        return (typed loc (Concat, Item_t (String_t, rest, instr_annot)))
    (* currency operations *)
    | Prim (loc, I_ADD, [], instr_annot),
      Item_t (Tez_t, Item_t (Tez_t, rest, _), _) ->
        return (typed loc (Add_tez, Item_t (Tez_t, rest, instr_annot)))
    | Prim (loc, I_SUB, [], instr_annot),
      Item_t (Tez_t, Item_t (Tez_t, rest, _), _) ->
        return (typed loc (Sub_tez, Item_t (Tez_t, rest, instr_annot)))
    | Prim (loc, I_MUL, [], instr_annot),
      Item_t (Tez_t, Item_t (Nat_t, rest, _), _) ->
        return (typed loc (Mul_teznat, Item_t (Tez_t, rest, instr_annot)))
    | Prim (loc, I_MUL, [], instr_annot),
      Item_t (Nat_t, Item_t (Tez_t, rest, _), _) ->
        return (typed loc (Mul_nattez, Item_t (Tez_t, rest, instr_annot)))
    (* boolean operations *)
    | Prim (loc, I_OR, [], instr_annot),
      Item_t (Bool_t, Item_t (Bool_t, rest, _), _) ->
        return (typed loc (Or, Item_t (Bool_t, rest, instr_annot)))
    | Prim (loc, I_AND, [], instr_annot),
      Item_t (Bool_t, Item_t (Bool_t, rest, _), _) ->
        return (typed loc (And, Item_t (Bool_t, rest, instr_annot)))
    | Prim (loc, I_XOR, [], instr_annot),
      Item_t (Bool_t, Item_t (Bool_t, rest, _), _) ->
        return (typed loc (Xor, Item_t (Bool_t, rest, instr_annot)))
    | Prim (loc, I_NOT, [], instr_annot),
      Item_t (Bool_t, rest, _) ->
        return (typed loc (Not, Item_t (Bool_t, rest, instr_annot)))
    (* integer operations *)
    | Prim (loc, I_ABS, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        return (typed loc (Abs_int, Item_t (Nat_t, rest, instr_annot)))
    | Prim (loc, I_INT, [], instr_annot),
      Item_t (Nat_t, rest, _) ->
        return (typed loc (Int_nat, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_NEG, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        return (typed loc (Neg_int, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_NEG, [], instr_annot),
      Item_t (Nat_t, rest, _) ->
        return (typed loc (Neg_nat, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_ADD, [], instr_annot),
      Item_t (Int_t, Item_t (Int_t, rest, _), _) ->
        return (typed loc (Add_intint, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_ADD, [], instr_annot),
      Item_t (Int_t, Item_t (Nat_t, rest, _), _) ->
        return (typed loc (Add_intnat, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_ADD, [], instr_annot),
      Item_t (Nat_t, Item_t (Int_t, rest, _), _) ->
        return (typed loc (Add_natint, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_ADD, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        return (typed loc (Add_natnat, Item_t (Nat_t, rest, instr_annot)))
    | Prim (loc, I_SUB, [], instr_annot),
      Item_t (Int_t, Item_t (Int_t, rest, _), _) ->
        return (typed loc (Sub_int, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_SUB, [], instr_annot),
      Item_t (Int_t, Item_t (Nat_t, rest, _), _) ->
        return (typed loc (Sub_int, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_SUB, [], instr_annot),
      Item_t (Nat_t, Item_t (Int_t, rest, _), _) ->
        return (typed loc (Sub_int, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_SUB, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        return (typed loc (Sub_int, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_MUL, [], instr_annot),
      Item_t (Int_t, Item_t (Int_t, rest, _), _) ->
        return (typed loc (Mul_intint, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_MUL, [], instr_annot),
      Item_t (Int_t, Item_t (Nat_t, rest, _), _) ->
        return (typed loc (Mul_intnat, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_MUL, [], instr_annot),
      Item_t (Nat_t, Item_t (Int_t, rest, _), _) ->
        return (typed loc (Mul_natint, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_MUL, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        return (typed loc (Mul_natnat, Item_t (Nat_t, rest, instr_annot)))
    | Prim (loc, I_EDIV, [], instr_annot),
      Item_t (Tez_t, Item_t (Nat_t, rest, _), _) ->
        return
          (typed loc
             (Ediv_teznat,
              Item_t (Option_t (Pair_t ((Tez_t, None), (Tez_t, None))), rest, instr_annot)))
    | Prim (loc, I_EDIV, [], instr_annot),
      Item_t (Tez_t, Item_t (Tez_t, rest, _), _) ->
        return (typed loc (Ediv_tez,
                           Item_t (Option_t (Pair_t ((Nat_t, None), (Tez_t, None))), rest, instr_annot)))
    | Prim (loc, I_EDIV, [], instr_annot),
      Item_t (Int_t, Item_t (Int_t, rest, _), _) ->
        return
          (typed loc
             (Ediv_intint,
              Item_t (Option_t (Pair_t ((Int_t, None), (Nat_t, None))), rest, instr_annot)))
    | Prim (loc, I_EDIV, [], instr_annot),
      Item_t (Int_t, Item_t (Nat_t, rest, _), _) ->
        return
          (typed loc
             (Ediv_intnat,
              Item_t (Option_t (Pair_t ((Int_t, None), (Nat_t, None))), rest, instr_annot)))
    | Prim (loc, I_EDIV, [], instr_annot),
      Item_t (Nat_t, Item_t (Int_t, rest, _), _) ->
        return
          (typed loc
             (Ediv_natint,
              Item_t (Option_t (Pair_t ((Int_t, None), (Nat_t, None))), rest, instr_annot)))
    | Prim (loc, I_EDIV, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        return
          (typed loc
             (Ediv_natnat,
              Item_t (Option_t (Pair_t ((Nat_t, None), (Nat_t, None))), rest, instr_annot)))
    | Prim (loc, I_LSL, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        return (typed loc (Lsl_nat, Item_t (Nat_t, rest, instr_annot)))
    | Prim (loc, I_LSR, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        return (typed loc (Lsr_nat, Item_t (Nat_t, rest, instr_annot)))
    | Prim (loc, I_OR, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        return (typed loc (Or_nat, Item_t (Nat_t, rest, instr_annot)))
    | Prim (loc, I_AND, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        return (typed loc (And_nat, Item_t (Nat_t, rest, instr_annot)))
    | Prim (loc, I_XOR, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        return (typed loc (Xor_nat, Item_t (Nat_t, rest, instr_annot)))
    | Prim (loc, I_NOT, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        return (typed loc (Not_int, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_NOT, [], instr_annot),
      Item_t (Nat_t, rest, _) ->
        return (typed loc (Not_nat, Item_t (Int_t, rest, instr_annot)))
    (* comparison *)
    | Prim (loc, I_COMPARE, [], instr_annot),
      Item_t (Int_t, Item_t (Int_t, rest, _), _) ->
        return (typed loc (Compare Int_key, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_COMPARE, [], instr_annot),
      Item_t (Nat_t, Item_t (Nat_t, rest, _), _) ->
        return (typed loc (Compare Nat_key, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_COMPARE, [], instr_annot),
      Item_t (Bool_t, Item_t (Bool_t, rest, _), _) ->
        return (typed loc (Compare Bool_key, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_COMPARE, [], instr_annot),
      Item_t (String_t, Item_t (String_t, rest, _), _) ->
        return (typed loc (Compare String_key, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_COMPARE, [], instr_annot),
      Item_t (Tez_t, Item_t (Tez_t, rest, _), _) ->
        return (typed loc (Compare Tez_key, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_COMPARE, [], instr_annot),
      Item_t (Key_hash_t, Item_t (Key_hash_t, rest, _), _) ->
        return (typed loc (Compare Key_hash_key, Item_t (Int_t, rest, instr_annot)))
    | Prim (loc, I_COMPARE, [], instr_annot),
      Item_t (Timestamp_t, Item_t (Timestamp_t, rest, _), _) ->
        return (typed loc (Compare Timestamp_key, Item_t (Int_t, rest, instr_annot)))
    (* comparators *)
    | Prim (loc, I_EQ, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        return (typed loc (Eq, Item_t (Bool_t, rest, instr_annot)))
    | Prim (loc, I_NEQ, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        return (typed loc (Neq, Item_t (Bool_t, rest, instr_annot)))
    | Prim (loc, I_LT, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        return (typed loc (Lt, Item_t (Bool_t, rest, instr_annot)))
    | Prim (loc, I_GT, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        return (typed loc (Gt, Item_t (Bool_t, rest, instr_annot)))
    | Prim (loc, I_LE, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        return (typed loc (Le, Item_t (Bool_t, rest, instr_annot)))
    | Prim (loc, I_GE, [], instr_annot),
      Item_t (Int_t, rest, _) ->
        return (typed loc (Ge, Item_t (Bool_t, rest, instr_annot)))
    (* protocol *)
    | Prim (loc, I_MANAGER, [], instr_annot),
      Item_t (Contract_t _, rest, _) ->
        return (typed loc (Manager, Item_t (Key_hash_t, rest, instr_annot)))
    | Prim (loc, I_TRANSFER_TOKENS, [], instr_annot),
      Item_t (p, Item_t
                (Tez_t, Item_t
                   (Contract_t (cp, cr), Item_t
                      (storage, Empty_t, storage_annot), _), _), _) ->
        check_item_ty p cp loc I_TRANSFER_TOKENS 1 4 >>=? fun (Eq _) ->
        begin match tc_context with
          | Dip _ -> fail (Transfer_in_dip loc)
          | Lambda -> fail (Transfer_in_lambda loc)
          | Toplevel { storage_type } ->
              check_item_ty storage storage_type loc I_TRANSFER_TOKENS 3 4 >>=? fun (Eq _) ->
              return (typed loc (Transfer_tokens storage,
                                 Item_t (cr, Item_t (storage, Empty_t, storage_annot),
                                         instr_annot)))
        end
    | Prim (loc, I_CREATE_ACCOUNT, [], instr_annot),
      Item_t
        (Key_hash_t, Item_t
           (Option_t Key_hash_t, Item_t
              (Bool_t, Item_t
                 (Tez_t, rest, _), _), _), _) ->
        return (typed loc (Create_account,
                           Item_t (Contract_t (Unit_t, Unit_t), rest, instr_annot)))
    | Prim (loc, I_DEFAULT_ACCOUNT, [], instr_annot),
      Item_t (Key_hash_t, rest, _) ->
        return
          (typed loc (Default_account, Item_t (Contract_t (Unit_t, Unit_t), rest, instr_annot)))
    | Prim (loc, I_CREATE_CONTRACT, [], instr_annot),
      Item_t
        (Key_hash_t, Item_t
           (Option_t Key_hash_t, Item_t
              (Bool_t, Item_t
                 (Bool_t, Item_t
                    (Tez_t, Item_t
                       (Lambda_t (Pair_t ((p, _), (gp, _)),
                                  Pair_t ((r, _), (gr, _))), Item_t
                          (ginit, rest, _), _), _), _), _), _), _) ->
        check_item_ty gp gr loc I_CREATE_CONTRACT 5 7 >>=? fun (Eq _) ->
        check_item_ty ginit gp loc I_CREATE_CONTRACT 6 7 >>=? fun (Eq _) ->
        return (typed loc (Create_contract (gp, p, r),
                           Item_t (Contract_t (p, r), rest, instr_annot)))
    | Prim (loc, I_NOW, [], instr_annot),
      stack ->
        return (typed loc (Now, Item_t (Timestamp_t, stack, instr_annot)))
    | Prim (loc, I_AMOUNT, [], instr_annot),
      stack ->
        return (typed loc (Amount, Item_t (Tez_t, stack, instr_annot)))
    | Prim (loc, I_BALANCE, [], instr_annot),
      stack ->
        return (typed loc (Balance, Item_t (Tez_t, stack, instr_annot)))
    | Prim (loc, I_HASH_KEY, [], instr_annot),
      Item_t (Key_t, rest, _) ->
        return (typed loc (Hash_key, Item_t (Key_hash_t, rest, instr_annot)))
    | Prim (loc, I_CHECK_SIGNATURE, [], instr_annot),
      Item_t (Key_t, Item_t (Pair_t ((Signature_t, _), (String_t, _)), rest, _), _) ->
        return (typed loc (Check_signature, Item_t (Bool_t, rest, instr_annot)))
    | Prim (loc, I_H, [], instr_annot),
      Item_t (t, rest, _) ->
        return (typed loc (H t, Item_t (String_t, rest, instr_annot)))
    | Prim (loc, I_STEPS_TO_QUOTA, [], instr_annot),
      stack ->
        return (typed loc (Steps_to_quota, Item_t (Nat_t, stack, instr_annot)))
    | Prim (loc, I_SOURCE, [ ta; tb ], instr_annot),
      stack ->
        (Lwt.return (parse_ty ta)) >>=? fun (Ex_ty ta, _) ->
        (Lwt.return (parse_ty tb)) >>=? fun (Ex_ty tb, _) ->
        return (typed loc (Source (ta, tb), Item_t (Contract_t (ta, tb), stack, instr_annot)))
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
                 | I_DEFAULT_ACCOUNT | I_AMOUNT | I_BALANCE
                 | I_CHECK_SIGNATURE | I_HASH_KEY
                 | I_H | I_STEPS_TO_QUOTA
                 as name), (_ :: _ as l), _), _ ->
        fail (Invalid_arity (loc, name, 0, List.length l))
    | Prim (loc, (I_NONE | I_LEFT | I_RIGHT | I_NIL | I_MAP | I_ITER
                 | I_EMPTY_SET | I_DIP | I_LOOP | I_LOOP_LEFT
                 as name), ([]
                           | _ :: _ :: _ as l), _), _ ->
        fail (Invalid_arity (loc, name, 1, List.length l))
    | Prim (loc, (I_PUSH | I_IF_NONE | I_IF_LEFT | I_IF_CONS
                 | I_EMPTY_MAP | I_IF | I_SOURCE
                 as name), ([] | [ _ ]
                           | _ :: _ :: _ :: _ as l), _), _ ->
        fail (Invalid_arity (loc, name, 2, List.length l))
    | Prim (loc, I_LAMBDA, ([] | [ _ ] | [ _ ; _ ]
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
        fail (Bad_stack (loc, I_TRANSFER_TOKENS, 3, stack))
    | Prim (loc, (I_DROP | I_DUP | I_CAR | I_CDR | I_SOME | I_H | I_DIP
                 | I_IF_NONE | I_LEFT | I_RIGHT | I_IF_LEFT | I_IF
                 | I_LOOP | I_IF_CONS | I_MANAGER | I_DEFAULT_ACCOUNT
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
            I_DEFAULT_ACCOUNT ; I_CHECK_SIGNATURE ; I_H ; I_HASH_KEY ;
            I_STEPS_TO_QUOTA ;
            I_PUSH ; I_NONE ; I_LEFT ; I_RIGHT ; I_NIL ;
            I_EMPTY_SET ; I_DIP ; I_LOOP ;
            I_IF_NONE ; I_IF_LEFT ; I_IF_CONS ;
            I_EMPTY_MAP ; I_IF ; I_SOURCE ; I_LAMBDA ]

and parse_contract
  : type arg ret. context -> arg ty -> ret ty -> Script.location -> Contract.t ->
    (arg, ret) typed_contract tzresult Lwt.t
  = fun ctxt arg ret loc contract ->
    Contract.exists ctxt contract >>=? function
    | false -> fail (Invalid_contract (loc, contract))
    | true ->
        trace
          (Invalid_contract (loc, contract)) @@
        Contract.get_script ctxt contract >>=? function
        | None ->
            Lwt.return
              (ty_eq arg Unit_t >>? fun (Eq _) ->
               ty_eq ret Unit_t >>? fun (Eq _) ->
               let contract : (arg, ret) typed_contract =
                 (arg, ret, contract) in
               ok contract)
        | Some { code } ->
            Lwt.return
              (parse_toplevel code >>? fun (arg_type, ret_type, _, _) ->
               parse_ty arg_type >>? fun (Ex_ty targ, _) ->
               parse_ty ret_type >>? fun (Ex_ty tret, _) ->
               ty_eq targ arg >>? fun (Eq _) ->
               ty_eq tret ret >>? fun (Eq _) ->
               let contract : (arg, ret) typed_contract =
                 (arg, ret, contract) in
               ok contract)

and parse_toplevel
  : Script.expr -> (Script.node * Script.node * Script.node * Script.node) tzresult
  = fun toplevel -> match root toplevel with
    | Int (loc, _) -> error (Invalid_kind (loc, [ Seq_kind ], Int_kind))
    | String (loc, _) -> error (Invalid_kind (loc, [ Seq_kind ], String_kind))
    | Prim (loc, _, _, _) -> error (Invalid_kind (loc, [ Seq_kind ], Prim_kind))
    | Seq (_, fields, _) ->
        let rec find_fields p r s c fields =
          match fields with
          | [] -> ok (p, r, s, c)
          | Int (loc, _) :: _ -> error (Invalid_kind (loc, [ Prim_kind ], Int_kind))
          | String (loc, _) :: _ -> error (Invalid_kind (loc, [ Prim_kind ], String_kind))
          | Seq (loc, _, _) :: _ -> error (Invalid_kind (loc, [ Prim_kind ], Seq_kind))
          | Prim (loc, K_parameter, [ arg ], _) :: rest ->
              begin match p with
                | None -> find_fields (Some arg) r s c rest
                | Some _ -> error (Duplicate_field (loc, K_parameter))
              end
          | Prim (loc, K_return, [ arg ], _) :: rest ->
              begin match r with
                | None -> find_fields p (Some arg) s c rest
                | Some _ -> error (Duplicate_field (loc, K_return))
              end
          | Prim (loc, K_storage, [ arg ], _) :: rest ->
              begin match s with
                | None -> find_fields p r (Some arg) c rest
                | Some _ -> error (Duplicate_field (loc, K_storage))
              end
          | Prim (loc, K_code, [ arg ], _) :: rest ->
              begin match c with
                | None -> find_fields p r s (Some arg) rest
                | Some _ -> error (Duplicate_field (loc, K_code))
              end
          | Prim (loc, (K_parameter | K_return | K_storage | K_code as name), args, _) :: _ ->
              error (Invalid_arity (loc, name, 1, List.length args))
          | Prim (loc, name, _, _) :: _ ->
              let allowed = [ K_parameter ; K_return ; K_storage ; K_code ] in
              error (Invalid_primitive (loc, allowed, name))
        in
        find_fields None None None None fields >>? function
        | (None, _, _, _) -> error (Missing_field K_parameter)
        | (Some _, None, _, _) -> error (Missing_field K_return)
        | (Some _, Some _, None, _) -> error (Missing_field K_storage)
        | (Some _, Some _, Some _, None) -> error (Missing_field K_code)
        | (Some p, Some r, Some s, Some c) -> ok (p, r, s, c)

type ex_script = Ex_script : ('a, 'b, 'c) script -> ex_script

let parse_script
  : ?type_logger: (int -> Script.expr list -> Script.expr list -> unit) ->
    context -> Script.t -> ex_script tzresult Lwt.t
  = fun ?type_logger ctxt { code ; storage } ->
    Lwt.return (parse_toplevel code) >>=? fun (arg_type, ret_type, storage_type, code_field) ->
    trace
      (Ill_formed_type (Some "parameter", code, location arg_type))
      (Lwt.return (parse_ty arg_type)) >>=? fun (Ex_ty arg_type, param_annot) ->
    trace
      (Ill_formed_type (Some "return", code, location ret_type))
      (Lwt.return (parse_ty ret_type)) >>=? fun (Ex_ty ret_type, _) ->
    trace
      (Ill_formed_type (Some "storage", code, location storage_type))
      (Lwt.return (parse_ty storage_type)) >>=? fun (Ex_ty storage_type, storage_annot) ->
    let arg_type_full = Pair_t ((arg_type, default_annot ~default:default_param_annot param_annot),
                                (storage_type, default_annot ~default:default_storage_annot storage_annot)) in
    let ret_type_full = Pair_t ((ret_type, None), (storage_type, None)) in
    trace
      (Ill_typed_data (None, storage, storage_type))
      (parse_data ?type_logger ctxt storage_type (root storage)) >>=? fun storage ->
    trace
      (Ill_typed_contract (code, []))
      (parse_returning (Toplevel { storage_type }) ctxt ?type_logger (arg_type_full, None) ret_type_full code_field)
    >>=? fun code ->
    return (Ex_script { code; arg_type; ret_type; storage; storage_type })

let type_map_enc =
  let open Data_encoding in
  list
    (conv
       (fun (loc, (bef, aft)) -> (loc, bef, aft))
       (fun (loc, bef, aft) -> (loc, (bef, aft)))
       (obj3
          (req "location" Script.location_encoding)
          (req "stackBefore" (list Script.expr_encoding))
          (req "stackAfter" (list Script.expr_encoding))))

let typecheck_code
  : context -> Script.expr -> type_map tzresult Lwt.t
  = fun ctxt code ->
    Lwt.return (parse_toplevel code) >>=? fun (arg_type, ret_type, storage_type, code_field) ->
    let type_map = ref [] in
    (* TODO: annotation checking *)
    trace
      (Ill_formed_type (Some "parameter", code, location arg_type))
      (Lwt.return (parse_ty arg_type)) >>=? fun (Ex_ty arg_type, param_annot) ->
    trace
      (Ill_formed_type (Some "return", code, location ret_type))
      (Lwt.return (parse_ty ret_type)) >>=? fun (Ex_ty ret_type, _) ->
    trace
      (Ill_formed_type (Some "storage", code, location storage_type))
      (Lwt.return (parse_ty storage_type)) >>=? fun (Ex_ty storage_type, storage_annot) ->
    let arg_type_full = Pair_t ((arg_type, default_annot ~default:default_param_annot param_annot),
                                (storage_type, default_annot ~default:default_storage_annot storage_annot)) in
    let ret_type_full = Pair_t ((ret_type, None), (storage_type, None)) in
    let result =
      parse_returning
        (Toplevel { storage_type })
        ctxt
        ~type_logger: (fun loc bef aft -> type_map := (loc, (bef, aft)) :: !type_map)
        (arg_type_full, None) ret_type_full code_field in
    trace
      (Ill_typed_contract (code, !type_map))
      result >>=? fun (Lam _) ->
    return !type_map

let typecheck_data
  : ?type_logger: (int -> Script.expr list -> Script.expr list -> unit) ->
    context -> Script.expr * Script.expr -> unit tzresult Lwt.t
  = fun ?type_logger ctxt (data, exp_ty) ->
    trace
      (Ill_formed_type (None, exp_ty, 0))
      (Lwt.return (parse_ty (root exp_ty))) >>=? fun (Ex_ty exp_ty, _) ->
    trace
      (Ill_typed_data (None, data, exp_ty))
      (parse_data ?type_logger ctxt exp_ty (root data)) >>=? fun _ ->
    return ()

(* ---- Error registration --------------------------------------------------*)

let ex_ty_enc =
  Data_encoding.conv
    (fun (Ex_ty ty) -> strip_locations (unparse_ty None ty))
    (fun expr ->
       match parse_ty (root expr) with
       | Ok (Ex_ty ty, _) -> Ex_ty ty
       | _ -> Ex_ty Unit_t (* FIXME: ? *))
    Script.expr_encoding

let () =
  let open Data_encoding in
  let located enc =
    merge_objs
      (obj1 (req "location" Script.location_encoding))
      enc in
  let arity_enc =
    int8 in
  let namespace_enc =
    def "primitiveNamespace" @@
    describe
      ~title: "Primitive namespace"
      ~description:
        "One of the three possible namespaces of primitive \
         (data constructor, type name or instruction)." @@
    string_enum [ "type", Type_namespace ;
                  "constant", Constant_namespace ;
                  "instruction", Instr_namespace ] in
  let kind_enc =
    def "primitiveNamespace" @@
    describe
      ~title: "Expression kind"
      ~description:
        "One of the four possible kinds of expression \
         (integer, string, primitive application or sequence)." @@
    string_enum [ "integer", Int_kind ;
                  "string", String_kind ;
                  "primitiveApplication", Prim_kind ;
                  "sequence", Seq_kind ] in
  let ex_stack_ty_enc =
    let rec unfold = function
      | Ex_stack_ty (Item_t (ty, rest, annot)) ->
          (Ex_ty ty, annot) :: unfold (Ex_stack_ty rest)
      | Ex_stack_ty Empty_t -> [] in
    let rec fold = function
      | (Ex_ty ty, annot) :: rest ->
          let Ex_stack_ty rest = fold rest in
          Ex_stack_ty (Item_t (ty, rest, annot))
      | [] -> Ex_stack_ty Empty_t in
    conv unfold fold (list (tup2 ex_ty_enc (option string))) in
  (* -- Structure errors ---------------------- *)
  register_error_kind
    `Permanent
    ~id:"invalidArityTypeError"
    ~title: "Invalid arity (typechecking error)"
    ~description:
      "In a script or data expression, a primitive was applied \
       to an unsupported number of arguments."
    (located (obj3
                (req "primitiveName" Script.prim_encoding)
                (req "expectedArity" arity_enc)
                (req "wrongArity" arity_enc)))
    (function
      | Invalid_arity (loc, name, exp, got) ->
          Some (loc, (name, exp, got))
      | _ -> None)
    (fun (loc, (name, exp, got)) ->
       Invalid_arity (loc, name, exp, got)) ;
  register_error_kind
    `Permanent
    ~id:"missingScriptField"
    ~title:"Script is missing a field (parse error)"
    ~description:
      "When parsing script, a field was expected, but not provided"
    (obj1 (req "prim" prim_encoding))
    (function Missing_field prim -> Some prim | _ -> None)
    (fun prim -> Missing_field prim) ;
  register_error_kind
    `Permanent
    ~id:"invalidPrimitiveTypeError"
    ~title: "Invalid primitive (typechecking error)"
    ~description:
      "In a script or data expression, a primitive was unknown."
    (located (obj2
                (dft "expectedPrimitiveNames" (list prim_encoding) [])
                (req "wrongPrimitiveName" prim_encoding)))
    (function
      | Invalid_primitive (loc, exp, got) -> Some (loc, (exp, got))
      | _ -> None)
    (fun (loc, (exp, got)) ->
       Invalid_primitive (loc, exp, got)) ;
  register_error_kind
    `Permanent
    ~id:"invalidExpressionKindTypeError"
    ~title: "Invalid expression kind (typechecking error)"
    ~description:
      "In a script or data expression, an expression was of the wrong kind \
       (for instance a string where only a primitive applications can appear)."
    (located (obj2
                (req "expectedKinds" (list kind_enc))
                (req "wrongKind" kind_enc)))
    (function
      | Invalid_kind (loc, exp, got) -> Some (loc, (exp, got))
      | _ -> None)
    (fun (loc, (exp, got)) ->
       Invalid_kind (loc, exp, got)) ;
  register_error_kind
    `Permanent
    ~id:"invalidPrimitiveNamespaceTypeError"
    ~title: "Invalid primitive namespace (typechecking error)"
    ~description:
      "In a script or data expression, a primitive was of the wrong namespace."
    (located (obj3
                (req "primitiveName" prim_encoding)
                (req "expectedNamespace" namespace_enc)
                (req "wrongNamespace" namespace_enc)))
    (function
      | Invalid_namespace (loc, name, exp, got) -> Some (loc, (name, exp, got))
      | _ -> None)
    (fun (loc, (name, exp, got)) ->
       Invalid_namespace (loc, name, exp, got)) ;
  register_error_kind
    `Permanent
    ~id:"unorderedMapLiteral"
    ~title:"Invalid map key order"
    ~description:"Map keys must be in strictly increasing order"
    (obj2
       (req "location" Script.location_encoding)
       (req "item" Script.expr_encoding))
    (function
      | Unordered_map_keys (loc, expr) -> Some (loc, expr)
      | _ -> None)
    (fun (loc, expr) -> Unordered_map_keys (loc, expr));
  register_error_kind
    `Permanent
    ~id:"duplicateMapKeys"
    ~title:"Duplicate map keys"
    ~description:"Map literals cannot contain duplicated keys"
    (obj2
       (req "location" Script.location_encoding)
       (req "item" Script.expr_encoding))
    (function
      | Duplicate_map_keys (loc, expr) -> Some (loc, expr)
      | _ -> None)
    (fun (loc, expr) -> Duplicate_map_keys (loc, expr));
  register_error_kind
    `Permanent
    ~id:"unorderedSetLiteral"
    ~title:"Invalid set value order"
    ~description:"Set values must be in strictly increasing order"
    (obj2
       (req "location" Script.location_encoding)
       (req "value" Script.expr_encoding))
    (function
      | Unordered_set_values (loc, expr) -> Some (loc, expr)
      | _ -> None)
    (fun (loc, expr) -> Unordered_set_values (loc, expr));
  register_error_kind
    `Permanent
    ~id:"duplicateSetValuesInLiteral"
    ~title:"Sets literals cannot contain duplicate elements"
    ~description:"Set literals cannot contain duplicate elements, \
                  but a duplicae was found while parsing."
    (obj2
       (req "location" Script.location_encoding)
       (req "value" Script.expr_encoding))
    (function
      | Duplicate_set_values (loc, expr) -> Some (loc, expr)
      | _ -> None)
    (fun (loc, expr) -> Duplicate_set_values (loc, expr));
  (* -- Instruction typing errors ------------- *)
  register_error_kind
    `Permanent
    ~id:"failNotInTailPositionTypeError"
    ~title: "FAIL not in tail position (typechecking error)"
    ~description:
      "There is non trivial garbage code after a FAIL instruction."
    (located empty)
    (function
      | Fail_not_in_tail_position loc -> Some (loc, ())
      | _ -> None)
    (fun (loc, ()) ->
       Fail_not_in_tail_position loc) ;
  register_error_kind
    `Permanent
    ~id:"undefinedBinopTypeError"
    ~title: "Undefined binop (typechecking error)"
    ~description:
      "A binary operation is called on operands of types \
       over which it is not defined."
    (located (obj3
                (req "operatorName" prim_encoding)
                (req "wrongLeftOperandType" ex_ty_enc)
                (req "wrongRightOperandType" ex_ty_enc)))
    (function
      | Undefined_binop (loc, n, tyl, tyr) ->
          Some (loc, (n, Ex_ty tyl, Ex_ty tyr))
      | _ -> None)
    (fun (loc, (n, Ex_ty tyl, Ex_ty tyr)) ->
       Undefined_binop (loc, n, tyl, tyr)) ;
  register_error_kind
    `Permanent
    ~id:"undefinedUnopTypeError"
    ~title: "Undefined unop (typechecking error)"
    ~description:
      "A unary operation is called on an operand of type \
       over which it is not defined."
    (located (obj2
                (req "operatorName" prim_encoding)
                (req "wrongOperandType" ex_ty_enc)))
    (function
      | Undefined_unop (loc, n, ty) ->
          Some (loc, (n, Ex_ty ty))
      | _ -> None)
    (fun (loc, (n, Ex_ty ty)) ->
       Undefined_unop (loc, n, ty)) ;
  register_error_kind
    `Permanent
    ~id:"badReturnTypeError"
    ~title: "Bad return (typechecking error)"
    ~description:
      "Unexpected stack at the end of a lambda or script."
    (located (obj2
                (req "expectedReturnType" ex_ty_enc)
                (req "wrongStackType" ex_stack_ty_enc)))
    (function
      | Bad_return (loc, sty, ty) -> Some (loc, (Ex_ty ty, Ex_stack_ty sty))
      | _ -> None)
    (fun (loc, (Ex_ty ty, Ex_stack_ty sty)) ->
       Bad_return (loc, sty, ty)) ;
  register_error_kind
    `Permanent
    ~id:"badStackTypeError"
    ~title: "Bad stack (typechecking error)"
    ~description:
      "The stack has an unexpected length or contents."
    (located (obj3
                (req "primitiveName" prim_encoding)
                (req "relevantStackPortion" int16)
                (req "wrongStackType" ex_stack_ty_enc)))
    (function
      | Bad_stack (loc, name, s, sty) -> Some (loc, (name, s, Ex_stack_ty sty))
      | _ -> None)
    (fun (loc, (name, s, Ex_stack_ty sty)) ->
       Bad_stack (loc, name, s, sty)) ;
  register_error_kind
    `Permanent
    ~id:"inconsistentAnnotations"
    ~title:"Annotations inconsistent between branches"
    ~description:"The annotations on two types could not be merged"
    (obj2
       (req "annot1" string)
       (req "annot2" string))
    (function Inconsistent_annotations (annot1, annot2) -> Some (annot1, annot2)
            | _ -> None)
    (fun (annot1, annot2) -> Inconsistent_annotations (annot1, annot2)) ;
  register_error_kind
    `Permanent
    ~id:"inconsistentTypeAnnotations"
    ~title:"Types contain inconsistent annotations"
    ~description:"The two types contain annotations that do not match"
    (located (obj2
                (req "type1" ex_ty_enc)
                (req "type2" ex_ty_enc)))
    (function
      | Inconsistent_type_annotations (loc, ty1, ty2) -> Some (loc, (Ex_ty ty1, Ex_ty ty2))
      | _ -> None)
    (fun (loc, (Ex_ty ty1, Ex_ty ty2)) -> Inconsistent_type_annotations (loc, ty1, ty2)) ;
  register_error_kind
    `Permanent
    ~id:"unexpectedAnnotation"
    ~title:"An annotation was encountered where no annotation is expected"
    ~description:"A node in the syntax tree was impropperly annotated"
    (located empty)
    (function Unexpected_annotation loc -> Some (loc, ())
            | _ -> None)
    (fun (loc, ()) -> Unexpected_annotation loc);
  register_error_kind
    `Permanent
    ~id:"unmatchedBranchesTypeError"
    ~title: "Unmatched branches (typechecking error)"
    ~description:
      "At the join point at the end of two code branches \
       the stacks have inconsistent lengths or contents."
    (located (obj2
                (req "firstStackType" ex_stack_ty_enc)
                (req "otherStackType" ex_stack_ty_enc)))
    (function
      | Unmatched_branches (loc, stya, styb) ->
          Some (loc, (Ex_stack_ty stya, Ex_stack_ty styb))
      | _ -> None)
    (fun (loc, (Ex_stack_ty stya, Ex_stack_ty styb)) ->
       Unmatched_branches (loc, stya, styb)) ;
  register_error_kind
    `Permanent
    ~id:"badStackItemTypeError"
    ~title: "Bad stack item (typechecking error)"
    ~description:
      "The type of a stack item is unexpected \
       (this error is always accompanied by a more precise one)."
    (obj1 (req "itemLevel" int16))
    (function
      | Bad_stack_item n -> Some n
      | _ -> None)
    (fun n ->
       Bad_stack_item n) ;
  register_error_kind
    `Permanent
    ~id:"TransferInLambdaTypeError"
    ~title: "Transfer in lambda (typechecking error)"
    ~description:
      "A TRANSFER_TOKENS instruction was encountered in a lambda expression."
    (located empty)
    (function
      | Transfer_in_lambda loc -> Some (loc, ())
      | _ -> None)
    (fun (loc, ()) ->
       Transfer_in_lambda loc) ;
  register_error_kind
    `Permanent
    ~id:"inconsistentStackLengthsTypeError"
    ~title: "Inconsistent stack lengths (typechecking error)"
    ~description:
      "A stack was of an unexpected length \
       (this error is always in the context of a located error)."
    empty
    (function
      | Bad_stack_length -> Some ()
      | _ -> None)
    (fun () ->
       Bad_stack_length) ;
  (* -- Value typing errors ------------------- *)
  register_error_kind
    `Permanent
    ~id:"invalidConstantTypeError"
    ~title: "Invalid constant (typechecking error)"
    ~description:
      "A data expression was invalid for its expected type."
    (located (obj2
                (req "expectedType" ex_ty_enc)
                (req "wrongExpression" Script.expr_encoding)))
    (function
      | Invalid_constant (loc, expr, ty) ->
          Some (loc, (Ex_ty ty, expr))
      | _ -> None)
    (fun (loc, (Ex_ty ty, expr)) ->
       Invalid_constant (loc, expr, ty)) ;
  register_error_kind
    `Permanent
    ~id:"invalidContractTypeError"
    ~title: "Invalid contract (typechecking error)"
    ~description:
      "A script or data expression references a contract that does not \
       exist or assumes a wrong type for an existing contract."
    (located (obj1 (req "contract" Contract.encoding)))
    (function
      | Invalid_contract (loc, c) ->
          Some (loc, c)
      | _ -> None)
    (fun (loc, c) ->
       Invalid_contract (loc, c)) ;
  register_error_kind
    `Permanent
    ~id:"comparableTypeExpectedTypeError"
    ~title: "Comparable type expected (typechecking error)"
    ~description:
      "A non comparable type was used in a place where \
       only comparable types are accepted."
    (located (obj1 (req "wrongType" ex_ty_enc)))
    (function
      | Comparable_type_expected (loc, ty) -> Some (loc, Ex_ty ty)
      | _ -> None)
    (fun (loc, Ex_ty ty) ->
       Comparable_type_expected (loc, ty)) ;
  register_error_kind
    `Permanent
    ~id:"InconsistentTypesTypeError"
    ~title: "Inconsistent types (typechecking error)"
    ~description:
      "This is the basic type clash error, \
       that appears in several places where the equality of \
       two types have to be proven, it is always accompanied \
       with another error that provides more context."
    (obj2
       (req "firstType" ex_ty_enc)
       (req "otherType" ex_ty_enc))
    (function
      | Inconsistent_types (tya, tyb) ->
          Some (Ex_ty tya, Ex_ty tyb)
      | _ -> None)
    (fun (Ex_ty tya, Ex_ty tyb) ->
       Inconsistent_types (tya, tyb)) ;
  register_error_kind
    `Permanent
    ~id:"invalidMapBody"
    ~title: "Invalid map body"
    ~description:
      "The body of a map block did not match the expected type"
    (obj2
       (req "loc" Script.location_encoding)
       (req "bodyType" ex_stack_ty_enc))
    (function
      | Invalid_map_body (loc, stack) ->
          Some (loc, Ex_stack_ty stack)
      | _ -> None)
    (fun (loc, Ex_stack_ty stack) ->
       Invalid_map_body (loc, stack)) ;
  register_error_kind
    `Permanent
    ~id:"invalidMapBlockFail"
    ~title:"FAIL instruction occurred as body of map block"
    ~description:"FAIL cannot be the only instruction in the body.\
                  The propper type of the return list cannot be inferred."
    (obj1 (req "loc" Script.location_encoding))
    (function
      | Invalid_map_block_fail loc -> Some loc
      | _ -> None)
    (fun loc -> Invalid_map_block_fail loc) ;
  register_error_kind
    `Permanent
    ~id:"invalidIterBody"
    ~title:"ITER body returned wrong stack type"
    ~description:"The body of an ITER instruction\
                  must result in the same stack type as before\
                  the ITER."
    (obj3
       (req "loc" Script.location_encoding)
       (req "befStack" ex_stack_ty_enc)
       (req "aftStack" ex_stack_ty_enc))
    (function
      | Invalid_iter_body (loc, bef, aft) -> Some (loc, Ex_stack_ty bef, Ex_stack_ty aft)
      | _ -> None)
    (fun (loc, Ex_stack_ty bef, Ex_stack_ty aft) -> Invalid_iter_body (loc, bef, aft)) ;
  (* Toplevel errors *)
  register_error_kind
    `Permanent
    ~id:"illTypedDataTypeError"
    ~title: "Ill typed data (typechecking error)"
    ~description:
      "The toplevel error thrown when trying to typecheck \
       a data expression against a given type \
       (always followed by more precise errors)."
    (obj3
       (opt "identifier" string)
       (req "expectedType" ex_ty_enc)
       (req "illTypedExpression" Script.expr_encoding))
    (function
      | Ill_typed_data (name, expr, ty) -> Some (name, Ex_ty ty,  expr)
      | _ -> None)
    (fun (name, Ex_ty ty,  expr) ->
       Ill_typed_data (name, expr, ty)) ;
  (* type error += Ill_formed_type of string option * Script.expr *)
  register_error_kind
    `Permanent
    ~id:"illFormedTypeTypeError"
    ~title: "Ill formed type (typechecking error)"
    ~description:
      "The toplevel error thrown when trying to parse a type expression \
       (always followed by more precise errors)."
    (obj3
       (opt "identifier" string)
       (req "illFormedExpression" Script.expr_encoding)
       (req "location" Script.location_encoding))
    (function
      | Ill_formed_type (name, expr, loc) -> Some (name, expr, loc)
      | _ -> None)
    (fun (name, expr, loc) ->
       Ill_formed_type (name, expr, loc)) ;
  register_error_kind
    `Permanent
    ~id:"illTypedContractTypeError"
    ~title: "Ill typed contract (typechecking error)"
    ~description:
      "The toplevel error thrown when trying to typecheck \
       a contract code against given input, output and storage types \
       (always followed by more precise errors)."
    (obj2
       (req "illTypedCode" Script.expr_encoding)
       (req "typeMap" type_map_enc))
    (function
      | Ill_typed_contract (expr, type_map) ->
          Some (expr, type_map)
      | _ -> None)
    (fun (expr, type_map) ->
       Ill_typed_contract (expr, type_map))
