(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_hash

(* Tezos protocol "bootstrap" - untyped script representation *)

type location = int

let location_encoding =
  let open Data_encoding in
  def
    "scriptLocation" @@
  describe
    ~title:
      "Script location"
    ~description:
      "The location of a node in a script (code, data or type) \
       as its index in the expression tree in prefix order, with \
       zero being the root and adding one for every basic node, \
       sequence and primitive application." @@
  int31

type expr = (* TODO: turn the location into an alpha ? *)
  | Int of location * string
  | Float of location * string
  | String of location * string
  | Prim of location * string * expr list
  | Seq of location * expr list

let expr_encoding =
  let open Data_encoding in
  let int_encoding =
    obj1 (req "int" string) in
  let float_encoding =
    obj1 (req "float" string) in
  let string_encoding =
    obj1 (req "string" string) in
  let prim_encoding expr_encoding =
    let json =
      union
        [ case string
            (function (v, []) -> Some v |  _ -> None)
            (fun v -> (v, [])) ;
          case (assoc (list expr_encoding))
            (fun (v, args) -> Some [ (v, args) ])
            (function
              | [ (v, args) ] -> (v, args)
              | _ -> Json.cannot_destruct "invalid script expression") ] in
    let binary =
      obj2 (req "prim" string) (req "args" (list expr_encoding)) in
    splitted ~json ~binary in
  let seq_encoding expr_encoding =
    list expr_encoding in
  mu "tezosScriptExpression" (fun expr_encoding ->
      describe
        ~title: "Script expression (data, type or code)" @@
      union ~tag_size:`Uint8
        [ case ~tag:0 int_encoding
            (function Int (_, v) -> Some v | _ -> None)
            (fun v -> Int (-1, v)) ;
          case ~tag:1 float_encoding
            (function Float (_, v) -> Some v | _ -> None)
            (fun v -> Float (-1, v)) ;
          case ~tag:2 string_encoding
            (function String (_, v) -> Some v | _ -> None)
            (fun v -> String (-1, v)) ;
          case ~tag:3 (prim_encoding expr_encoding)
            (function
              | Prim (_, v, args) -> Some (v, args)
              | _ -> None)
            (function (prim, args) -> Prim (-1, prim, args)) ;
          case ~tag:4 (seq_encoding expr_encoding)
            (function Seq (_, v) -> Some v | _ -> None)
            (fun args -> Seq (-1, args)) ])

let update_locations ir =
  let rec update_locations i = function
    | Int (_, v) -> (Int (i, v), succ i)
    | Float (_, v) -> (Float (i, v), succ i)
    | String (_, v) -> (String (i, v), succ i)
    | Prim (_, name, args) ->
        let (nargs, ni) =
          List.fold_left (fun (nargs, ni) arg ->
              let narg, ni = update_locations ni arg in
              (narg :: nargs, ni))
            ([], succ i) args in
        (Prim (i, name, List.rev nargs), ni)
    | Seq (_, args) ->
        let (nargs, ni) =
          List.fold_left (fun (nargs, ni) arg ->
              let narg, ni = update_locations ni arg in
              (narg :: nargs, ni))
            ([], succ i) args in
        (Seq (i, List.rev nargs), ni) in
  fst (update_locations 0 ir)

let expr_encoding =
  Data_encoding.conv
    (fun to_write -> to_write)
    (fun just_read -> update_locations just_read)
    expr_encoding

type code =
  { code : expr ;
    arg_type : expr ;
    ret_type : expr ;
    storage_type : expr }

type storage =
  { storage : expr ;
    storage_type : expr }

let storage_cost _ = Tez_repr.of_cents_exn 50L (* FIXME *)
let code_cost _ = Tez_repr.of_cents_exn 50L (* FIXME *)

open Data_encoding

let storage_encoding =
  conv
    (fun { storage ; storage_type } -> (storage, storage_type))
    (fun (storage, storage_type) -> { storage ; storage_type })
    (obj2
       (req "storage" expr_encoding)
       (req "storageType" expr_encoding))

let code_encoding =
  conv
    (fun { code; arg_type; ret_type; storage_type } ->
       (code, arg_type, ret_type, storage_type))
    (fun (code, arg_type, ret_type, storage_type) ->
       { code; arg_type; ret_type; storage_type })
    (obj4
       (req "code" expr_encoding)
       (req "argType" expr_encoding)
       (req "retType" expr_encoding)
       (req "storageType" expr_encoding))

let hash_expr data =
  let bytes = Data_encoding.Binary.to_bytes expr_encoding data in
  Script_expr_hash.(hash_bytes [ bytes ] |> to_b48check)

type t =
  | No_script
  | Script of {
      code: code ;
      storage: storage ;
    }

let encoding =
  let open Data_encoding in
  union ~tag_size:`Uint8 [
    case ~tag:0 empty
      (function No_script -> Some () | _ -> None)
      (fun () -> No_script) ;
    case ~tag:1
      (obj2
         (req "code" code_encoding)
         (req "storage" storage_encoding))
      (function Script { code ; storage } -> Some (code, storage) | _ -> None)
      (fun (code, storage) -> Script { code ; storage })
  ]
