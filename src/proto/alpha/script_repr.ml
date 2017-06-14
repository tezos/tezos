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
  | String of location * string
  | Prim of location * string * expr list * string option
  | Seq of location * expr list * string option

let expr_encoding =
  let open Data_encoding in
  let int_encoding =
    obj1 (req "int" string) in
  let string_encoding =
    obj1 (req "string" string) in
  let prim_encoding expr_encoding =
    let json =
      union
        [ case string
            (function (v, [], None) -> Some v |  _ -> None)
            (fun v -> (v, [], None)) ;
          case (assoc (tup2 (list expr_encoding) (option string)))
            (fun (v, args, annot) -> Some [ (v, (args, annot)) ])
            (function
              | [ (v, (args, annot)) ] -> (v, args, annot)
              | _ -> Json.cannot_destruct "invalid script expression") ] in
    let binary =
      obj3 (req "prim" string) (req "args" (list expr_encoding)) (opt "annot" string) in
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
          case ~tag:1 string_encoding
            (function String (_, v) -> Some v | _ -> None)
            (fun v -> String (-1, v)) ;
          case ~tag:2 (prim_encoding expr_encoding)
            (function
              | Prim (_, v, args, annot) -> Some (v, args, annot)
              | _ -> None)
            (function (prim, args, annot) -> Prim (-1, prim, args, annot)) ;
          case ~tag:3 (seq_encoding expr_encoding)
            (function Seq (_, v, _annot) -> Some v | _ -> None)
            (fun args -> Seq (-1, args, None)) ])

let update_locations ir =
  let rec update_locations i = function
    | Int (_, v) -> (Int (i, v), succ i)
    | String (_, v) -> (String (i, v), succ i)
    | Prim (_, name, args, annot) ->
        let (nargs, ni) =
          List.fold_left (fun (nargs, ni) arg ->
              let narg, ni = update_locations ni arg in
              (narg :: nargs, ni))
            ([], succ i) args in
        (Prim (i, name, List.rev nargs, annot), ni)
    | Seq (_, args, annot) ->
        let (nargs, ni) =
          List.fold_left (fun (nargs, ni) arg ->
              let narg, ni = update_locations ni arg in
              (narg :: nargs, ni))
            ([], succ i) args in
        (Seq (i, List.rev nargs, annot), ni) in
  fst (update_locations 1 ir)

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
  Script_expr_hash.(hash_bytes [ bytes ] |> to_b58check)

type t =
  { code : code ;
    storage : storage }

let encoding =
  let open Data_encoding in
  conv
    (function { code ; storage } -> (code, storage))
    (fun (code, storage) -> { code ; storage })
    (obj2
       (req "code" code_encoding)
       (req "storage" storage_encoding))
