(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type location = Micheline.canonical_location

let location_encoding = Micheline.canonical_location_encoding

type expr = Michelson_v1_primitives.prim Micheline.canonical

type lazy_expr = expr Data_encoding.lazy_t

type node = (location, Michelson_v1_primitives.prim) Micheline.node

let expr_encoding =
  Micheline.canonical_encoding
    ~variant:"michelson_v1"
    Michelson_v1_primitives.prim_encoding

type error += Lazy_script_decode (* `Permanent *)

let () =
  register_error_kind `Permanent
    ~id:"invalid_binary_format"
    ~title:"Invalid binary format"
    ~description:"Could not deserialize some piece of data \
                  from its binary representation"
    Data_encoding.empty
    (function Lazy_script_decode -> Some () | _ -> None)
    (fun () -> Lazy_script_decode)

let lazy_expr_encoding =
  Data_encoding.lazy_encoding expr_encoding

let lazy_expr expr =
  Data_encoding.make_lazy expr_encoding expr

let force_decode expr =
  match Data_encoding.force_decode expr with
  | Some v -> ok v
  | None -> error Lazy_script_decode

let force_bytes expr =
  match Data_encoding.force_bytes expr with
  | bytes -> ok bytes
  | exception _ -> error Lazy_script_decode

type t = {
  code : lazy_expr ;
  storage : lazy_expr
}

let encoding =
  let open Data_encoding in
  def "scripted.contracts" @@
  conv
    (fun { code ; storage } -> (code, storage))
    (fun (code, storage) -> { code ; storage })
    (obj2
       (req "code" lazy_expr_encoding)
       (req "storage" lazy_expr_encoding))

let rec node_size node =
  let open Micheline in
  match node with
  | Int (_, n) -> (1, 1 + (Z.numbits n + 63) / 64)
  | String (_, s) -> (1, 1 + (String.length s + 7) / 8)
  | Prim (_, _, args, annot) ->
      List.fold_left
        (fun (blocks, words) node ->
           let (nblocks, nwords) = node_size node in
           (blocks + 1 + nblocks, words + 2 + nwords))
        (match annot with
         | None -> (1, 2)
         | Some annot -> (1, 4 + (String.length annot + 7) / 8))
        args
  | Seq (_, args, annot) ->
      List.fold_left
        (fun (blocks, words) node ->
           let (nblocks, nwords) = node_size node in
           (blocks + 1 + nblocks, words + 2 + nwords))
        (match annot with
         | None -> (1, 2)
         | Some annot -> (1, 3 + (String.length annot + 7) / 8))
        args

let expr_size expr =
  node_size (Micheline.root expr)

let expr_cost expr =
  let blocks, words = expr_size expr in
  Gas_limit_repr.(((Compare.Int.max 0 (blocks - 1)) *@ alloc_cost 0) +@ alloc_cost words)
