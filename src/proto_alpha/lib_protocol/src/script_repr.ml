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

type annot = Micheline.annot

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
  | Bytes (_, s) -> (1, 1 + (MBytes.length s + 7) / 8)
  | Prim (_, _, args, annot) ->
      List.fold_left
        (fun (blocks, words) node ->
           let (nblocks, nwords) = node_size node in
           (blocks + 1 + nblocks, words + 2 + nwords))
        (match annot with
         | [] -> (1, 2)
         | annots ->
             let annots_length = List.fold_left (fun acc s -> acc + String.length s) 0 annots in
             (1, 4 + (annots_length + 7) / 8))
        args
  | Seq (_, args) ->
      List.fold_left
        (fun (blocks, words) node ->
           let (nblocks, nwords) = node_size node in
           (blocks + 1 + nblocks, words + 2 + nwords))
        (1, 2)
        args

let expr_size expr =
  node_size (Micheline.root expr)

let traversal_cost expr =
  let blocks, _words = expr_size expr in
  Gas_limit_repr.step_cost blocks

let deserialized_cost expr =
  let open Gas_limit_repr in
  let blocks, words = expr_size expr in
  ((Compare.Int.max 0 (blocks - 1)) *@ alloc_cost 0) +@
  alloc_cost words +@
  step_cost blocks

let serialized_cost bytes =
  let open Gas_limit_repr in
  alloc_bytes_cost (MBytes.length bytes)

let force_decode lexpr =
  match Data_encoding.force_decode lexpr with
  | Some v ->
      let deserialize_cost =
        Data_encoding.fold_lazy
          (fun _ -> Gas_limit_repr.free)
          (fun _ -> deserialized_cost v)
          (fun c_free _ -> c_free)
          lexpr in
      ok (v, deserialize_cost)
  | None -> error Lazy_script_decode

let force_bytes expr =
  let open Gas_limit_repr in
  match Data_encoding.force_bytes expr with
  | bytes ->
      let serialize_cost =
        Data_encoding.fold_lazy
          (fun v -> traversal_cost v +@ serialized_cost bytes)
          (fun _ -> Gas_limit_repr.free)
          (fun _ c_free -> c_free)
          expr in
      ok (bytes, serialize_cost)
  | exception _ -> error Lazy_script_decode

let minimal_deserialize_cost lexpr =
  let open Gas_limit_repr in
  Data_encoding.fold_lazy
    (fun _ -> Gas_limit_repr.free)
    (fun b -> alloc_bytes_cost (MBytes.length b))
    (fun c_free _ -> c_free)
    lexpr
