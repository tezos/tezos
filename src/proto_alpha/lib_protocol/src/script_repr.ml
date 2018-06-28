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

let int_node_size_of_numbits n =
  (1, 1 + (n + 63) / 64)
let int_node_size n =
  int_node_size_of_numbits (Z.numbits n)
let string_node_size_of_length s =
  (1, 1 + (s + 7) / 8)
let string_node_size s =
  string_node_size_of_length (String.length s)
let bytes_node_size_of_length s =
  (* approx cost of indirection to the C heap *)
  (2, 1 + (s + 7) / 8 + 12)
let bytes_node_size s =
  bytes_node_size_of_length (MBytes.length s)
let prim_node_size_nonrec_of_lengths n_args annots =
  let annots_length = List.fold_left (fun acc s -> acc + String.length s) 0 annots in
  if Compare.Int.(annots_length = 0) then
    (1 + n_args, 2 + 2 * n_args)
  else
    (2 + n_args, 4 + 2 * n_args + (annots_length + 7) / 8)
let prim_node_size_nonrec args annots =
  let n_args = List.length args in
  prim_node_size_nonrec_of_lengths n_args annots
let seq_node_size_nonrec_of_length n_args =
  (1 + n_args, 2 + 2 * n_args)
let seq_node_size_nonrec args =
  let n_args = List.length args in
  seq_node_size_nonrec_of_length n_args

let rec node_size node =
  let open Micheline in
  match node with
  | Int (_, n) -> int_node_size n
  | String (_, s) -> string_node_size s
  | Bytes (_, s) -> bytes_node_size s
  | Prim (_, _, args, annot) ->
      List.fold_left
        (fun (blocks, words) node ->
           let (nblocks, nwords) = node_size node in
           (blocks + nblocks, words + nwords))
        (prim_node_size_nonrec args annot)
        args
  | Seq (_, args) ->
      List.fold_left
        (fun (blocks, words) node ->
           let (nblocks, nwords) = node_size node in
           (blocks + nblocks, words + nwords))
        (seq_node_size_nonrec args)
        args

let expr_size expr =
  node_size (Micheline.root expr)

let traversal_cost expr =
  let blocks, _words = expr_size expr in
  Gas_limit_repr.step_cost blocks

let node_cost (blocks, words) =
  let open Gas_limit_repr in
  ((Compare.Int.max 0 (blocks - 1)) *@ alloc_cost 0) +@
  alloc_cost words +@
  step_cost blocks

let int_node_cost n = node_cost (int_node_size n)
let int_node_cost_of_numbits n = node_cost (int_node_size_of_numbits n)
let string_node_cost s = node_cost (string_node_size s)
let string_node_cost_of_length s = node_cost (string_node_size_of_length s)
let bytes_node_cost s = node_cost (bytes_node_size s)
let bytes_node_cost_of_length s = node_cost (bytes_node_size_of_length s)
let prim_node_cost_nonrec args annot = node_cost (prim_node_size_nonrec args annot)
let prim_node_cost_nonrec_of_length n_args annot = node_cost (prim_node_size_nonrec_of_lengths n_args annot)
let seq_node_cost_nonrec args = node_cost (seq_node_size_nonrec args)
let seq_node_cost_nonrec_of_length n_args = node_cost (seq_node_size_nonrec_of_length n_args)

let deserialized_cost expr =
  node_cost (expr_size expr)

let serialized_cost bytes =
  let open Gas_limit_repr in
  alloc_cost 12 +@ alloc_bytes_cost (MBytes.length bytes)

let force_decode lexpr =
  match Data_encoding.force_decode lexpr with
  | Some v ->
      let deserialize_cost =
        Data_encoding.apply_lazy
          ~fun_value:(fun _ -> Gas_limit_repr.free)
          ~fun_bytes:(fun _ -> deserialized_cost v)
          ~fun_combine:(fun c_free _ -> c_free)
          lexpr in
      ok (v, deserialize_cost)
  | None -> error Lazy_script_decode

let force_bytes expr =
  let open Gas_limit_repr in
  match Data_encoding.force_bytes expr with
  | bytes ->
      let serialize_cost =
        Data_encoding.apply_lazy
          ~fun_value:(fun v -> traversal_cost v +@ serialized_cost bytes)
          ~fun_bytes:(fun _ -> Gas_limit_repr.free)
          ~fun_combine:(fun _ c_free -> c_free)
          expr in
      ok (bytes, serialize_cost)
  | exception _ -> error Lazy_script_decode

let minimal_deserialize_cost lexpr =
  Data_encoding.apply_lazy
    ~fun_value:(fun _ -> Gas_limit_repr.free)
    ~fun_bytes:(fun b -> serialized_cost b)
    ~fun_combine:(fun c_free _ -> c_free)
    lexpr
