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
  conv
    (fun { code ; storage } -> (code, storage))
    (fun (code, storage) -> { code ; storage })
    (obj2
       (req "code" lazy_expr_encoding)
       (req "storage" lazy_expr_encoding))
