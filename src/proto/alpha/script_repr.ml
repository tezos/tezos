(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Tezos_hash

type location = Micheline.canonical_location

let location_encoding = Micheline.canonical_location_encoding

type expr = Michelson_v1_primitives.prim Micheline.canonical

type node = (location, Michelson_v1_primitives.prim) Micheline.node

let expr_encoding = Micheline.canonical_encoding Michelson_v1_primitives.prim_encoding

let hash_expr data =
  let bytes = Data_encoding.Binary.to_bytes expr_encoding data in
  Script_expr_hash.(hash_bytes [ bytes ] |> to_b58check)

type t = { code : expr ; storage : expr }

let encoding =
  let open Data_encoding in
  conv
    (fun { code ; storage } -> (code, storage))
    (fun (code, storage) -> { code ; storage })
    (obj2 (req "code" expr_encoding) (req "storage" expr_encoding))
