(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type location = Micheline.canonical_location

type expr = Michelson_v1_primitives.prim Micheline.canonical

type error += Lazy_script_decode (* `Permanent *)

type lazy_expr = expr Data_encoding.lazy_t

val force_decode : lazy_expr -> expr tzresult

val force_bytes : lazy_expr -> MBytes.t tzresult

type node = (location, Michelson_v1_primitives.prim) Micheline.node

val location_encoding : location Data_encoding.t

val expr_encoding : expr Data_encoding.t

val lazy_expr_encoding : lazy_expr Data_encoding.t

val lazy_expr : expr -> lazy_expr

type t = { code : lazy_expr ; storage : lazy_expr }

val encoding : t Data_encoding.encoding
