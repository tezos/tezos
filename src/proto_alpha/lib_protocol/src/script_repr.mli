(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

type location = Micheline.canonical_location

type annot = Micheline.annot

type expr = Michelson_v1_primitives.prim Micheline.canonical

type error += Lazy_script_decode (* `Permanent *)

type lazy_expr = expr Data_encoding.lazy_t

type node = (location, Michelson_v1_primitives.prim) Micheline.node

val location_encoding : location Data_encoding.t

val expr_encoding : expr Data_encoding.t

val lazy_expr_encoding : lazy_expr Data_encoding.t

val lazy_expr : expr -> lazy_expr

type t = { code : lazy_expr ; storage : lazy_expr }

val encoding : t Data_encoding.encoding

val deserialized_cost : expr -> Gas_limit_repr.cost

val serialized_cost : MBytes.t -> Gas_limit_repr.cost
val traversal_cost : node -> Gas_limit_repr.cost
val node_cost : node -> Gas_limit_repr.cost

val int_node_cost : Z.t -> Gas_limit_repr.cost
val int_node_cost_of_numbits : int -> Gas_limit_repr.cost
val string_node_cost : string -> Gas_limit_repr.cost
val string_node_cost_of_length : int -> Gas_limit_repr.cost
val bytes_node_cost : MBytes.t -> Gas_limit_repr.cost
val bytes_node_cost_of_length : int -> Gas_limit_repr.cost
val prim_node_cost_nonrec : expr list -> annot -> Gas_limit_repr.cost
val prim_node_cost_nonrec_of_length : int -> annot -> Gas_limit_repr.cost
val seq_node_cost_nonrec : expr list -> Gas_limit_repr.cost
val seq_node_cost_nonrec_of_length : int -> Gas_limit_repr.cost

val force_decode : lazy_expr -> (expr * Gas_limit_repr.cost) tzresult

val force_bytes : lazy_expr -> (MBytes.t * Gas_limit_repr.cost) tzresult

val minimal_deserialize_cost : lazy_expr -> Gas_limit_repr.cost
