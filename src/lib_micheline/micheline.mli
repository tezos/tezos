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

type annot = string list

(** The abstract syntax tree of Micheline expressions. The first
    parameter is used to contain locations, but can also embed custom
    data. The second parameter is the type of primitive names. *)
type ('l, 'p) node =
  | Int of 'l * Z.t
  | String of 'l * string
  | Bytes of 'l * MBytes.t
  | Prim of 'l * 'p * ('l, 'p) node list * annot
  | Seq of 'l * ('l, 'p) node list

(** Encoding for expressions, as their {!canonical} encoding.
    Locations are stored in a side table.
    See {!canonical_encoding} for the [variant] parameter. *)
val table_encoding : variant:string ->
  'l Data_encoding.encoding -> 'p Data_encoding.encoding ->
  ('l, 'p) node Data_encoding.encoding

(** Encoding for expressions, as their {!canonical} encoding.
    Locations are erased when serialized, and restored to a provided
    default value when deserialized.
    See {!canonical_encoding} for the [variant] parameter. *)
val erased_encoding : variant:string ->
  'l -> 'p Data_encoding.encoding -> ('l, 'p) node Data_encoding.encoding

(** Extract the location of the node. *)
val location : ('l, 'p) node -> 'l

(** Extract the annotations of the node. *)
val annotations : ('l, 'p) node -> string list

(** Expression form using canonical integer numbering as
    locations. The root has number zero, and each node adds one in the
    order of infix traversal. To be used when locations are not
    important, or when one wants to attach properties to nodes in an
    expression without rewriting it (using an indirection table with
    canonical locations as keys). *)
type 'p canonical

(** Canonical integer locations that appear inside {!canonical} expressions. *)
type canonical_location = int

(** Encoding for canonical integer locations. *)
val canonical_location_encoding : canonical_location Data_encoding.encoding

(** Encoding for expressions in canonical form. The first parameter
    is a name used to produce named definitions in the schemas. Make
    sure to use different names if two expression variants with
    different primitive encodings are used in the same schema. *)
val canonical_encoding : variant:string -> 'l Data_encoding.encoding -> 'l canonical Data_encoding.encoding

(** Old version of {!canonical_encoding} for retrocompatibility.
    Do not use in new code. *)
val canonical_encoding_v0 : variant:string -> 'l Data_encoding.encoding -> 'l canonical Data_encoding.encoding

(** Alias for {!canonical_encoding}. *)
val canonical_encoding_v1 : variant:string -> 'l Data_encoding.encoding -> 'l canonical Data_encoding.encoding

(** Compute the canonical form of an expression.
    Drops the concrete locations completely. *)
val strip_locations : (_, 'p) node -> 'p canonical

(** Give the root node of an expression in canonical form. *)
val root : 'p canonical -> (canonical_location, 'p) node

(** Compute the canonical form of an expression.
    Saves the concrete locations in an association list. *)
val extract_locations : ('l, 'p) node -> 'p canonical * (canonical_location * 'l) list

(** Transforms an expression in canonical form into a polymorphic one.
    Takes a mapping function to inject the concrete locations. *)
val inject_locations : (canonical_location -> 'l) -> 'p canonical -> ('l, 'p) node

(** Copies the tree, updating its primitives. *)
val map : ('a -> 'b) -> 'a canonical -> 'b canonical

(** Copies the tree, updating its primitives and locations. *)
val map_node : ('la -> 'lb) -> ('pa -> 'pb) -> ('la, 'pa) node -> ('lb, 'pb) node
