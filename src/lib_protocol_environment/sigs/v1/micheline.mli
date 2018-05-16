(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type annot = string list

type ('l, 'p) node =
  | Int of 'l * Z.t
  | String of 'l * string
  | Prim of 'l * 'p * ('l, 'p) node list * annot
  | Seq of 'l * ('l, 'p) node list

type 'p canonical
type canonical_location = int

val root : 'p canonical -> (canonical_location, 'p) node
val canonical_location_encoding : canonical_location Data_encoding.encoding
val canonical_encoding : variant:string -> 'l Data_encoding.encoding -> 'l canonical Data_encoding.encoding
val erased_encoding : variant:string -> 'l -> 'p Data_encoding.encoding -> ('l, 'p) node Data_encoding.encoding
val table_encoding : variant:string -> 'l Data_encoding.encoding -> 'p Data_encoding.encoding -> ('l, 'p) node Data_encoding.encoding

val location : ('l, 'p) node -> 'l
val annotations : ('l, 'p) node -> string list

val strip_locations : (_, 'p) node -> 'p canonical
val extract_locations : ('l, 'p) node -> 'p canonical * (canonical_location * 'l) list
val inject_locations : (canonical_location -> 'l) -> 'p canonical -> ('l, 'p) node
