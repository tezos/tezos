(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* A smart contract is some code and some storage. The storage has a
  type and an initial value. The code is the code itself, the types of
  its arguments, the type of its result, and the type of the storage
  it is using.

  All of them are expressed in a simple [expr] type, combining
  [Int] (integer constant), [String] (string constant), [Prim]
  (a generic primitive for most operations) and [Seq] a sequence
  of operations.
 *)

type location =
  int

type expr =
  | Int of location * string
  | String of location * string
  | Prim of location * string * expr list * string option
  | Seq of location * expr list * string option

type code =
  { code : expr ;
    arg_type : expr ;
    ret_type : expr ;
    storage_type : expr }

type storage =
  { storage : expr ;
    storage_type : expr }

type t =
  { code : code ;
    storage : storage }

val location_encoding : location Data_encoding.t
val expr_encoding : expr Data_encoding.t
val storage_encoding : storage Data_encoding.t
val code_encoding : code Data_encoding.t
val encoding : t Data_encoding.t

val hash_expr : expr -> string
