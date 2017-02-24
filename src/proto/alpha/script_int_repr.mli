(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* sign *)
type signed = Signed
type unsigned = Unsigned

(* length *)
type eight = Eight
type sixteen = Sixteen
type thirtytwo = Thirtytwo
type sixtyfour = Sixtyfour

(* int values *)
type ('s, 'l) int_val = Int of repr and repr

(* int types *)
type ('s, 'l) int_kind =
  | Int8 : (signed, eight) int_kind
  | Uint8 : (unsigned, eight) int_kind
  | Int16 : (signed, sixteen) int_kind
  | Uint16 : (unsigned, sixteen) int_kind
  | Int32 : (signed, thirtytwo) int_kind
  | Uint32 : (unsigned, thirtytwo) int_kind
  | Int64 : (signed, sixtyfour) int_kind
  | Uint64 : (unsigned, sixtyfour) int_kind

(* homogeneous operator types *)
type ('s, 'l) binop =
  ('s, 'l) int_kind -> ('s, 'l) int_val -> ('s, 'l) int_val -> ('s, 'l) int_val
type ('s, 'l) unop =
  ('s, 'l) int_kind -> ('s, 'l) int_val -> ('s, 'l) int_val
type ('s, 'l) checked_binop =
  ('s, 'l) int_kind -> ('s, 'l) int_val -> ('s, 'l) int_val -> ('s, 'l) int_val option
type ('s, 'l) checked_unop =
  ('s, 'l) int_kind -> ('s, 'l) int_val -> ('s, 'l) int_val option
type ('s, 'l) shift =
  ('s, 'l) int_kind -> ('s, 'l) int_val -> ('s, eight) int_val -> ('s, 'l) int_val

(* cast operators *)
val cast : ('tos, 'tol) int_kind -> ('s, 'l) int_val -> ('tos, 'tol) int_val
val checked_cast : ('tos, 'tol) int_kind -> ('s, 'l) int_val -> ('tos, 'tol) int_val option

(* to native int64s *)
val to_int64 : ('s, 'l) int_kind -> ('s, 'l) int_val -> int64
val of_int64 : ('s, 'l) int_kind -> int64 -> ('s, 'l) int_val
val checked_of_int64 : ('s, 'l) int_kind -> int64 -> ('s, 'l) int_val option

(* arithmetics *)
val abs : (signed, 'l) unop
val neg : (signed, 'l) unop
val add : ('s, 'l) binop
val sub : ('s, 'l) binop
val mul : ('s, 'l) binop
val div : ('s, 'l) binop
val rem : ('s, 'l) binop
val checked_abs : (signed, 'l) checked_unop
val checked_neg : (signed, 'l) checked_unop
val checked_add : ('s, 'l) checked_binop
val checked_sub : ('s, 'l) checked_binop
val checked_mul : ('s, 'l) checked_binop

(* bitwise logic *)
val logand : (unsigned, 'l) binop
val logor : (unsigned, 'l) binop
val logxor : (unsigned, 'l) binop
val lognot : (unsigned, 'l) unop
val logsl : (unsigned, 'l) shift
val logsr : (unsigned, 'l) shift

(* sign aware comparison *)
val compare : ('s, 'l) int_kind ->
  ('s, 'l) int_val -> ('s, 'l) int_val -> (signed, sixtyfour) int_val
val equal : ('s, 'l) int_kind ->
  ('s, 'l) int_val -> ('s, 'l) int_val -> bool

(* utilities *)
val string_of_int_kind : ('s, 'l) int_kind -> string
