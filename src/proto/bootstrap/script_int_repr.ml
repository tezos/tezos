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
type ('s, 'l) int_val = Int of repr and repr = int64

(* types *)
and ('s, 'l) int_kind  =
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
let cast
  : type tos tol. (tos, tol) int_kind -> (_, _) int_val -> (tos, tol) int_val
  = fun to_kind (Int v) ->
  let (land) = Int64.logand
  and (lor) = Int64.logor
  and (=) = Compare.Int64.(=) in
  match to_kind with
  | Int8 when v land 0x80L = 0x80L ->
      Int ((v land 0x000000FFL) lor 0xFFFFFFFFFFFFFF00L)
  | Int8 ->
      Int (v land 0x000000FFL)
  | Uint8 ->
      Int (v land 0x000000FFL)
  | Int16 when v land 0x8000L = 0x8000L ->
      Int ((v land 0x0000FFFFL) lor 0xFFFFFFFFFFFF0000L)
  | Int16 ->
      Int (v land 0x0000FFFFL)
  | Uint16 ->
      Int (v land 0x0000FFFFL)
  | Int32 when v land 0x80000000L = 0x80000000L ->
      Int ((v land 0xFFFFFFFFL) lor 0xFFFFFFFF00000000L)
  | Int32 ->
      Int (v land 0xFFFFFFFFL)
  | Uint32 ->
      Int (v land 0xFFFFFFFFL)
  | Int64 -> Int v
  | Uint64 -> Int v

let checked_cast
  : type tos tol. (tos, tol) int_kind -> (_, _) int_val -> (tos, tol) int_val option
  = fun to_kind (Int v as arg) ->
    let Int casted as res = cast to_kind arg in
    if Compare.Int64.(casted <> v) then None else Some res

(* to native int64s *)
let to_int64 _ (Int v) =
  v
let of_int64 k v =
  cast k (Int v)
let checked_of_int64 k v =
  checked_cast k (Int v)

(* arithmetics *)
let add kind (Int va) (Int vb) =
  of_int64 kind (Int64.add va vb)
let sub kind (Int va) (Int vb) =
  of_int64 kind (Int64.sub va vb)
let mul : type s l. (s, l) int_kind -> (s, l) int_val -> (s, l) int_val -> (s, l) int_val
  = fun kind (Int va) (Int vb) -> let r = Int64.mul va vb in match kind with
    | Int8 -> of_int64 Int8 r
    | Uint8 -> of_int64 Uint8 r
    | Int16 -> of_int64 Int16 r
    | Uint16 -> of_int64 Uint16 r
    | Int32 -> of_int64 Int32 r
    | Uint32 -> of_int64 Uint32 r
    | Int64 -> of_int64 Int64 r
    | Uint64 -> invalid_arg "Script_int.mul"
let div : type s l. (s, l) int_kind -> (s, l) int_val -> (s, l) int_val -> (s, l) int_val
  = fun kind (Int va) (Int vb) -> let r = Int64.div va vb in match kind with
    | Int8 -> of_int64 Int8 r
    | Uint8 -> of_int64 Uint8 r
    | Int16 -> of_int64 Int16 r
    | Uint16 -> of_int64 Uint16 r
    | Int32 -> of_int64 Int32 r
    | Uint32 -> of_int64 Uint32 r
    | Int64 -> of_int64 Int64 r
    | Uint64 -> invalid_arg "Script_int.div"
let rem : type s l. (s, l) int_kind -> (s, l) int_val -> (s, l) int_val -> (s, l) int_val
  = fun kind (Int va) (Int vb) -> let r = Int64.rem va vb in match kind with
    | Int8 -> of_int64 Int8 r
    | Uint8 -> of_int64 Uint8 r
    | Int16 -> of_int64 Int16 r
    | Uint16 -> of_int64 Uint16 r
    | Int32 -> of_int64 Int32 r
    | Uint32 -> of_int64 Uint32 r
    | Int64 -> of_int64 Int64 r
    | Uint64 -> invalid_arg "Script_int.rem"
let neg kind (Int v) =
  of_int64 kind (Int64.neg v)
let abs kind (Int v) =
  of_int64 kind (Int64.abs v)

(* bitwise logic *)
let logand _ (Int va) (Int vb) =
  Int (Int64.logand va vb)
let logor _ (Int va) (Int vb) =
  Int (Int64.logor va vb)
let logxor kind (Int va) (Int vb) =
  cast kind (Int (Int64.logxor va vb))
let lognot kind (Int v) =
  cast kind (Int (Int64.lognot v))
let logsl kind (Int va) (Int vb) =
  cast kind (Int (Int64.shift_left va (Int64.to_int vb)))
let logsr _ (Int va) (Int vb) =
  Int (Int64.shift_right_logical va (Int64.to_int vb))

(* sign aware comparison *)
let compare
  : type s l. (s, l) int_kind -> (s, l) int_val -> (s, l) int_val -> (signed, sixtyfour) int_val
  = fun kind (Int va) (Int vb) ->
    let cmp = match kind with
      | Int8 -> Compare.Int64.compare va vb
      | Uint8 -> Compare.Uint64.compare va vb
      | Int16 -> Compare.Int64.compare va vb
      | Uint16 -> Compare.Uint64.compare va vb
      | Int32 -> Compare.Int64.compare va vb
      | Uint32 -> Compare.Uint64.compare va vb
      | Int64 -> Compare.Int64.compare va vb
      | Uint64 -> Compare.Uint64.compare va vb in
    Int Compare.Int.(if cmp = 0 then 0L else if cmp > 0 then 1L else -1L)

let equal kind va vb =
  Compare.Int64.(to_int64 kind va = to_int64 kind vb)

(* checked arithmetics *)
let checked_add : type s l. (s, l) int_kind -> (s, l) int_val -> (s, l) int_val -> (s, l) int_val option
  = fun kind (Int va) (Int vb) -> let r = Int64.add va vb in match kind with
    | Int8 -> checked_of_int64 Int8 r
    | Uint8 -> checked_of_int64 Uint8 r
    | Int16 -> checked_of_int64 Int16 r
    | Uint16 -> checked_of_int64 Uint16 r
    | Int32 -> checked_of_int64 Int32 r
    | Uint32 -> checked_of_int64 Uint32 r
    | Int64 when Compare.Int.(Compare.Int64.compare r va < 0) -> None
    | Int64 -> Some (Int r)
    | Uint64 when Compare.Int.(Compare.Uint64.compare r va < 0) -> None
    | Uint64 -> Some (Int r)

let checked_sub : type s l. (s, l) int_kind -> (s, l) int_val -> (s, l) int_val -> (s, l) int_val option
  = fun kind (Int va) (Int vb) -> let r = Int64.sub va vb in match kind with
    | Int8 -> checked_of_int64 Int8 r
    | Uint8 -> checked_of_int64 Uint8 r
    | Int16 -> checked_of_int64 Int16 r
    | Uint16 -> checked_of_int64 Uint16 r
    | Int32 -> checked_of_int64 Int32 r
    | Uint32 -> checked_of_int64 Uint32 r
    | Int64 when Compare.Int64.(vb >= 0L) ->
        if Compare.Int.(Compare.Int64.compare r va <= 0) then Some (Int r) else None
    | Int64 ->
        if Compare.Int.(Compare.Int64.compare r va >= 0) then Some (Int r) else None
    | Uint64 when Compare.Int.(Compare.Uint64.compare r va > 0) -> None
    | Uint64 -> Some (Int r)

let checked_neg : type l. (signed, l) int_kind -> (signed, l) int_val -> (signed, l) int_val option
  = fun kind (Int v) -> let r = Int64.neg v in match kind with
    | Int8 -> checked_of_int64 Int8 r
    | Int16 -> checked_of_int64 Int16 r
    | Int32 -> checked_of_int64 Int32 r
    | Int64 when Compare.Int64.(v = Int64.min_int) -> None
    | Int64 -> Some (Int r)

let checked_abs : type l. (signed, l) int_kind -> (signed, l) int_val -> (signed, l) int_val option
  = fun kind (Int v) -> let r = Int64.abs v in match kind with
  | Int8 -> checked_of_int64 Int8 r
  | Int16 -> checked_of_int64 Int16 r
  | Int32 -> checked_of_int64 Int32 r
  | Int64 when Compare.Int64.(v = Int64.min_int) -> None
  | Int64 -> Some (Int r)

let checked_mul : type s l. (s, l) int_kind -> (s, l) int_val -> (s, l) int_val -> (s, l) int_val option
  = fun kind (Int va) (Int vb) -> let r = Int64.mul va vb in match kind with
    | Int8 -> checked_of_int64 Int8 r
    | Uint8 -> checked_of_int64 Uint8 r
    | Int16 -> checked_of_int64 Int16 r
    | Uint16 -> checked_of_int64 Uint16 r
    | Int32 -> checked_of_int64 Int32 r
    | Uint32 -> checked_of_int64 Uint32 r
    | Int64 ->
        if Compare.Int64.(vb = 0L || va = 0L) then Some (Int r)
        else if Compare.Int64.(r = 0L) then None
        else if Compare.Int64.(Int64.div r va = vb) then Some (Int r)
        else None
    | Uint64 -> invalid_arg "Script_int.checked_mul"

let string_of_int_kind (type s) (type l) (kind:(s,l) int_kind) =
  match kind with
  | Int8 -> "int8"
  | Uint8 -> "uint8"
  | Int16 -> "int16"
  | Uint16 -> "uint16"
  | Int32 -> "int32"
  | Uint32 -> "uint32"
  | Int64 -> "int64"
  | Uint64 -> "uint64"
