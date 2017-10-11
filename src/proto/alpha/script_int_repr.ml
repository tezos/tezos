(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type n = Natural_tag
type z = Integer_tag
type 't num = Z.t

let compare x y = Z.compare x y

let zero = Z.zero
let zero_n = Z.zero

let to_string x = Z.to_string x
let of_string s = try Some (Z.of_string s) with _ -> None

let to_int64 x = try Some (Z.to_int64 x) with _ -> None
let of_int64 n = Z.of_int64 n

let to_int x = try Some (Z.to_int x) with _ -> None
let of_int n = Z.of_int n

let of_zint x = x
let to_zint x = x

let add x y = Z.add x y
let sub x y = Z.sub x y
let mul x y = Z.mul x y

let ediv x y =
  try
    let (q, r) = Z.ediv_rem x y in
    Some (q, r)
  with _ -> None

let add_n = add
let mul_n = mul
let ediv_n = ediv

let abs x = Z.abs x
let neg x = Z.neg x
let int x = x

let shift_left x y =
  if Compare.Int.(Z.compare y (Z.of_int 256) > 0) then
    None
  else
    let y = Z.to_int y in
    Some (Z.shift_left x y)

let shift_right x y =
  if Compare.Int.(Z.compare y (Z.of_int 256) > 0) then
    None
  else
    let y = Z.to_int y in
    Some (Z.shift_right x y)

let shift_left_n = shift_left
let shift_right_n = shift_right

let logor x y = Z.logor x y
let logxor x y = Z.logxor x y
let logand x y = Z.logand x y
let lognot x = Z.lognot x
