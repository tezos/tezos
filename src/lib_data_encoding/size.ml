(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let bool = 1
let int8 = 1
let uint8 = 1
let char = 1
let int16 = 2
let uint16 = 2
let uint30 = 4
let uint32 = 4
let uint64 = 8
let int31 = 4
let int32 = 4
let int64 = 8
let float = 8

type tag_size = [ `Uint8 | `Uint16 ]

let tag_size = function
  | `Uint8 -> uint8
  | `Uint16 -> uint16


type signed_integer = [ `Int31 | `Int16 | `Int8 ]
type unsigned_integer = [ `Uint30 | `Uint16 | `Uint8 ]
type integer = [ signed_integer | unsigned_integer ]

let signed_range_to_size min max : [> signed_integer ] =
  if min >= ~-128 && max <= 127
  then `Int8
  else if min >= ~-32_768 && max <= 32_767
  then `Int16
  else `Int31

(* max should be centered at zero *)
let unsigned_range_to_size max : [> unsigned_integer ] =
  if max <= 255
  then `Uint8
  else if max <= 65535
  then `Uint16
  else `Uint30

let integer_to_size = function
  | `Int31 -> int31
  | `Int16 -> int16
  | `Int8 -> int8
  | `Uint30 -> uint30
  | `Uint16 -> uint16
  | `Uint8 -> uint8

let range_to_size ~minimum ~maximum : integer =
  if minimum < 0
  then signed_range_to_size minimum maximum
  else unsigned_range_to_size (maximum - minimum)

let enum_size arr =
  unsigned_range_to_size (Array.length arr)

