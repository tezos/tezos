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
  assert (max >= 0) ;
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

let max_int = function
  | `Uint30 | `Int31 -> (1 lsl 30) - 1
  | `Int16 -> 1 lsl 15 - 1
  | `Int8 -> 1 lsl 7 - 1
  | `Uint16 -> 1 lsl 16 - 1
  | `Uint8 -> 1 lsl 8 - 1

let min_int = function
  | `Uint8 | `Uint16 | `Uint30 -> 0
  | `Int31 -> - (1 lsl 30)
  | `Int16 -> - (1 lsl 15)
  | `Int8 -> - (1 lsl 7)

let range_to_size ~minimum ~maximum : integer =
  if minimum < 0
  then signed_range_to_size minimum maximum
  else unsigned_range_to_size (maximum - minimum)

let enum_size arr =
  unsigned_range_to_size (Array.length arr)
