(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

val bool: int
val int8: int
val uint8: int
val char: int
val int16: int
val uint16: int
val uint30: int
val uint32: int
val uint64: int
val int31: int
val int32: int
val int64: int
val float: int

type tag_size = [ `Uint8 | `Uint16 ]

val tag_size: tag_size -> int

type signed_integer = [ `Int31 | `Int16 | `Int8 ]
type unsigned_integer = [ `Uint30 | `Uint16 | `Uint8 ]
type integer = [ signed_integer | unsigned_integer ]
val integer_to_size: integer -> int
val range_to_size: minimum:int -> maximum:int -> integer
val enum_size: 'a array -> [> unsigned_integer ]

