(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = int32
type roll = t

let encoding = Data_encoding.int32

let first = 0l
let succ i = Int32.succ i

let random sequence ~bound =
  Seed_repr.take_int32 sequence bound

let to_int32 v = v

let (=) = Compare.Int32.(=)
