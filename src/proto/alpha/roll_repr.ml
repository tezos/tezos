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

module Index = struct
  type t = roll
  let path_length = 3
  let nbyte_of_int32 i n =
    Int32.to_string @@
    Int32.logand (Int32.shift_right i (8 * n)) (Int32.of_int 0xff)
  let to_path roll l =
    nbyte_of_int32 roll 0 :: nbyte_of_int32 roll 1 :: Int32.to_string roll :: l
  let of_path = function
    | _ :: _ :: s :: _ -> begin
        try Some (Int32.of_string s)
        with _ -> None
      end
    | _ -> None
end
