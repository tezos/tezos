(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Time
type time = t

type error += Timestamp_add of exn

let of_seconds s =
  try Some (of_seconds (Int64.of_string s))
  with _ -> None
let to_seconds = to_seconds
let to_seconds_string s = Int64.to_string (to_seconds s)

let pp = pp_hum

let (+?) x y =
  (* TODO check overflow *)
  try ok (add x (Period_repr.to_seconds y))
  with exn -> Error [Timestamp_add exn]

