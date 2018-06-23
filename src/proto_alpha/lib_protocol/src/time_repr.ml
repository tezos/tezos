(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Time
type time = t

type error += Timestamp_add (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"timestamp_add"
    ~title:"Timestamp add"
    ~description:"Overflow when adding timestamps."
    ~pp:(fun ppf () ->
        Format.fprintf ppf "Overflow when adding timestamps.")
    Data_encoding.empty
    (function Timestamp_add -> Some () | _ -> None)
    (fun () -> Timestamp_add)

let of_seconds s =
  try Some (of_seconds (Int64.of_string s))
  with _ -> None
let to_seconds = to_seconds
let to_seconds_string s = Int64.to_string (to_seconds s)

let pp = pp_hum

let (+?) x y =
  (* TODO check overflow *)
  try ok (add x (Period_repr.to_seconds y))
  with _exn -> Error [ Timestamp_add ]
