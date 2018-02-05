(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* a protocol change proposal *)
type proposal = Protocol_hash.t

(* votes can be for, against or neutral.
   Neutral serves to count towards a quorum *)
type ballot = Yay | Nay | Pass

let ballot_encoding =
  let of_int8 = function
    | 0 -> Yay
    | 1 -> Nay
    | 2 -> Pass
    | _ -> invalid_arg "ballot_of_int8"
  in
  let to_int8 = function
    | Yay -> 0
    | Nay -> 1
    | Pass -> 2
  in
  let open Data_encoding in
  (* union *)
  splitted
    ~binary: (conv to_int8 of_int8 int8)
    ~json: (string_enum [
        "yay", Yay ;
        "nay", Nay ;
        "pass", Pass ;
      ])
