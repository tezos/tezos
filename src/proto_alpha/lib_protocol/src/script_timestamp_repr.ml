(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = Z.t

let compare = Z.compare

let of_int64 = Z.of_int64

let of_string x =
  match Time_repr.of_notation x with
  | None ->
      begin try Some (Z.of_string x)
        with _ -> None
      end
  | Some time ->
      Some (of_int64 (Time_repr.to_seconds time))

let to_notation x =
  try
    let notation = Time_repr.to_notation (Time.of_seconds (Z.to_int64 x)) in
    if String.equal notation "out_of_range"
    then None
    else Some notation
  with _ -> None

let to_num_str = Z.to_string

let to_string x =
  match to_notation x with
  | None -> to_num_str x
  | Some s -> s

let diff x y = Script_int_repr.of_zint @@ Z.sub x y

let sub_delta t delta = Z.sub t (Script_int_repr.to_zint delta)

let add_delta t delta =
  Z.add t (Script_int_repr.to_zint delta)

let to_zint x = x
