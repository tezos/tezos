(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type t = int32
type raw_level = t
include (Compare.Int32 : Compare.S with type t := t)
let encoding = Data_encoding.int32
let pp ppf level = Format.fprintf ppf "%ld" level
let arg =
  let construct raw_level = Int32.to_string raw_level in
  let destruct str =
    match Int32.of_string str with
    | exception _ -> Error "Cannot parse level"
    | raw_level -> Ok raw_level in
  RPC.Arg.make
    ~descr:"A level integer"
    ~name: "block_level"
    ~construct
    ~destruct
    ()

let root = 0l
let succ = Int32.succ
let pred l =
  if l = 0l
  then None
  else Some (Int32.pred l)

let diff = Int32.sub

let to_int32 l = l
let of_int32_exn l =
  if Compare.Int32.(l >= 0l)
  then l
  else invalid_arg "Level_repr.of_int32"

type error += Unexpected_level of Int32.t

let of_int32 l =
  try Ok (of_int32_exn l)
  with _ -> Error [Unexpected_level l]

module Index = struct
  type t = raw_level
  let path_length = 1
  let to_path level l = Int32.to_string level :: l
  let of_path = function
    | [s] -> begin
        try Some (Int32.of_string s)
        with _ -> None
      end
    | _ -> None
end
