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

type t = int32
type raw_level = t
include (Compare.Int32 : Compare.S with type t := t)
let encoding = Data_encoding.int32
let pp ppf level = Format.fprintf ppf "%ld" level
let rpc_arg =
  let construct raw_level = Int32.to_string raw_level in
  let destruct str =
    match Int32.of_string str with
    | exception _ -> Error "Cannot parse level"
    | raw_level -> Ok raw_level in
  RPC_arg.make
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

type error += Unexpected_level of Int32.t (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"unexpected_level"
    ~title:"Unexpected level"
    ~description:"Level must be non-negative."
    ~pp:(fun ppf l ->
        Format.fprintf ppf "The level is %s but should be non-negative." (Int32.to_string l))
    Data_encoding.(obj1 (req "level" int32))
    (function Unexpected_level l -> Some l | _ -> None)
    (fun l -> Unexpected_level l)

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
  let rpc_arg = rpc_arg
  let encoding = encoding
  let compare = compare
end
