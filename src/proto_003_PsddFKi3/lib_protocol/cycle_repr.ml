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
type cycle = t

let encoding = Data_encoding.int32
let rpc_arg =
  let construct = Int32.to_string in
  let destruct str =
    match Int32.of_string str with
    | exception _ -> Error "Cannot parse cycle"
    | cycle -> Ok cycle in
  RPC_arg.make
    ~descr:"A cycle integer"
    ~name: "block_cycle"
    ~construct
    ~destruct
    ()

let pp ppf cycle = Format.fprintf ppf "%ld" cycle

include (Compare.Int32 : Compare.S with type t := t)

module Map = Map.Make(Compare.Int32)

let root = 0l
let succ = Int32.succ
let pred = function
  | 0l -> None
  | i -> Some (Int32.pred i)

let add c i =
  assert Compare.Int.(i > 0) ;
  Int32.add c (Int32.of_int i)

let sub c i =
  assert Compare.Int.(i > 0) ;
  let r = Int32.sub c (Int32.of_int i) in
  if Compare.Int32.(r < 0l) then None else Some r

let to_int32 i = i

let of_int32_exn l =
  if Compare.Int32.(l >= 0l)
  then l
  else invalid_arg "Level_repr.Cycle.of_int32"

module Index = struct
  type t = cycle
  let path_length = 1
  let to_path c l =
    Int32.to_string (to_int32 c) :: l
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
