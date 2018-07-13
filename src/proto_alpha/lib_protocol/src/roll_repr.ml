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

include Compare.Int32
type roll = t

let encoding = Data_encoding.int32

let first = 0l
let succ i = Int32.succ i

let random sequence ~bound =
  Seed_repr.take_int32 sequence bound

let rpc_arg =
  RPC_arg.like
    RPC_arg.int32
    "roll"

let to_int32 v = v


module Index = struct
  type t = roll
  let path_length = 3
  let to_path roll l =
    (Int32.to_string @@ Int32.logand roll (Int32.of_int 0xff)) ::
    (Int32.to_string @@ Int32.logand (Int32.shift_right_logical roll 8) (Int32.of_int 0xff)) ::
    Int32.to_string roll :: l
  let of_path = function
    | _ :: _ :: s :: _ -> begin
        try Some (Int32.of_string s)
        with _ -> None
      end
    | _ -> None
  let rpc_arg = rpc_arg
  let encoding = encoding
  let compare = compare
end
