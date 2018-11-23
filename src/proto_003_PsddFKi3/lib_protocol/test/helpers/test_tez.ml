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

open Proto_alpha
open Alpha_context
open Alpha_environment

(* This module is mostly to wrap the errors from the protocol *)
module Tez = struct
  include Tez

  let ( +? ) t1 t2 = (t1 +? t2) |> wrap_error
  let ( -? ) t1 t2 = (t1 -? t2) |> wrap_error
  let ( *? ) t1 t2 = (t1 *? t2) |> wrap_error
  let ( /? ) t1 t2 = (t1 /? t2) |> wrap_error

  let ( + ) t1 t2 =
    match t1 +? t2 with
    | Ok r -> r
    | Error _ ->
        Pervasives.failwith "adding tez"

  let of_int x =
    match Tez.of_mutez (Int64.mul (Int64.of_int x) 1_000_000L) with
    | None -> invalid_arg "tez_of_int"
    | Some x -> x

  let of_mutez_exn x =
    match Tez.of_mutez x with
    | None -> invalid_arg "tez_of_mutez"
    | Some x -> x


  let max_tez =
    match Tez.of_mutez Int64.max_int with
    | None -> assert false
    | Some p -> p

end
