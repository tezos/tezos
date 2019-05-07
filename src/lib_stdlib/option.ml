(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let map ~f = function
  | None -> None
  | Some x -> Some (f x)

let apply ~f = function
  | None -> None
  | Some x -> f x

let (>>=) x f = apply ~f x
let (>>|) x f = map ~f x

let iter ~f = function
  | None -> ()
  | Some x -> f x

let unopt ~default = function
  | None -> default
  | Some x -> x

let unopt_map ~f ~default = function
  | None -> default
  | Some x -> f x

let unopt_exn err = function
  | Some x -> x
  | _ -> raise err

let unopt_assert ~loc:(name, line, pos, _) = function
  | Some v -> v
  | None -> raise (Assert_failure (name, line, pos))

let first_some a b = match a, b with
  | None, None -> None
  | None, Some v -> Some v
  | Some v, _ -> Some v

let try_with f =
  try Some (f ()) with _ -> None

let some x = Some x

let pp ?(default="") data_pp ppf opt =
  unopt_map
    ~f:(fun i -> data_pp ppf i)
    ~default:(Format.pp_print_string ppf default)
    opt
