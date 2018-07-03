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


let test_rt_opt name testable enc dec input =
  try
    let roundtripped = dec (enc input) in
    Alcotest.check (Alcotest.option testable) name (Some input) roundtripped
  with
    exc ->
      Alcotest.failf "%s failed for %a: exception whilst decoding: %s"
        name (Alcotest.pp testable) input (Printexc.to_string exc)

let test_decode_opt_safe name testable dec encoded =
  match dec encoded with
  | Some _ | None -> ()
  | exception exc ->
      Alcotest.failf "%s failed for %a: exception whilst decoding: %s"
        name (Alcotest.pp testable) encoded (Printexc.to_string exc)

let test_decode_opt_fail name testable dec encoded =
  try
    let decoded = dec encoded in
    Alcotest.check (Alcotest.option testable) name None decoded
  with
    exc ->
      Alcotest.failf "%s failed: exception whilst decoding: %s"
        name (Printexc.to_string exc)
