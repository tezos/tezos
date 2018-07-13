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

(** Test expected errors while serializing data. *)

open Data_encoding
open Types

let check_raises expected f =
  match f () with
  | exception exn when expected exn -> ()
  | exception exn ->
      Alcotest.failf "Unexpected exception: %s." (Printexc.to_string exn)
  | _ -> Alcotest.failf "Expecting exception, got success."

let json ?(expected = fun _ -> true) encoding value () =
  check_raises expected begin fun () ->
    ignore (Json.construct encoding value : Json.t) ;
  end

let bson ?(expected = fun _ -> true) encoding value () =
  check_raises expected begin fun () ->
    ignore (Bson.construct encoding value : Bson.t) ;
  end

let binary ?(expected = fun _ -> true) encoding value () =
  check_raises expected begin fun () ->
    ignore (Binary.to_bytes_exn encoding value : MBytes.t) ;
  end

let all name encoding value =
  [ name ^ ".json", `Quick, json encoding value ;
    name ^ ".bson", `Quick, bson encoding value ;
    name ^ ".bytes", `Quick, binary encoding value ]

let all_ranged_int minimum maximum =
  let encoding = ranged_int minimum maximum in
  let name = Format.asprintf "ranged_int.%d" minimum in
  all (name ^ ".min") encoding (minimum - 1) @
  all (name ^ ".max") encoding (maximum + 1)

let all_ranged_float minimum maximum =
  let encoding = ranged_float minimum maximum in
  let name = Format.asprintf "ranged_float.%f" minimum in
  all (name ^ ".min") encoding (minimum -. 1.) @
  all (name ^ ".max") encoding (maximum +. 1.)

let test_bounded_string_list =
  let expected = function
    | Binary_error.Write_error Size_limit_exceeded -> true
    | _ -> false in
  let test name ~total ~elements v =
    "bounded_string_list." ^ name, `Quick,
    binary ~expected (bounded_list ~total ~elements string) v in
  [ test "a" ~total:0 ~elements:0 [""] ;
    test "b1" ~total:3 ~elements:4 [""] ;
    test "b2" ~total:4 ~elements:3 [""] ;
    test "c1" ~total:19 ~elements:4 ["";"";"";"";""] ;
    test "c2" ~total:20 ~elements:3 ["";"";"";"";""] ;
    test "d1" ~total:20 ~elements:5 ["";"";"";"";"a"] ;
    test "d2" ~total:21 ~elements:4 ["";"";"";"";"a"] ;
    test "e" ~total:30 ~elements:10 ["ab";"c";"def";"gh";"ijk"] ;
  ]

let tests =
  all_ranged_int 100 400 @
  all_ranged_int 19000 19254 @
  all_ranged_int ~-100 300 @
  all_ranged_int ~-300_000_000 300_000_000 @
  all_ranged_float ~-. 100. 300. @
  all "string.fixed" (Fixed.string 4) "turlututu" @
  all "string.bounded" (Bounded.string 4) "turlututu" @
  all "bytes.fixed" (Fixed.bytes 4) (MBytes.of_string "turlututu") @
  all "bytes.bounded" (Bounded.bytes 4) (MBytes.of_string "turlututu") @
  all "unknown_case.B" mini_union_enc (B "2") @
  all "unknown_case.E" mini_union_enc E @
  test_bounded_string_list @
  all "n" n (Z.of_string "-12") @
  []
