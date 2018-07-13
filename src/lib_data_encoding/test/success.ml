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

(** Trivial back-and-forth test: a value is serialized, then
    unserialized and compared to the original value. All backend
    (json, bson, binary, and streamed binary) are tested for each of
    the basic encoding described here. No serialization or
    deserialization failure are expected in these tests. *)

(* TODO `varopt` ; `assoc` ; `Data_encoding.json` *)

open Data_encoding
open Helpers
open Types
open Utils.Infix

let json ty encoding value () =
  no_exception begin fun () ->
    let json = Json.construct encoding value in
    let result = Json.destruct encoding json in
    Alcotest.check ty "json" value result
  end

let bson ty encoding value () =
  no_exception begin fun () ->
    let json = Bson.construct encoding value in
    let result = Bson.destruct encoding json in
    Alcotest.check ty "bson" value result
  end

let binary ty encoding value () =
  no_exception begin fun () ->
    let bytes = Binary.to_bytes_exn encoding value in
    let result = Binary.of_bytes_exn encoding bytes in
    Alcotest.check ty "binary" value result
  end

let stream ty encoding value () =
  no_exception begin fun () ->
    let bytes = Binary.to_bytes_exn encoding value in
    let len_data = MBytes.length bytes in
    for sz = 1 to max 1 len_data do
      let name = Format.asprintf "stream (%d)" sz in
      match chunked_read sz encoding bytes with
      | Binary.Success { result ; size ; stream } ->
          if size <> MBytes.length bytes ||
             not (Binary_stream.is_empty stream) then
            Alcotest.failf "%s failed: remaining data" name ;
          Alcotest.check ty name value result
      | Binary.Await _ ->
          Alcotest.failf "%s failed: not enough data" name
      | Binary.Error error ->
          Alcotest.failf
            "@[<v 2>%s failed: read error@ %a@]"
            name
            Binary.pp_read_error error
    done ;
  end

let all name ty encoding value =
  let stream_encoding =
    match Data_encoding.classify encoding with
    | `Variable -> dynamic_size encoding
    | `Dynamic | `Fixed _ -> encoding in
  [ name ^ ".json", `Quick, json ty encoding value ;
    name ^ ".bson", `Quick, bson ty encoding value ;
    name ^ ".binary", `Quick, binary ty encoding value ;
    name ^ ".binary_stream", `Quick, stream ty stream_encoding value ]

let all_int encoding size =
  let name = Format.asprintf "int%d" size in
  all (name ^ ".min") Alcotest.int encoding ~- (1 lsl (size - 1)) @
  all (name ^ ".mean") Alcotest.int encoding 0 @
  all (name ^ ".max") Alcotest.int encoding ((1 lsl (size - 1)) - 1)

let all_uint encoding size =
  let name = Format.asprintf "uint%d" size in
  all (name ^ ".min") Alcotest.int encoding 0 @
  all (name ^ ".mean") Alcotest.int encoding (1 lsl (size - 1)) @
  all (name ^ ".max") Alcotest.int encoding ((1 lsl size) - 1)

let all_ranged_int minimum maximum =
  let encoding = ranged_int minimum maximum in
  let name = Format.asprintf "ranged_int.%d" minimum in
  all (name ^ ".min") Alcotest.int encoding minimum @
  all (name ^ ".mean") Alcotest.int encoding ((minimum + maximum) / 2) @
  all (name ^ ".max") Alcotest.int encoding maximum

let all_ranged_float minimum maximum =
  let encoding = ranged_float minimum maximum in
  let name = Format.asprintf "ranged_float.%f" minimum in
  all (name ^ ".min") Alcotest.float encoding minimum @
  all (name ^ ".mean") Alcotest.float encoding ((minimum +. maximum) /. 2.) @
  all (name ^ ".max") Alcotest.float encoding maximum

let test_n_sequence () =
  let test i =
    binary Alcotest.z z i () ;
    stream Alcotest.z z i () in
  for i = 0 to 10_000 do test (Z.of_int i) done ;
  for i = 100_000_000 to 100_010_000 do test (Z.of_int i) done

let test_z_sequence () =
  let test i =
    binary Alcotest.z z i () ;
    stream Alcotest.z z i () in
  for i = -10_000 to 10_000 do test (Z.of_int i) done ;
  for i = 100_000_000 to 100_010_000 do test (Z.of_int i) done ;
  for i = -100_000_000 downto -100_010_000 do test (Z.of_int i) done

let test_string_enum_boundary () =
  let entries = List.rev_map (fun x -> string_of_int x, x) (0 -- 254) in
  let run_test cases =
    List.iter (fun (_, num)  ->
        let enc = string_enum cases in
        json Alcotest.int enc num () ;
        bson Alcotest.int enc num () ;
        binary Alcotest.int enc num () ;
        stream Alcotest.int enc num ())
      cases in
  run_test entries ;
  let entries2 = (("255", 255) :: entries) in
  run_test entries2 ;
  run_test (("256", 256) :: entries2)

let test_bounded_string_list =
  let test name ~total ~elements v =
    "bounded_string_list." ^ name, `Quick,
    binary Alcotest.(list string)
      (bounded_list ~total ~elements string) v in
  [ test "a" ~total:0 ~elements:0 [] ;
    test "b" ~total:4 ~elements:4 [""] ;
    test "c" ~total:20 ~elements:4 ["";"";"";"";""] ;
    test "d" ~total:21 ~elements:5 ["";"";"";"";"a"] ;
    test "e" ~total:31 ~elements:10 ["ab";"c";"def";"gh";"ijk"] ;
  ]

let tests =
  all "null" Alcotest.pass null () @
  all "empty" Alcotest.pass empty () @
  all "constant" Alcotest.pass (constant "toto") () @
  all_int int8 8 @
  all_uint uint8 8 @
  all_int int16 16 @
  all_uint uint16 16 @
  all_int int31 31 @
  all "int32.min" Alcotest.int32 int32 Int32.min_int @
  all "int32.max" Alcotest.int32 int32 Int32.max_int @
  all "int64.min" Alcotest.int64 int64 Int64.min_int @
  all "int64.max" Alcotest.int64 int64 Int64.max_int @
  all_ranged_int 100 400 @
  all_ranged_int 19000 19254 @
  all_ranged_int ~-100 300 @
  all_ranged_int ~-300_000_000 300_000_000 @
  all "bool.true" Alcotest.bool bool true @
  all "bool.false" Alcotest.bool bool false @
  all "string" Alcotest.string string "tutu" @
  all "string.fixed" Alcotest.string (Fixed.string 4) "tutu" @
  all "string.variable" Alcotest.string Variable.string "tutu" @
  all "string.bounded1" Alcotest.string (Bounded.string 4) "tu" @
  all "string.bounded2" Alcotest.string (Bounded.string 4) "tutu" @
  all "bytes" Alcotest.bytes bytes (MBytes.of_string "titi") @
  all "bytes.fixed" Alcotest.bytes (Fixed.bytes 4)
    (MBytes.of_string "titi") @
  all "bytes.variable" Alcotest.bytes Variable.bytes
    (MBytes.of_string "titi") @
  all "bytes.bounded1" Alcotest.bytes (Bounded.bytes 4) (MBytes.of_string "tu") @
  all "bytes.bounded2" Alcotest.bytes (Bounded.bytes 4) (MBytes.of_string "tutu") @
  all "float" Alcotest.float float 42. @
  all "float.max" Alcotest.float float max_float @
  all "float.min" Alcotest.float float min_float @
  all "float.neg_zero" Alcotest.float float (-. 0.) @
  all "float.zero" Alcotest.float float (+. 0.) @
  all "float.infinity" Alcotest.float float infinity @
  all "float.neg_infity" Alcotest.float float neg_infinity @
  all "float.epsilon" Alcotest.float float epsilon_float @
  all "float.nan" Alcotest.float float nan @
  all_ranged_float ~-. 100. 300. @
  all "n.zero" Alcotest.n n (Z.zero) @
  all "n.one" Alcotest.n n (Z.one) @
  [ "n.sequence", `Quick, test_n_sequence ] @
  let rec fact i l =
    if i < 1 then
      []
    else
      let l = Z.mul l (Z.of_int i) in
      fact (i - 1) l @
      all (Format.asprintf "n.fact.%d" i) Alcotest.n n l in
  fact 35 Z.one @
  all "n.a" Alcotest.n n
    (Z.of_string "123574503164821730218493275982143254986574985328") @
  all "n.b" Alcotest.n n
    (Z.of_string "8493275982143254986574985328") @
  all "n.c" Alcotest.n n
    (Z.of_string "123574503164821730218474985328") @
  all "n.d" Alcotest.n n
    (Z.of_string "10000000000100000000001000003050000000060600000000000777000008") @
  all "z.zero" Alcotest.z z (Z.zero) @
  all "z.one" Alcotest.z z (Z.one) @
  [ "z.sequence", `Quick, test_z_sequence ] @
  let rec fact n l =
    if n < 1 then
      []
    else
      let l = Z.mul l (Z.of_int n) in
      fact (n - 1) l @
      all (Format.asprintf "z.fact.%d" n) Alcotest.z z l in
  fact 35 Z.one @
  all "z.a" Alcotest.z z
    (Z.of_string "123574503164821730218493275982143254986574985328") @
  all "z.b" Alcotest.z z
    (Z.of_string "8493275982143254986574985328") @
  all "z.c" Alcotest.z z
    (Z.of_string "123574503164821730218474985328") @
  all "z.d" Alcotest.z z
    (Z.of_string "10000000000100000000001000003050000000060600000000000777000008") @
  all "z.e" Alcotest.z z
    (Z.of_string "-123574503164821730218493275982143254986574985328") @
  all "z.f" Alcotest.z z
    (Z.of_string "-8493275982143254986574985328") @
  all "z.g" Alcotest.z z
    (Z.of_string "-123574503164821730218474985328") @
  all "z.h" Alcotest.z z
    (Z.of_string "-10000000000100000000001000003050000000060600000000000777000008") @
  all "none" Alcotest.(option string) (option string) None @
  all "some.string" Alcotest.(option string) (option string)
    (Some "thing") @
  all "enum" Alcotest.int enum_enc 4 @
  all "obj" Alcotest.record record_obj_enc default_record @
  all "obj.dft" Alcotest.record record_obj_enc
    { default_record with b = false } @
  all "obj.req" Alcotest.record record_obj_enc
    { default_record with c = None } @
  all "tup" Alcotest.record record_tup_enc default_record @
  all "obj.variable" Alcotest.variable_record variable_record_obj_enc
    default_variable_record @
  all "tup.variable" Alcotest.variable_record variable_record_tup_enc
    default_variable_record @
  all "obj.variable_left" Alcotest.variable_left_record variable_left_record_obj_enc
    default_variable_left_record @
  all "tup.variable_left" Alcotest.variable_left_record variable_left_record_tup_enc
    default_variable_left_record @
  all "union.A" Alcotest.union union_enc (A 1) @
  all "union.B" Alcotest.union union_enc (B "2") @
  all "union.C" Alcotest.union union_enc (C 3) @
  all "union.D" Alcotest.union union_enc (D "4") @
  all "union.E" Alcotest.union union_enc E @
  all "variable_list.empty" Alcotest.(list int) (Variable.list int31) [] @
  all "variable_list" Alcotest.(list int) (Variable.list int31) [1;2;3;4;5] @
  all "variable_array.empty" Alcotest.(array int) (Variable.array int31) [||] @
  all "variable_array" Alcotest.(array int) (Variable.array int31) [|1;2;3;4;5|] @
  all "list.empty" Alcotest.(list int) (list int31) [] @
  all "list" Alcotest.(list int) (list int31) [1;2;3;4;5] @
  all "array.empty" Alcotest.(array int) (array int31) [||] @
  all "array" Alcotest.(array int) (array int31) [|1;2;3;4;5|] @
  all "mu_list.empty" Alcotest.(list int) (mu_list_enc int31) [] @
  all "mu_list" Alcotest.(list int) (mu_list_enc int31) [1;2;3;4;5] @
  test_bounded_string_list @
  [ "string_enum_boundary", `Quick, test_string_enum_boundary ;
  ]
