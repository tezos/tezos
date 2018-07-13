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

open Data_encoding
open Helpers

let test ?(expected = fun _ -> true) name f =
  name, `Quick, fun () -> check_raises expected f

let tests = [
  test "multi_variable_tup" (fun () -> tup2 Variable.string Variable.string) ;
  test "variable_in_list" (fun () -> list Variable.string) ;
  test "nested_option" (fun () -> option (option int8)) ;
  test "merge_non_objs" (fun () -> merge_objs int8 string) ;
  test "empty_union" (fun () -> union []) ;
  test "duplicated_tag" (fun () ->
      union [ case (Tag 0) ~title:"" empty (fun () -> None) (fun () -> ()) ;
              case (Tag 0) ~title:"" empty (fun () -> None) (fun () -> ()) ]) ;
  test "fixed_negative_size" (fun () -> Fixed.string (~- 1)) ;
  test "fixed_null_size" (fun () -> Fixed.bytes 0) ;
  test "array_null_size" (fun () -> Variable.list empty) ;
  test "list_null_size" (fun () -> Variable.list null) ;
  test "zeroable_in_list" (fun () -> list (obj1 (varopt "x" int8))) ;
]
