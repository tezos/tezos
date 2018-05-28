(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

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
      union [ case (Tag 0) empty (fun () -> None) (fun () -> ()) ;
              case (Tag 0) empty (fun () -> None) (fun () -> ()) ]) ;
  test "fixed_negative_size" (fun () -> Fixed.string (~- 1)) ;
  test "fixed_null_size" (fun () -> Fixed.bytes 0) ;
  test "array_null_size" (fun () -> Variable.list empty) ;
  test "list_null_size" (fun () -> Variable.list null) ;
  test "zeroable_in_list" (fun () -> list (obj1 (varopt "x" int8))) ;
]
