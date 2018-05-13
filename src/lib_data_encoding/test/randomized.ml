(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(** Various randomly generated data. *)

open Data_encoding

(** Generate encodings of the encoding and the randomized generator *)
let test_generator ?(iterations=50) ty encoding generator =
  for _ = 0 to iterations - 1 do
    let value = generator () in
    Success.json ty encoding value () ;
    Success.bson ty encoding value () ;
    Success.binary ty encoding value () ;
    Success.stream ty encoding value () ;
  done

let rec make_int_list acc len () =
  if len = 0 then
    acc
  else
    make_int_list (Random.int64 Int64.max_int :: acc) (len - 1) ()

let test_randomized_int_list () =
  test_generator
    Alcotest.(list int64)
    (list int64)
    (make_int_list [] 100)

let test_randomized_string_list () =
  test_generator
    Alcotest.(list string)
    (list string)
    (fun () -> List.map Int64.to_string (make_int_list [] 20 ()))

let test_randomized_variant_list () =
  test_generator
    Alcotest.(list (result (option string) string))
    (list (result (option string) (obj1 (req "failure" string))))
    (fun () ->
       List.map
         (fun x ->
            let str = Int64.to_string x in
            if Random.bool ()
            then if Random.bool () then Ok (Some str) else Ok None
            else Error str)
         (make_int_list [] 20 ()))

let tests = [
  "int_list", `Quick, test_randomized_int_list ;
  "string_list", `Quick, test_randomized_string_list ;
  "variant_list", `Quick, test_randomized_variant_list ;
]
