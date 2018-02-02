(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let bench ?(num_iterations=1000) name thunk =
  Gc.full_major () ;
  Gc.compact () ;
  let start_time = Sys.time () in
  for i = 0 to (num_iterations - 1) do
    thunk ()
  done ;
  let end_time = Sys.time () in
  Format.printf
    "Benchmark: %s took %f for %d iterations.@."
    name
    (end_time -. start_time)
    num_iterations

let bench_json_binary ?(num_iterations=1000) name encoding value =
  bench ~num_iterations ("writing " ^ name ^ " json")
    (fun () -> ignore @@ Data_encoding_ezjsonm.to_string @@ Data_encoding.Json.construct encoding value) ;
  let encoded_json = Data_encoding_ezjsonm.to_string @@ Data_encoding.Json.construct encoding value in
  bench ~num_iterations ("reading " ^ name ^ " json")
    (fun () -> Data_encoding.Json.destruct encoding (Ezjsonm.from_string encoded_json)) ;
  bench ~num_iterations ("writing " ^ name ^ " binary")
    (fun () -> ignore @@ Data_encoding.Binary.to_bytes encoding value) ;
  let encoded_binary =  Data_encoding.Binary.to_bytes encoding value in
  bench ~num_iterations ("reading " ^ name ^ " binary")
    (fun () -> ignore @@ Data_encoding.Binary.of_bytes_exn encoding encoded_binary)

type t =
  | A of string
  | B of bool
  | I of int
  | F of float
  | R of t * t

let cases_encoding : t Data_encoding.t =
  let open Data_encoding in
  mu "recursive"
    (fun recursive -> union [
         case (Tag 0)
           string
           (function A s -> Some s
                   | _ -> None)
           (fun s -> A s) ;
         case (Tag 1)
           bool
           (function B bool -> Some bool
                   | _ -> None)
           (fun bool -> B bool) ;
         case (Tag 2)
           int31
           (function I int -> Some int
                   | _ -> None)
           (fun int -> I int) ;
         case (Tag 3)
           float
           (function F float -> Some float
                   | _ -> None)
           (fun float -> F float) ;
         case (Tag 4)
           (obj2 (req "field1" recursive)
              (req "field2" recursive))
           (function R (a, b) -> Some (a, b)
                   | _ -> None)
           (fun (a, b) -> R (a, b))
       ])

let () =
  (* bench_json_binary "1000_element_int_list" Data_encoding.(list int31) (Array.to_list (Array.make 1000 0)) *)
  (* bench_json_binary "option_element_int_list" Data_encoding.(list (option int31)) (Array.to_list (Array.make 1000 (Some 0))) *)
  (* bench_json_binary "option_option_element_list"
   *   Data_encoding.(list (option (option int31)))
   *   (Array.to_list (Array.make 1000 (Some None))) *)
  bench_json_binary "option_option_result_element_list"
    Data_encoding.(list (result (option (option int31)) string))
    (Array.to_list (Array.make 1000 (Error "hello")))
(* bench ~num_iterations:10000 "binary_encoding"
 *   (let encoding = Data_encoding.(list cases_encoding) in
 *    let value = Array.to_list (Array.make 1000 (R (R (A "asdf", B true), F 1.0))) in
 *    (fun () -> ignore @@ Data_encoding.Binary.to_bytes encoding value)) *)
(* bench_json_binary "binary_encoding_large_list"
 *   Data_encoding.(list cases_encoding)
 *   (Array.to_list (Array.make 10000 (R (R (A "asdf", B true), F 1.0)))) *)
