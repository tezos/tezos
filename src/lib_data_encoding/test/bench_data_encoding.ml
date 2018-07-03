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

let bench ?(num_iterations=1000) name thunk =
  Gc.full_major () ;
  Gc.compact () ;
  let start_time = Sys.time () in
  for _i = 0 to (num_iterations - 1) do
    thunk ()
  done ;
  let end_time = Sys.time () in
  Format.printf
    "Benchmark: %s took %f for %d iterations.@."
    name
    (end_time -. start_time)
    num_iterations

let read_stream encoding bytes =
  let rec loop bytes status =
    match bytes, status with
    | [], Data_encoding.Binary.Success _ -> ()
    | bytes :: bytess, Await f -> loop bytess (f bytes)
    | _, _ -> assert false in
  loop bytes (Data_encoding.Binary.read_stream encoding)

let bench_all ?(num_iterations=1000) name encoding value =
  bench ~num_iterations ("writing " ^ name ^ " json")
    (fun () -> ignore @@ Data_encoding.Json.to_string @@ Data_encoding.Json.construct encoding value) ;
  bench ~num_iterations ("writing " ^ name ^ " binary")
    (fun () -> ignore @@ Data_encoding.Binary.to_bytes_exn encoding value) ;
  let encoded_json = Data_encoding.Json.to_string @@ Data_encoding.Json.construct encoding value in
  bench ~num_iterations ("reading " ^ name ^ " json")
    (fun () -> ignore (Data_encoding.Json.destruct encoding (Ezjsonm.from_string encoded_json))) ;
  let encoded_binary =  Data_encoding.Binary.to_bytes_exn encoding value in
  bench ~num_iterations ("reading " ^ name ^ " binary")
    (fun () -> ignore @@ Data_encoding.Binary.of_bytes encoding encoded_binary ) ;
  bench ~num_iterations ("reading " ^ name ^ " streamed binary (one chunk)")
    (fun () -> read_stream encoding [encoded_binary]) ;
  bench ~num_iterations
    ("reading " ^ name ^ " streamed binary (small chunks)")
    (fun () -> read_stream encoding (MBytes.cut 1 encoded_binary)) ;
  ()

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
           ~title:"A"
           string
           (function A s -> Some s
                   | _ -> None)
           (fun s -> A s) ;
         case (Tag 1)
           ~title:"B"
           bool
           (function B bool -> Some bool
                   | _ -> None)
           (fun bool -> B bool) ;
         case (Tag 2)
           ~title:"I"
           int31
           (function I int -> Some int
                   | _ -> None)
           (fun int -> I int) ;
         case (Tag 3)
           ~title:"F"
           float
           (function F float -> Some float
                   | _ -> None)
           (fun float -> F float) ;
         case (Tag 4)
           ~title:"R"
           (obj2
              (req "field1" recursive)
              (req "field2" recursive))
           (function R (a, b) -> Some (a, b)
                   | _ -> None)
           (fun (a, b) -> R (a, b))
       ])

let () =

  bench_all
    "10000_element_int_list"
    Data_encoding.(list int31)
    ~num_iterations:1000
    (Array.to_list (Array.make 10000 0)) ;

  bench_all
    "option_element_int_list"
    Data_encoding.(list (option int31))
    (Array.to_list (Array.make 10000 (Some 0))) ;

  let encoding = Data_encoding.(list (result (option int31) string)) in
  let value = (Array.to_list (Array.make 10000 (Error "hello"))) in
  bench_all "option_result_element_list" encoding value;

  let encoding = Data_encoding.(list cases_encoding) in
  let value = Array.to_list (Array.make 1000 (R (R (A "asdf", B true), F 1.0))) in
  bench ~num_iterations:1000 "binary_encoding"
    (fun () -> ignore @@ Data_encoding.Binary.to_bytes encoding value) ;

  bench_all "binary_encoding_large_list"
    Data_encoding.(list cases_encoding)
    (Array.to_list (Array.make 2000 (R (R (A "asdf", B true), F 1.0))))

