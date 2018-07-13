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

type record = {
  a : int ;
  b : bool ;
  c : Z.t option ;
  d : float ;
}

let default_record = { a = 32 ; b = true ; c = Some Z.one ; d = 12.34 }

let record_obj_enc =
  conv
    (fun { a ; b ; c ; d } -> ((a, b), (c, d)))
    (fun ((a, b), (c, d)) -> { a ; b ; c ; d })
    (merge_objs
       (obj2
          (req "a" int31)
          (dft "b" bool false))
       (obj2
          (opt "c" z)
          (req "d" float)))

let record_tup_enc =
  conv
    (fun { a ; b ; c ; d } -> ((a, b, c), d))
    (fun ((a, b, c), d) -> { a ; b ; c ; d })
    (merge_tups
       (tup3 int31 bool (option z))
       (tup1 float))

let record_to_string { a ; b ; c ; d } =
  let c =
    match c with
    | None -> "none"
    | Some c -> Z.to_string c in
  Format.asprintf "(%d, %B, %s, %f)" a b c d

type variable_record = {
  p : int ;
  q : MBytes.t ;
}

let default_variable_record = { p = 23 ; q = MBytes.of_string "wwwxxyyzzz" }

let variable_record_obj_enc =
  conv
    (fun { p ; q } -> (p, q))
    (fun (p, q) -> { p ; q })
    (obj2
       (req "p" int31)
       (req "q" Variable.bytes))

let variable_record_tup_enc =
  conv
    (fun { p ; q } -> (p, q))
    (fun (p, q) -> { p ; q })
    (tup2 int31 Variable.bytes)

let variable_record_to_string { p ; q } =
  Format.asprintf "(%d, %a)" p MBytes.pp_hex q

type variable_left_record = {
  x : int ;
  y : MBytes.t ;
  z : int ;
}

let default_variable_left_record =
  { x = 98 ; y = MBytes.of_string "765" ; z = 4321 }

let variable_left_record_obj_enc =
  conv
    (fun { x ; y ; z } -> (x, y, z))
    (fun (x, y, z) -> { x ; y ; z })
    (obj3
       (req "x" int31)
       (req "y" Variable.bytes)
       (req "z" int31))

let variable_left_record_tup_enc =
  conv
    (fun { x ; y ; z } -> (x, y, z))
    (fun (x, y, z) -> { x ; y ; z })
    (tup3 int31 Variable.bytes int31)

let variable_left_record_to_string { x ; y ; z } =
  Format.asprintf "(%d, %a, %d)" x MBytes.pp_hex y z

type union = A of int | B of string | C of int | D of string | E

let union_enc =
  union [
    case (Tag 1)
      ~title:"A"
      int8
      (function A i -> Some i | _ -> None)
      (fun i -> A i) ;
    case (Tag 2)
      ~title:"B"
      string
      (function B s -> Some s | _ -> None)
      (fun s -> B s) ;
    case (Tag 3)
      ~title:"C"
      (obj1 (req "C" int8))
      (function C i -> Some i | _ -> None)
      (fun i -> C i) ;
    case (Tag 4)
      ~title:"D"
      (obj2
         (req "kind" (constant "D"))
         (req "data" (string)))
      (function D s -> Some ((), s) | _ -> None)
      (fun ((), s) -> D s) ;
    case (Tag 5)
      ~title:"E"
      empty
      (function E -> Some () | _ -> None)
      (fun () -> E) ;
  ]

let mini_union_enc =
  union [
    case (Tag 1)
      ~title:"A"
      int8
      (function A i -> Some i | _ -> None)
      (fun i -> A i) ;
  ]

let union_to_string = function
  | A i -> Printf.sprintf "A %d" i
  | B s -> Printf.sprintf "B %s" s
  | C i -> Printf.sprintf "C %d" i
  | D s -> Printf.sprintf "D %s" s
  | E -> "E"

let enum_enc =
  string_enum
    [ "one", 1 ; "two", 2 ; "three", 3 ; "four", 4 ; "five", 5 ; "six", 6 ]

let mini_enum_enc =
  string_enum
    [ "one", 1 ; "two", 2 ]

let mu_list_enc enc =
  mu "list" @@ fun mu_list_enc ->
  union [
    case (Tag 0)
      ~title:"Nil"
      empty
      (function [] -> Some () | _ :: _ -> None)
      (fun () -> []) ;
    case (Tag 1)
      ~title:"Cons"
      (obj2
         (req "value" enc)
         (req "next" mu_list_enc))
      (function x :: xs -> Some (x, xs) | [] -> None)
      (fun (x, xs) -> x :: xs) ;
  ]

let bounded_list ~total ~elements enc =
  check_size total (Variable.list (check_size elements enc))

module Alcotest = struct
  include Alcotest
  let float =
    testable
      Fmt.float
      (fun f1 f2 ->
         match classify_float f1, classify_float f2 with
         | FP_nan, FP_nan -> true
         | _ -> f1 = f2)
  let bytes =
    testable
      (Fmt.of_to_string (fun s -> let `Hex s = MBytes.to_hex s in s))
      MBytes.equal
  let z =
    testable
      (Fmt.of_to_string Z.to_string)
      Z.equal
  let n = z
  let record =
    testable
      (Fmt.of_to_string record_to_string)
      (=)
  let variable_record =
    testable
      (Fmt.of_to_string variable_record_to_string)
      (=)
  let variable_left_record =
    testable
      (Fmt.of_to_string variable_left_record_to_string)
      (=)
  let union =
    testable
      (Fmt.of_to_string union_to_string)
      (=)
end
