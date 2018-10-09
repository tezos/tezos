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

open Proto_alpha

let known_ok_tez_litterals =
  [ 0L, "0" ;
    10L, "0.00001" ;
    100L, "0.0001" ;
    1_000L, "0.001" ;
    10_000L, "0.01" ;
    100_000L, "0.1" ;
    1_000_000L, "1" ;
    10_000_000L, "10" ;
    100_000_000L, "100" ;
    1_000_000_000L, "1000" ;
    10_000_000_000L, "10000" ;
    100_000_000_000L, "100000" ;
    1_000_000_000_000L, "1000000" ;
    1_000_000_000_001L, "1000000.000001" ;
    1_000_000_000_010L, "1000000.00001" ;
    1_000_000_000_100L, "1000000.0001" ;
    1_000_000_001_000L, "1000000.001" ;
    1_000_000_010_000L, "1000000.01" ;
    1_000_000_100_000L, "1000000.1" ;
    123_123_123_123_123_123L, "123123123123.123123" ;
    999_999_999_999_999_999L, "999999999999.999999" ]

let known_bad_tez_litterals =
  [ "10000." ;
    "100,." ;
    "100," ;
    "1,0000" ;
    "0.0000,1" ;
    "0.00,1" ;
    "0,1" ;
    "HAHA" ;
    "0.000,000,1" ;
    "0.0000000" ;
    "9,999,999,999,999.999,999"]

let fail expected given msg =
  Format.kasprintf Pervasives.failwith
    "@[%s@ expected: %s@ got: %s@]" msg expected given

let fail_msg fmt = Format.kasprintf (fail "" "") fmt

let default_printer _ = ""

let equal ?(eq=(=)) ?(prn=default_printer) ?(msg="") x y =
  if not (eq x y) then fail (prn x) (prn y) msg

let is_none ?(msg="") x =
  if x <> None then fail "None" "Some _" msg

let is_some ?(msg="") x =
  if x = None then fail "Some _" "None" msg

let test_known_tez_litterals () =
  List.iter
    (fun (v, s) ->
       let vv = Tez_repr.of_mutez v in
       let vs = Tez_repr.of_string s in
       let vs' = Tez_repr.of_string (String.concat "" (String.split_on_char ',' s)) in
       let vv = match vv with None -> fail_msg "could not unopt %Ld" v | Some vv -> vv in
       let vs = match vs with None -> fail_msg "could not unopt %s" s | Some vs -> vs in
       let vs' = match vs' with None -> fail_msg "could not unopt %s" s | Some vs' -> vs' in

       equal ~prn:Tez_repr.to_string vv vs ;
       equal ~prn:Tez_repr.to_string vv vs' ;
       equal ~prn:(fun s -> s) (Tez_repr.to_string vv) s)
    known_ok_tez_litterals ;
  List.iter
    (fun s ->
       let vs = Tez_repr.of_string s in
       is_none ~msg:("Unexpected successful parsing of " ^ s) vs)
    known_bad_tez_litterals ;
  return_unit

let test_random_tez_litterals () =
  for _ = 0 to 100_000 do
    let v = Random.int64 12L in
    let vv = Tez_repr.of_mutez v in
    let vv = match vv with None -> fail_msg "could not unopt %Ld" v | Some vv -> vv in
    let s = Tez_repr.to_string vv in
    let vs = Tez_repr.of_string s in
    let s' = String.concat "" (String.split_on_char ',' s) in
    let vs' = Tez_repr.of_string s' in
    is_some ~msg:("Could not parse " ^ s ^ " back") vs ;
    is_some ~msg:("Could not parse " ^ s ^ " back") vs' ;
    begin match vs with
      | None -> assert false
      | Some vs ->
          let rev = Tez_repr.to_int64 vs in
          equal ~prn:Int64.to_string ~msg:(Tez_repr.to_string vv) v rev
    end ;
    begin match vs' with
      | None -> assert false
      | Some vs' ->
          let rev = Tez_repr.to_int64 vs' in
          equal ~prn:Int64.to_string ~msg:(Tez_repr.to_string vv) v rev
    end
  done ;
  return_unit

let tests = [
  "tez-litterals", (fun _ -> test_known_tez_litterals ()) ;
  "rnd-tez-litterals", (fun _ -> test_random_tez_litterals ()) ;
]

let wrap (n, f) =
  Alcotest_lwt.test_case n `Quick begin fun _ () ->
    f () >>= function
    | Ok () -> Lwt.return_unit
    | Error error ->
        Format.kasprintf Pervasives.failwith "%a" pp_print_error error
  end

let tests = List.map wrap tests
