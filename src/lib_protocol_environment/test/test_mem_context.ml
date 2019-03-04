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

open Tezos_protocol_environment_memory

(** Context creation *)

let create_block2 ctxt =
  Context.set ctxt ["a"; "b"] (MBytes.of_string "Novembre") >>= fun ctxt ->
  Context.set ctxt ["a"; "c"] (MBytes.of_string "Juin") >>= fun ctxt ->
  Context.set ctxt ["version";] (MBytes.of_string "0.0") >>= fun ctxt ->
  Lwt.return ctxt

let create_block3a ctxt =
  Context.del ctxt ["a"; "b"] >>= fun ctxt ->
  Context.set ctxt ["a"; "d"] (MBytes.of_string "Mars") >>= fun ctxt ->
  Lwt.return ctxt

let create_block3b ctxt =
  Context.del ctxt ["a"; "c"] >>= fun ctxt ->
  Context.set ctxt ["a"; "d"] (MBytes.of_string "Février") >>= fun ctxt ->
  Lwt.return ctxt

type t = {
  genesis: Context.t ;
  block2: Context.t ;
  block3a: Context.t ;
  block3b: Context.t ;
}

let wrap_context_init f _ () =
  let genesis = Context.empty in
  create_block2 genesis >>= fun block2 ->
  create_block3a block2 >>= fun block3a ->
  create_block3b block2 >>= fun block3b ->
  f { genesis; block2 ; block3a; block3b } >>= fun result ->
  Lwt.return result

(** Simple test *)

let c = function
  | None -> None
  | Some s -> Some (MBytes.to_string s)

let test_simple { block2 = ctxt ; _ } =
  Context.get ctxt ["version"] >>= fun version ->
  Assert.equal_string_option ~msg:__LOC__ (c version) (Some "0.0") ;
  Context.get ctxt ["a";"b"] >>= fun novembre ->
  Assert.equal_string_option (Some "Novembre") (c novembre) ;
  Context.get ctxt ["a";"c"] >>= fun juin ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Juin") (c juin) ;
  Lwt.return_unit

let test_continuation { block3a = ctxt ; _ } =
  Context.get ctxt ["version"] >>= fun version ->
  Assert.equal_string_option ~msg:__LOC__ (Some "0.0") (c version) ;
  Context.get ctxt ["a";"b"] >>= fun novembre ->
  Assert.is_none ~msg:__LOC__ (c novembre) ;
  Context.get ctxt ["a";"c"] >>= fun juin ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Juin") (c juin) ;
  Context.get ctxt ["a";"d"] >>= fun mars ->
  Assert.equal_string_option ~msg:__LOC__  (Some "Mars") (c mars) ;
  Lwt.return_unit

let test_fork { block3b = ctxt ; _ } =
  Context.get ctxt ["version"] >>= fun version ->
  Assert.equal_string_option ~msg:__LOC__ (Some "0.0") (c version) ;
  Context.get ctxt ["a";"b"] >>= fun novembre ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
  Context.get ctxt ["a";"c"] >>= fun juin ->
  Assert.is_none ~msg:__LOC__ (c juin) ;
  Context.get ctxt ["a";"d"] >>= fun mars ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Février") (c mars) ;
  Lwt.return_unit

let test_replay { genesis = ctxt0 ; _ }  =
  Context.set ctxt0 ["version"] (MBytes.of_string "0.0") >>= fun ctxt1 ->
  Context.set ctxt1 ["a"; "b"] (MBytes.of_string "Novembre") >>= fun ctxt2 ->
  Context.set ctxt2 ["a"; "c"] (MBytes.of_string "Juin") >>= fun ctxt3 ->
  Context.set ctxt3 ["a"; "d"] (MBytes.of_string "July") >>= fun ctxt4a ->
  Context.set ctxt3 ["a"; "d"] (MBytes.of_string "Juillet") >>= fun ctxt4b ->
  Context.set ctxt4a ["a"; "b"] (MBytes.of_string "November") >>= fun ctxt5a ->
  Context.get ctxt4a ["a";"b"] >>= fun novembre ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
  Context.get ctxt5a ["a";"b"] >>= fun november ->
  Assert.equal_string_option ~msg:__LOC__ (Some "November") (c november) ;
  Context.get ctxt5a ["a";"d"] >>= fun july ->
  Assert.equal_string_option ~msg:__LOC__ (Some "July") (c july) ;
  Context.get ctxt4b ["a";"b"] >>= fun novembre ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Novembre") (c novembre) ;
  Context.get ctxt4b ["a";"d"] >>= fun juillet ->
  Assert.equal_string_option ~msg:__LOC__ (Some "Juillet") (c juillet) ;
  Lwt.return_unit

let fold_keys s k ~init ~f =
  let rec loop k acc =
    Context.fold s k ~init:acc
      ~f:(fun file acc ->
          match file with
          | `Key k -> f k acc
          | `Dir k -> loop k acc) in
  loop k init
let keys t = fold_keys t ~init:[] ~f:(fun k acc -> Lwt.return (k :: acc))

let test_fold { genesis = ctxt ; _ } =
  Context.set ctxt ["a"; "b"] (MBytes.of_string "Novembre") >>= fun ctxt ->
  Context.set ctxt ["a"; "c"] (MBytes.of_string "Juin") >>= fun ctxt ->
  Context.set ctxt ["a"; "d"; "e"] (MBytes.of_string "Septembre") >>= fun ctxt ->
  Context.set ctxt ["f";] (MBytes.of_string "Avril") >>= fun ctxt ->
  Context.set ctxt ["g"; "h"] (MBytes.of_string "Avril") >>= fun ctxt ->
  keys ctxt [] >>= fun l ->
  Assert.equal_string_list_list ~msg:__LOC__
    [["a";"b"];
     ["a";"c"];
     ["a";"d";"e"];
     ["f"];
     ["g";"h"]] (List.sort compare l) ;
  keys ctxt ["a"] >>= fun l ->
  Assert.equal_string_list_list
    ~msg:__LOC__ [["a";"b"]; ["a";"c"]; ["a";"d";"e"]]
    (List.sort compare l) ;
  keys ctxt ["f"] >>= fun l ->
  Assert.equal_string_list_list ~msg:__LOC__ [] l ;
  keys ctxt ["g"] >>= fun l ->
  Assert.equal_string_list_list ~msg:__LOC__ [["g";"h"]] l ;
  keys ctxt ["i"] >>= fun l ->
  Assert.equal_string_list_list ~msg:__LOC__ [] l ;
  Lwt.return_unit

(******************************************************************************)

let tests = [
  "simple", test_simple ;
  "continuation", test_continuation ;
  "fork", test_fork ;
  "replay", test_replay ;
  "fold", test_fold ;
]

let tests =
  List.map
    (fun (n, f) -> Alcotest_lwt.test_case n `Quick (wrap_context_init f))
    tests
