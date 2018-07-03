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

let rec permut = function
  | [] -> [[]]
  | x :: xs ->
      let insert xs =
        let rec loop acc left right =
          match right with
          | [] -> List.rev (x :: left) :: acc
          | y :: ys ->
              loop
                ((List.rev_append left (x :: right)) :: acc)
                (y :: left) ys in
        loop [] [] xs in
      List.concat (List.map insert (permut xs))

let test_take_n _ =
  ListLabels.iter (permut [1;2;3;4;5;6;7;8;9]) ~f:begin fun xs ->
    Assert.equal ~msg:__LOC__ (TzList.take_n ~compare 1 xs) [9]
  end ;
  ListLabels.iter (permut [1;2;3;4;5;6;7;8;9]) ~f:begin fun xs ->
    Assert.equal ~msg:__LOC__ (TzList.take_n ~compare 3 xs) [7;8;9]
  end ;
  let inv_compare x y = compare y x in
  ListLabels.iter (permut [1;2;3;4;5;6;7;8;9]) ~f:begin fun xs ->
    Assert.equal ~msg:__LOC__ (TzList.take_n ~compare:inv_compare 3 xs) [3;2;1]
  end ;
  (* less elements than the bound. *)
  ListLabels.iter (permut [1;2;3;4;5;6;7;8;9]) ~f:begin fun xs ->
    Assert.equal ~msg:__LOC__ (TzList.take_n ~compare 12 xs) [1;2;3;4;5;6;7;8;9]
  end ;
  (* with duplicates. *)
  ListLabels.iter (permut [1;2;3;3;4;5;5;5;6]) ~f:begin fun xs ->
    Assert.equal ~msg:__LOC__ (TzList.take_n ~compare 3 xs) [5;5;6]
  end ;
  ListLabels.iter (permut [1;2;3;3;4;5;5;5;6]) ~f:begin fun xs ->
    Assert.equal ~msg:__LOC__ (TzList.take_n ~compare 5 xs) [4;5;5;5;6]
  end

let tests = [
  "take_n", `Quick, test_take_n ;
]

let () =
  Alcotest.run "stdlib" [
    "tzList", tests ;
  ]
