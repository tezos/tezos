(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad

let rec (--) i j =
  if j < i then []
  else i :: (i+1) -- j

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
    Assert.equal ~msg:__LOC__ (take_n ~compare 1 xs) [9]
  end ;
  ListLabels.iter (permut [1;2;3;4;5;6;7;8;9]) ~f:begin fun xs ->
    Assert.equal ~msg:__LOC__ (take_n ~compare 3 xs) [7;8;9]
  end ;
  let inv_compare x y = compare y x in
  ListLabels.iter (permut [1;2;3;4;5;6;7;8;9]) ~f:begin fun xs ->
    Assert.equal ~msg:__LOC__ (take_n ~compare:inv_compare 3 xs) [3;2;1]
  end ;
  (* less elements than the bound. *)
  ListLabels.iter (permut [1;2;3;4;5;6;7;8;9]) ~f:begin fun xs ->
    Assert.equal ~msg:__LOC__ (take_n ~compare 12 xs) [1;2;3;4;5;6;7;8;9]
  end ;
  (* with duplicates. *)
  ListLabels.iter (permut [1;2;3;3;4;5;5;5;6]) ~f:begin fun xs ->
    Assert.equal ~msg:__LOC__ (take_n ~compare 3 xs) [5;5;6]
  end ;
  ListLabels.iter (permut [1;2;3;3;4;5;5;5;6]) ~f:begin fun xs ->
    Assert.equal ~msg:__LOC__ (take_n ~compare 5 xs) [4;5;5;5;6]
  end ;
  return ()

let tests : (string * (string -> unit tzresult Lwt.t)) list = [
  "take_n", test_take_n ;
]

let () =
  Test.run "utils." tests
