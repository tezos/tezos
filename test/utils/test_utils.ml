(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad
open Hash

let random = Random.State.make [|0|]
let randint = Random.State.int random
module Int = struct type t = int let compare = compare end
module SI = Set.Make(Int)
module IntBounded = Bounded(Int)

let print_list v =
  String.concat " " @@ "[" :: (List.map string_of_int v) @ ["]"]

let test_random_list () =
  let len = 1 + randint 100 in
  let l = Array.init len (fun _ -> randint 100) |> Array.to_list in
  let bound = randint (2*len + 1) + 1 in
  let b = IntBounded.create bound in
  List.iter (fun x -> IntBounded.insert x b) l;
  let result = IntBounded.get b in
  let expected = l |> List.sort compare |> List.rev |> take_n ~compare bound in
  let msg = Printf.sprintf "Bounded mismatch:
Bound: %d
Input: %s
Output: %s
Expected: %s
%s" bound (print_list l) (print_list result) (print_list expected) __LOC__ in
  Assert.equal ~msg result expected;;

let test_random_lists _ =
  for _ = 1 to 1000 do test_random_list (); done;
  return ();;

let test_prior_failure_case _ =
  let ob = (IntBounded.create 6) in
  List.iter (fun x -> IntBounded.insert x ob) [31; 94; 32; 15; 39; 18; 18; ];
  Assert.equal ~msg:__LOC__ (IntBounded.get ob) [18; 18; 31; 32; 39; 94];
  return ()

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
  "bounded", test_random_lists;
  "bounded_regression", test_prior_failure_case;
  "take_n", test_take_n ;
]

let () =
  Test.run "utils." tests
