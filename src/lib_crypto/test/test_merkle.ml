(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad
open Utils.Infix

type tree =
  | Empty
  | Leaf of int
  | Node of tree * tree

let rec list_of_tree = function
  | Empty -> [], 0
  | Leaf x -> [x], 1
  | Node (x, y) ->
      let x, sx = list_of_tree x
      and y, sy = list_of_tree y in
      assert (sx = sy) ;
      x @ y, sx + sy

module Merkle = Blake2B.Generic_Merkle_tree(struct
    type t = tree
    type elt = int
    let empty = Empty
    let leaf i = Leaf i
    let node x y = Node (x, y)
  end)

let rec compare_list xs ys =
  match xs, ys with
  | [], [] -> true
  | [x], y :: ys when x = y -> ys = [] || compare_list xs ys
  | x :: xs, y :: ys when x = y -> compare_list xs ys
  | _, _ -> false

let check_size i =
  let l = 0 -- i in
  let l2, _ = list_of_tree (Merkle.compute l) in
  if compare_list l l2 then
    return ()
  else
    failwith "Failed for %d: %a"
      i
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_string ppf ";")
         Format.pp_print_int)
      l2

let test_compute _ =
  iter_s check_size (0--99)

let check_path i =
  let l = 0 -- i in
  let orig = Merkle.compute l in
  iter_s (fun j ->
      let path = Merkle.compute_path l j in
      let found, pos = Merkle.check_path path j in
      if found = orig && j = pos then
        return ()
      else
        failwith "Failed for %d in %d." j i)
    l

let test_path _ =
  iter_s check_path (0--128)

let tests : (string * (string -> unit tzresult Lwt.t)) list = [
  "compute", test_compute ;
  "path", test_path ;
]

let () =
  let module Test = Tezos_test_helpers.Test.Make(Error_monad) in
  Test.run "merkel." tests
