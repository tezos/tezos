(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* This file should not depend on any other file from tests. *)

let (>>?=) x y = match x with
  | Ok(a) -> y a
  | Error(b) -> fail @@ List.hd b

(** Like List.find but returns the index of the found element *)
let findi p =
  let rec aux p i = function
    | [] -> raise Not_found
    | x :: l -> if p x then (x,i) else aux p (i+1) l
  in
  aux p 0

exception Pair_of_list
let pair_of_list = function
  | [a;b] -> a,b
  | _ -> raise Pair_of_list
