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

let fail expected given msg =
  Format.kasprintf Pervasives.failwith
    "@[%s@ expected: %s@ got: %s@]" msg expected given

let fail_msg ?(expected="") ?(given="") fmt =
  Format.kasprintf (fail expected given) fmt

let default_printer _ = ""

let equal ?(eq=(=)) ?(prn=default_printer) ?(msg="") x y =
  if not (eq x y) then fail (prn x) (prn y) msg

let equal_operation ?msg op1 op2 =
  let eq op1 op2 =
    match op1, op2 with
    | None, None -> true
    | Some op1, Some op2 ->
        Operation.equal op1 op2
    | _ -> false in
  let prn = function
    | None -> "none"
    | Some op -> Operation_hash.to_b58check (Operation.hash op) in
  equal ?msg ~prn ~eq op1 op2

let equal_block ?msg st1 st2 =
  let eq st1 st2 =
    match st1, st2 with
    | None, None -> true
    | Some st1, Some st2 -> Block_header.equal st1 st2
    | _ -> false in
  let prn = function
    | None -> "none"
    | Some st -> Block_hash.to_b58check (Block_header.hash st) in
  equal ?msg ~prn ~eq st1 st2

let make_equal_list eq prn ?(msg="") x y =
  let rec iter i x y =
    match x, y with
    | hd_x :: tl_x, hd_y :: tl_y ->
        if eq hd_x hd_y then
          iter (succ i) tl_x tl_y
        else
          fail_msg ~expected:(prn hd_x) ~given:(prn hd_y)
            "%s (at index %d)" msg i
    | _ :: _, [] | [], _ :: _ ->
        fail_msg ~expected:"" ~given:""
          "%s (lists of different sizes %d %d)" msg
          (List.length x) (List.length y)
    | [], [] ->
        () in
  iter 0 x y

let equal_string_list ?msg l1 l2 =
  make_equal_list ?msg (=) (fun x -> x) l1 l2

let equal_string_list_list ?msg l1 l2 =
  let pr_persist l =
    let res =
      String.concat ";" (List.map (fun s -> Printf.sprintf "%S" s) l) in
    Printf.sprintf "[%s]" res in
  make_equal_list ?msg (=) pr_persist l1 l2

let equal_block_set ?msg set1 set2 =
  let b1 = Block_hash.Set.elements set1
  and b2 = Block_hash.Set.elements set2 in
  make_equal_list ?msg
    (fun h1 h2 -> Block_hash.equal h1 h2)
    Block_hash.to_string
    b1 b2

let equal_block_map ?msg ~eq map1 map2 =
  let b1 = Block_hash.Map.bindings map1
  and b2 = Block_hash.Map.bindings map2 in
  make_equal_list ?msg
    (fun (h1, b1) (h2, b2) -> Block_hash.equal h1 h2 && eq b1 b2)
    (fun (h1, _) -> Block_hash.to_string h1)
    b1 b2

let equal_block_hash_list ?msg l1 l2 =
  let pr_block_hash = Block_hash.to_short_b58check in
  make_equal_list ?msg Block_hash.equal pr_block_hash l1 l2

let is_false ?(msg="") x =
  if x then fail "false" "true" msg

let is_true ?(msg="") x =
  if not x then fail "true" "false" msg

let equal_checkpoint ?msg cp1 cp2 =
  let eq cp1 cp2 =
    match cp1, cp2 with
    | None, None -> true
    | Some (x, bh1), Some (y, bh2) ->
        Int32.equal x y &&
        (Block_hash.equal bh1 bh2)
    | _ -> false in
  let prn = function
    | None -> "none"
    | Some (_x, bh) ->
        (*let s = Printf.sprintf "%s" x in*)
        Block_hash.to_b58check bh
  in
  equal ?msg ~prn ~eq cp1 cp2
