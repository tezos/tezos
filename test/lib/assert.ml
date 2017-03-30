(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Hash
open Kaputt.Abbreviations

include Kaputt.Assertion

let format_msg = function None -> None | Some msg -> Some (msg ^ "\n")

let is_error ?(msg="") x =
  match x with
  | Error _ -> ()
  | Ok _ -> fail "Error _" "Ok _" msg

let is_ok ?(msg="") x =
  match x with
  | Ok _ -> ()
  | Error _ -> fail "Ok _" "Error _" msg

let equal_persist_list ?msg l1 l2 =
  let msg = format_msg msg in
  let pr_persist l =
    let res =
      String.concat ";" (List.map (fun s -> Printf.sprintf "%S" s) l) in
    Printf.sprintf "[%s]" res in
  Assert.make_equal_list ?msg (=) pr_persist l1 l2

let equal_block_hash_list ?msg l1 l2 =
  let msg = format_msg msg in
  let pr_block_hash = Block_hash.to_short_b58check in
  Assert.make_equal_list ?msg Block_hash.equal pr_block_hash l1 l2

let equal_string_list ?msg l1 l2 =
  let msg = format_msg msg in
  Assert.make_equal_list ?msg (=) (fun x -> x) l1 l2

let equal_string_option ?msg o1 o2 =
  let msg = format_msg msg in
  let prn = function
    | None -> "None"
    | Some s -> s in
  Assert.equal ?msg ~prn o1 o2

let equal_error_monad ?msg exn1 exn2 =
  let msg = format_msg msg in
  let prn exn = match exn with
    | Error_monad.Exn err -> Printexc.to_string err
    | Error_monad.Unclassified err -> err in
  Assert.equal ?msg ~prn exn1 exn2

let equal_block_set ?msg set1 set2 =
  let msg = format_msg msg in
  let b1 = Block_hash.Set.elements set1
  and b2 = Block_hash.Set.elements set2 in
  Assert.make_equal_list ?msg
    (fun h1 h2 -> Block_hash.equal h1 h2)
    Block_hash.to_string
    b1 b2

let equal_block_map ?msg ~eq map1 map2 =
  let msg = format_msg msg in
  let b1 = Block_hash.Map.bindings map1
  and b2 = Block_hash.Map.bindings map2 in
  Assert.make_equal_list ?msg
    (fun (h1, b1) (h2, b2) -> Block_hash.equal h1 h2 && eq b1 b2)
    (fun (h1, _) -> Block_hash.to_string h1)
    b1 b2

let equal_operation ?msg op1 op2 =
  let msg = format_msg msg in
  let eq op1 op2 =
    match op1, op2 with
    | None, None -> true
    | Some op1, Some op2 ->
        Store.Operation.equal op1 op2
    | _ -> false in
  let prn = function
    | None -> "none"
    | Some op -> Hash.Operation_hash.to_hex (Store.Operation.hash op) in
  Assert.equal ?msg ~prn ~eq op1 op2

let equal_block ?msg st1 st2 =
  let msg = format_msg msg in
  let eq st1 st2 =
    match st1, st2 with
    | None, None -> true
    | Some st1, Some st2 -> Store.Block_header.equal st1 st2
    | _ -> false in
  let prn = function
    | None -> "none"
    | Some st ->
        Hash.Block_hash.to_hex (Store.Block_header.hash st) in
  Assert.equal ?msg ~prn ~eq st1 st2

let equal_result ?msg r1 r2 ~equal_ok ~equal_err =
  let msg = format_msg msg in
  match r1, r2 with
  | Ok r1, Ok r2 -> equal_ok ?msg r1 r2
  | Error e1, Error e2 -> equal_err ?msg e1 e2
  | Ok r, Error e | Error e, Ok r ->
      Assert.fail_msg "Results are not the same"

let equal_exn ?msg exn1 exn2 =
  let msg = format_msg msg in
  let prn = Printexc.to_string in
  Assert.equal ?msg ~prn exn1 exn2

let test_fail ?(msg  = "") ?(prn = Assert.default_printer) f may_fail =
  try
    let value = f () in
    fail "any exception" ("no exception: " ^ prn value) msg
  with exn ->
    if not (may_fail exn) then
      fail "exception"
        (Printf.sprintf "unexpectec exception: %s" (Printexc.to_string exn))
        msg

let fail_msg fmt =
  Format.kasprintf Assert.fail_msg fmt

let fail expected given fmt =
  Format.kasprintf (Assert.fail expected given) fmt

let equal_float ?eq ?prn ?msg f1 f2 =
  match classify_float f1, classify_float f2 with
  | FP_nan, FP_nan -> ()
  | _ -> equal ?eq ?prn ?msg f1 f2
