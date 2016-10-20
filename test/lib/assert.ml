(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Kaputt.Abbreviations

include Kaputt.Assertion

let format_msg = function None -> None | Some msg -> Some (msg ^ "\n")

let equal_persist_list ?msg l1 l2 =
  let msg = format_msg msg in
  let pr_persist l =
    let res =
      String.concat ";" (List.map (fun s -> Printf.sprintf "%S" s) l) in
    Printf.sprintf "[%s]" res in
  Assert.make_equal_list ?msg (=) pr_persist l1 l2

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

let equal_block_map ?msg ~eq map1 map2 =
  let msg = format_msg msg in
  let open Hash in
  let module BlockMap = Hash_map(Block_hash) in
  Assert.equal ?msg ~eq map1 map2

let equal_operation ?msg op1 op2 =
  let msg = format_msg msg in
  let eq op1 op2 =
    match op1, op2 with
    | None, None -> true
    | Some (h1, op1), Some (h2, op2) ->
        Hash.Operation_hash.equal h1 h2 && op1 = op2
    | _ -> false in
  let prn = function
    | None -> "none"
    | Some (h, op) -> Hash.Operation_hash.to_hex h in
  Assert.equal ?msg ~prn ~eq op1 op2

let equal_block ?msg st1 st2 =
  let msg = format_msg msg in
  let eq st1 st2 =
    match st1, st2 with
    | None, None -> true
    | Some (h1, st1), Some (h2, st2) ->
        Hash.Block_hash.equal h1 h2 && st1 = st2
    | _ -> false in
  let prn = function
    | None -> "none"
    | Some (h, st) -> Hash.Block_hash.to_hex h in
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

