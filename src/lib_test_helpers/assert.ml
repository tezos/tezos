(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Kaputt.Assertion

module Assert = Kaputt.Abbreviations.Assert

let format_msg = function None -> None | Some msg -> Some (msg ^ "\n")

let is_error ?(msg="") = function
  | Error _ -> ()
  | Ok _ -> fail "Error _" "Ok _" msg

let is_ok ?(msg="") = function
  | Ok _ -> ()
  | Error _ -> fail "Ok _" "Error _" msg

let equal_string_list_list ?msg l1 l2 =
  let msg = format_msg msg in
  let pr_persist l =
    let res =
      String.concat ";" (List.map (fun s -> Printf.sprintf "%S" s) l) in
    Printf.sprintf "[%s]" res in
  Assert.make_equal_list ?msg (=) pr_persist l1 l2

let equal_string_list ?msg l1 l2 =
  let msg = format_msg msg in
  Assert.make_equal_list ?msg (=) (fun x -> x) l1 l2

let equal_string_option ?msg o1 o2 =
  let msg = format_msg msg in
  let prn = function
    | None -> "None"
    | Some s -> s in
  Assert.equal ?msg ~prn o1 o2

let equal_result ?msg r1 r2 ~equal_ok ~equal_err =
  let msg = format_msg msg in
  match r1, r2 with
  | Ok r1, Ok r2 -> equal_ok ?msg r1 r2
  | Error e1, Error e2 -> equal_err ?msg e1 e2
  | Ok _, Error _ | Error _, Ok _ ->
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
