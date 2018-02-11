(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let fail expected given msg =
  Format.kasprintf failwith
    "@[%s@ expected: %s@ got: %s@]" msg expected given
let fail_msg fmt = Format.kasprintf (fail "" "") fmt

let default_printer _ = ""

let equal ?(eq=(=)) ?(prn=default_printer) ?(msg="") x y =
  if not (eq x y) then fail (prn x) (prn y) msg

let not_equal ?(eq=(=)) ?(prn=default_printer) ?(msg="") x y =
  if eq x y then fail (prn x) (prn y) msg

let is_some ?(msg = "Assert.is_some: error.") a =
  match a with
  | None -> fail "Some _" "None" msg
  | Some _ -> ()
let is_true ?(msg="") x =
  if not x then fail "true" "false" msg

let equal_float ?eq ?prn ?msg f1 f2 =
  match classify_float f1, classify_float f2 with
  | FP_nan, FP_nan -> ()
  | _ -> equal ?eq ?prn ?msg f1 f2

let test_fail ?(msg  = "") ?(prn = default_printer) f may_fail =
  try
    let value = f () in
    fail "any exception" ("no exception: " ^ prn value) msg
  with exn ->
    if not (may_fail exn) then
      fail "exception"
        (Printf.sprintf "unexpectec exception: %s" (Printexc.to_string exn))
        msg
