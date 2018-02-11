(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let fail expected given msg =
  Format.kasprintf Pervasives.failwith
    "@[%s@ expected: %s@ got: %s@]" msg expected given
let fail_msg fmt = Format.kasprintf (fail "" "") fmt

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
    | Some op -> Operation_hash.to_hex (Operation.hash op) in
  equal ?msg ~prn ~eq op1 op2

let equal_block ?msg st1 st2 =
  let eq st1 st2 =
    match st1, st2 with
    | None, None -> true
    | Some st1, Some st2 -> Block_header.equal st1 st2
    | _ -> false in
  let prn = function
    | None -> "none"
    | Some st -> Block_hash.to_hex (Block_header.hash st) in
  equal ?msg ~prn ~eq st1 st2
