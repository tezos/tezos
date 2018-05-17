(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)


let test_rt_opt name testable enc dec input =
  try
    let roundtripped = dec (enc input) in
    Alcotest.check (Alcotest.option testable) name (Some input) roundtripped
  with
    exc ->
      Alcotest.failf "%s failed for %a: exception whilst decoding: %s"
        name (Alcotest.pp testable) input (Printexc.to_string exc)

let test_decode_opt_safe name testable dec encoded =
  match dec encoded with
  | Some _ | None -> ()
  | exception exc ->
      Alcotest.failf "%s failed for %a: exception whilst decoding: %s"
        name (Alcotest.pp testable) encoded (Printexc.to_string exc)

let test_decode_opt_fail name testable dec encoded =
  try
    let decoded = dec encoded in
    Alcotest.check (Alcotest.option testable) name None decoded
  with
    exc ->
      Alcotest.failf "%s failed: exception whilst decoding: %s"
        name (Printexc.to_string exc)
