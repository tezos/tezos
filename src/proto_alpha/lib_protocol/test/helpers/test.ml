(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Wraps an alcotest so that it prints correcly errors from the Error_monad. *)
let tztest name speed f =
  Alcotest_lwt.test_case name speed begin fun _sw () ->
    f () >>= function
    | Ok () -> Lwt.return_unit
    | Error err ->
        Tezos_stdlib_unix.Logging_unix.close () >>= fun () ->
        Format.eprintf "WWW %a@." pp_print_error err ;
        Lwt.fail Alcotest.Test_error
  end
