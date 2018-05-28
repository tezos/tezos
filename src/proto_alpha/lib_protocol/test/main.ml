(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let wrap (n, f) =
  Alcotest_lwt.test_case n `Quick begin fun _ () ->
    f () >>= function
    | Ok () -> Lwt.return_unit
    | Error error ->
        Format.kasprintf Pervasives.failwith "%a"
          (Format.pp_print_list
             (fun ppf e -> Data_encoding.Json.pp ppf (Error_monad.json_of_error e)))
          error
  end

let () =
  Alcotest.run "tezos-protocol-alpha" [
    "dsl", List.map wrap Test_dsl.tests ;
    "transaction", List.map wrap Test_transaction.tests ;
    "endorsement", List.map wrap Test_endorsement.tests ;
    "origination", List.map wrap Test_origination.tests ;
    "bigmaps", List.map wrap Test_big_maps.tests ;
    "michelson", List.map wrap Test_michelson.tests ;
    (* "activation", List.map wrap Test_activation.tests ; *)
  ]
