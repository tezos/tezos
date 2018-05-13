(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let () =
  Random.init 100 ;
  Alcotest.run "tezos-data-encoding" [
    "success", Success.tests ;
    "invalid_encoding", Invalid_encoding.tests ;
    "read_failure", Read_failure.tests ;
    "write_failure", Write_failure.tests ;
    "randomized", Randomized.tests ;
  ]
