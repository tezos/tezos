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
    "data_encoding", Test_data_encoding.tests ;
    "stream_data_encoding", Test_stream_data_encoding.tests ;
  ]
