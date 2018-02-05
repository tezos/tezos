(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let () =
  let module Test = Test.Make(Error_monad) in
  Test.run "proto_alpha."
    ( Test_isolate_dsl.tests @
      Test_isolate_transaction.tests @
      Test_isolate_endorsement.tests @
      Test_isolate_origination.tests @
      Test_isolate_michelson.tests )
