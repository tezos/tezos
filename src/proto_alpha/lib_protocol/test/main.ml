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
    ( Test_dsl.tests @
      Test_transaction.tests @
      Test_endorsement.tests @
      Test_origination.tests @
      Test_big_maps.tests @
      Test_michelson.tests )
