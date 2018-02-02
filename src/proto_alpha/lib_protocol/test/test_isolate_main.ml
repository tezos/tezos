(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let () =
  Test_isolate_dsl.main ();
  Test_isolate_transaction.main ();
  Test_isolate_endorsement.main ();
  Test_isolate_origination.main ();
  Test_isolate_michelson.main ()

