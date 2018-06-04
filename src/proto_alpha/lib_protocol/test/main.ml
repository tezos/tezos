(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let () =
  Alcotest.run "protocol_alpha" [
    "transfer", Transfer.tests ;
    "origination", Origination.tests ;
    "baking", Baking.tests ;
    "activation", Activation.tests ;
    "seed", Seed.tests ;
    "endorsement", Endorsement.tests ;
    "double endorsement", Double_endorsement.tests ;
    "double baking", Double_baking.tests ;
  ]
