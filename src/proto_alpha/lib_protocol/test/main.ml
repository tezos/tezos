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
    "activation", Activation.tests ;
    "endorsement", Endorsement.tests ;
    "double endorsement", Double_endorsement.tests ;
    "double baking", Double_baking.tests ;
    "seed", Seed.tests ;
    "baking", Baking.tests ;
  ]
