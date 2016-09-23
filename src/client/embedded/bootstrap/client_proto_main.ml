(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let protocol =
  Protocol_hash.of_b48check
    "4prgmSgbaeMKbgLtLjpsHaDD9QvG2dbC2bLq2XBmyxd2RJgLFpcAg"

let () =
  Client_version.register protocol @@
  Client_proto_programs.commands () @
  Client_proto_contracts.commands () @
  Client_proto_context.commands ()
