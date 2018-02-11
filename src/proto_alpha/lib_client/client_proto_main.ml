(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let protocol =
  Protocol_hash.of_b58check_exn
    "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"

let () =
  Client_commands.register protocol @@
  List.map (Cli_entries.map_command (new Proto_alpha.wrap_full_context)) @@
  Client_proto_programs_commands.commands () @
  Client_proto_contracts_commands.commands () @
  Client_proto_context_commands.commands () @
  Client_baking_main.commands ()
