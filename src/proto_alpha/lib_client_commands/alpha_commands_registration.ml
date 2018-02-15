(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let () =
  Client_commands.register Proto_alpha.hash @@
  List.map (Cli_entries.map_command (new Proto_alpha.wrap_full_context)) @@
  Client_proto_programs_commands.commands () @
  Client_proto_contracts_commands.commands () @
  Client_proto_context_commands.commands ()
