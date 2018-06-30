(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let () =
  Client_commands.register Proto_alpha.hash @@ fun _network ->
  List.map (Clic.map_command (new Proto_alpha.wrap_full)) @@
  Delegate_commands.delegate_commands ()
