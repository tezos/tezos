(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let select_commands _ _ =
  return
    (List.map
       (Clic.map_command (new Proto_alpha.wrap_full))
       (Delegate_commands.commands ()))

let () = Client_main_run.run select_commands
