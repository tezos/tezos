(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_config

let get_commands_for_version ctxt block protocol =
  Block_services.protocol ctxt block >>= function
  | Ok version -> begin
      match protocol with
      | None ->
          return (Some version, Client_commands.commands_for_version version)
      | Some given_version -> begin
          if not (Protocol_hash.equal version given_version) then
            Format.eprintf
              "@[<v 2>Warning:@,\
               The protocol provided via `-protocol` (%a)@,\
               is not the one retrieved from the node (%a).@]@\n@."
              Protocol_hash.pp_short given_version
              Protocol_hash.pp_short version ;
          return (Some version, Client_commands.commands_for_version given_version)
        end
    end
  | Error errs -> begin
      match protocol with
      | None -> begin
          Format.eprintf
            "@[<v 2>@{<warning>@{<title>Warning@}@}@,\
             Failed to acquire the protocol version from the node@,%a@]@\n@."
            (Format.pp_print_list pp) errs ;
          return (None, [])
        end
      | Some version ->
          return (Some version, Client_commands.commands_for_version version)
    end

let select_commands ctxt { block ; protocol } =
  get_commands_for_version ctxt block protocol >>|? fun (_, commands_for_version)  ->
  Client_rpc_commands.commands @
  Client_network_commands.commands () @
  Client_keys_commands.commands () @
  Client_protocols.commands () @
  Client_helpers_commands.commands () @
  commands_for_version

let () = Client_main_run.run select_commands
