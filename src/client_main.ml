(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Main Program *)

open Lwt.Infix
open Client_commands
open Error_monad

let cctxt config rpc_config =
  let startup =
    CalendarLib.Printer.Precise_Calendar.sprint
      "%Y-%m-%dT%H:%M:%SZ"
      (CalendarLib.Calendar.Precise.now ()) in
  let log channel msg = match channel with
    | "stdout" ->
        print_endline msg ;
        Lwt.return ()
    | "stderr" ->
        prerr_endline msg ;
        Lwt.return ()
    | log ->
        let (//) = Filename.concat in
        Lwt_utils.create_dir (config.base_dir // "logs" // log) >>= fun () ->
        Lwt_io.with_file
          ~flags: Unix.[ O_APPEND ; O_CREAT ; O_WRONLY ]
          ~mode: Lwt_io.Output
          Client_commands.(config.base_dir // "logs" // log // startup)
          (fun chan -> Lwt_io.write chan msg) in
  Client_commands.make_context ~config ~rpc_config log

(* Main (lwt) entry *)
let main () =
  Random.self_init () ;
  Sodium.Random.stir () ;
  Lwt.catch begin fun () ->
    let parsed_config_file, block = Client_config.preparse_args Sys.argv in
    let rpc_config : Client_rpcs.config = {
      Client_rpcs.default_config with
      host = parsed_config_file.node_addr ;
      port = parsed_config_file.node_port ;
      tls = parsed_config_file.tls ;
    } in
    begin
      Client_node_rpcs.Blocks.protocol rpc_config block >>= function
      | Ok version ->
          Lwt.return (Some version, Client_commands.commands_for_version version)
      | Error err ->
          Format.eprintf
            "Failed to acquire the protocol version from the node: %a.@."
            pp_print_error err ;
          Lwt.return (None, [])
    end >>= fun (_version, commands_for_version)  ->
    let commands =
      Client_generic_rpcs.commands @
      Client_network.commands () @
      Client_keys.commands () @
      Client_protocols.commands () @
      Client_helpers.commands () @
      commands_for_version in
    let (command, parsed_args) =
      Client_config.parse_args
        (Cli_entries.usage ~commands)
        (Cli_entries.inline_dispatch commands)
        Sys.argv in
    let config : Client_commands.cfg = {
      base_dir = parsed_config_file.base_dir ;
      force = parsed_args.force ;
      block ;
      web_port = Client_commands.default_cfg.web_port ;
    } in
    let rpc_config =
      if parsed_args.print_timings then
        { rpc_config with
          logger = Client_rpcs.timings_logger Format.err_formatter }
      else
        rpc_config
    in
    command (cctxt config rpc_config) >>= function
    | Ok () ->
        Lwt.return 0
    | Error [Cli_entries.Command_not_found] ->
        Format.eprintf "Unknown command, try `-help`.@." ;
        Lwt.return 1
    | Error [Cli_entries.Bad_argument (idx, _n, v)] ->
        Format.eprintf "There's a problem with argument %d, %s.@." idx v ;
        Lwt.return 1
    | Error [Cli_entries.Command_failed message] ->
        Format.eprintf "Command failed, %s.@." message ;
        Lwt.return 1
    | Error err ->
        Format.eprintf "Error: %a@." pp_print_error err ;
        Lwt.return 1
  end begin function
    | Arg.Help help ->
        Format.printf "%s%!" help ;
        Lwt.return 0
    | Arg.Bad help ->
        Format.eprintf "%s%!" help ;
        Lwt.return 1
    |  Client_commands.Version_not_found ->
        Format.eprintf "Unknown protocol version, try `list versions`.@." ;
        Lwt.return 1
    | Failure message ->
        Format.eprintf "Fatal error: %s@." message ;
        Lwt.return 1
    | exn ->
        Format.printf "Fatal internal error: %s@."
          (Printexc.to_string exn) ;
        Lwt.return 1
  end

(* Where all the user friendliness starts *)
let () = Pervasives.exit (Lwt_main.run (main ()))
