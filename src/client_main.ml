(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Main Program *)

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
    let original_args = List.tl (Array.to_list Sys.argv) in
    begin
      Client_config.parse_config_args
        (cctxt Client_commands.default_cfg Client_rpcs.default_config)
        original_args
      >>=? fun (parsed_config_file, parsed_args, remaining) ->
      let rpc_config : Client_rpcs.config = {
        Client_rpcs.default_config with
        host = parsed_config_file.node_addr ;
        port = parsed_config_file.node_port ;
        tls = parsed_config_file.tls ;
      } in
      begin
        Client_node_rpcs.Blocks.protocol rpc_config parsed_args.block >>= function
        | Ok version ->
            return (Some version, Client_commands.commands_for_version version)
        | Error errs ->
            Format.eprintf
              "@[<v 2>Ignored error:@,Failed to acquire the protocol version from the node@,%a@."
              (Format.pp_print_list pp) errs ;
            return (None, [])
      end >>=? fun (_version, commands_for_version)  ->
      let commands =
        Client_generic_rpcs.commands @
        Client_network.commands () @
        Client_keys.commands () @
        Client_protocols.commands () @
        Client_helpers.commands () @
        commands_for_version in
      let config : Client_commands.cfg = {
        base_dir = parsed_config_file.base_dir ;
        force = parsed_args.force ;
        block = parsed_args.block ;
        web_port = Client_commands.default_cfg.web_port ;
      } in
      let rpc_config =
        if parsed_args.print_timings then
          { rpc_config with
            logger = Client_rpcs.timings_logger Format.err_formatter }
        else if parsed_args.log_requests
        then {rpc_config with logger = Client_rpcs.full_logger Format.err_formatter }
        else rpc_config
      in
      let client_config = (cctxt config rpc_config) in
      (Cli_entries.dispatch
         ~global_options:Client_config.global_options
         commands
         client_config
         remaining) end >>=
    Cli_entries.handle_cli_errors
      ~stdout: Format.std_formatter
      ~stderr: Format.err_formatter
      ~global_options:Client_config.global_options
    >>= function
    | Ok i ->
        Lwt.return i
    | Error errs ->
        Format.eprintf "@[<v 2>Fatal error:@,%a@."
          (Format.pp_print_list Error_monad.pp) errs ;
        Lwt.return 1
  end begin function
    | Arg.Help help ->
        Format.printf "%s%!" help ;
        Lwt.return 0
    |  Client_commands.Version_not_found ->
        Format.eprintf "Unknown protocol version.@." ;
        Lwt.return 1
    | Failure message ->
        Format.eprintf
          "Fatal error: %s@." message ;
        Lwt.return 1
    | exn ->
        Format.printf "Fatal internal error: %s@."
          (Printexc.to_string exn) ;
        Lwt.return 1
  end

(* Where all the user friendliness starts *)
let () = Pervasives.exit (Lwt_main.run (main ()))
