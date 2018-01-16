(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Main Program *)

let cctxt ~base_dir ~block rpc_config =
  Client_commands.make_context ~base_dir ~block ~rpc_config (Client_commands.default_log ~base_dir)

(* Main (lwt) entry *)
let main () =
  Random.self_init () ;
  Sodium.Random.stir () ;
  Lwt.catch begin fun () ->
    let original_args = List.tl (Array.to_list Sys.argv) in
    begin
      Client_config.parse_config_args
        (cctxt ~base_dir:Client_commands.default_base_dir
           ~block:Client_commands.default_block
           Client_rpcs.default_config)
        original_args
      >>=? fun (parsed_config_file, parsed_args, remaining) ->
      let rpc_config : Client_rpcs.config = {
        Client_rpcs.default_config with
        host = parsed_config_file.node_addr ;
        port = parsed_config_file.node_port ;
        tls = parsed_config_file.tls ;
      } in
      let ctxt = new Client_rpcs.http_ctxt rpc_config in
      begin
        Client_node_rpcs.Blocks.protocol ctxt parsed_args.block >>= function
        | Ok version -> begin
            match parsed_args.protocol with
            | None ->
                return (Some version, Client_commands.commands_for_version version)
            | Some given_version -> begin
                if not (Protocol_hash.equal version given_version) then
                  Format.eprintf
                    "@[<v 2>Warning:@,\
                     The protocol provided via `-protocol` (%a)@,\
                     is not the one retrieved from the node (%a).@."
                    Protocol_hash.pp_short given_version
                    Protocol_hash.pp_short version ;
                return (Some version, Client_commands.commands_for_version given_version)
              end
          end
        | Error errs -> begin
            match parsed_args.protocol with
            | None -> begin
                Format.eprintf
                  "@[<v 2>Ignored error:@,Failed to acquire the protocol version from the node@,%a@."
                  (Format.pp_print_list pp) errs ;
                return (None, [])
              end
            | Some version ->
                return (Some version, Client_commands.commands_for_version version)
          end
      end >>=? fun (_version, commands_for_version)  ->
      let commands =
        Client_generic_rpcs.commands @
        Client_network.commands () @
        Client_keys.commands () @
        Client_protocols.commands () @
        Client_helpers.commands () @
        Client_debug.commands () @
        commands_for_version in
      let rpc_config =
        if parsed_args.print_timings then
          { rpc_config with
            logger = RPC_client.timings_logger Format.err_formatter }
        else if parsed_args.log_requests
        then { rpc_config with logger = RPC_client.full_logger Format.err_formatter }
        else rpc_config
      in
      let client_config =
        cctxt ~block:parsed_args.block ~base_dir:parsed_config_file.base_dir rpc_config in
      (Cli_entries.dispatch
         ~global_options:Client_config.global_options
         commands
         client_config
         remaining) end >>=
    Cli_entries.handle_cli_errors
      ~stdout:Format.std_formatter
      ~stderr:Format.err_formatter
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
