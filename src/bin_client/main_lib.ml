(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Main Program *)

let cctxt ~base_dir ~block rpc_config =
  Client_context_unix.make_context ~base_dir ~block ~rpc_config (Client_context_unix.default_log ~base_dir)

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

(* Main (lwt) entry *)
let main ?only_commands () =
  let executable_name = Filename.basename Sys.executable_name in
  let global_options = Client_config.global_options () in
  let original_args, autocomplete =
    (* for shell aliases *)
    let rec move_autocomplete_token_upfront acc = function
      | "bash_autocomplete" :: prev_arg :: cur_arg :: script :: args ->
          let args = List.rev acc @ args in
          args, Some (prev_arg, cur_arg, script)
      | x :: rest -> move_autocomplete_token_upfront (x :: acc) rest
      | [] -> List.rev acc, None in
    match Array.to_list Sys.argv with
    | _ :: args -> move_autocomplete_token_upfront [] args
    | [] -> [], None in
  Random.self_init () ;
  ignore Cli_entries.(setup_formatter Format.std_formatter
                        (if Unix.isatty Unix.stdout then Ansi else Plain) Short) ;
  ignore Cli_entries.(setup_formatter Format.err_formatter
                        (if Unix.isatty Unix.stderr then Ansi else Plain) Short) ;
  Lwt.catch begin fun () -> begin
      Client_config.parse_config_args
        (cctxt ~base_dir:Client_context_unix.default_base_dir
           ~block:Client_context_unix.default_block
           RPC_client.default_config)
        original_args
      >>=? fun (parsed_config_file, parsed_args, config_commands, remaining) ->
      let rpc_config : RPC_client.config = {
        RPC_client.default_config with
        host = parsed_config_file.node_addr ;
        port = parsed_config_file.node_port ;
        tls = parsed_config_file.tls ;
      } in
      let ctxt = new RPC_client.http_ctxt rpc_config Media_type.all_media_types in
      begin match only_commands with
        | None ->
            get_commands_for_version ctxt
              parsed_args.block
              parsed_args.protocol >>|? fun (_version, commands_for_version)  ->
            Client_generic_rpcs.commands @
            Client_network.commands () @
            Client_keys.commands () @
            Client_protocols.commands () @
            Client_helpers.commands () @
            config_commands @
            commands_for_version
        | Some commands ->
            return (config_commands @ commands)
      end >>=? fun commands ->
      let commands =
        Cli_entries.add_manual
          ~executable_name
          ~global_options
          (if Unix.isatty Unix.stdout then Cli_entries.Ansi else Cli_entries.Plain)
          Format.std_formatter
          commands in
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
      begin match autocomplete with
        | Some (prev_arg, cur_arg, script) ->
            Cli_entries.autocompletion
              ~script ~cur_arg ~prev_arg ~args:original_args ~global_options
              commands client_config >>=? fun completions ->
            List.iter print_endline completions ;
            return ()
        | None ->
            Cli_entries.dispatch commands client_config remaining
      end
    end >>= function
    | Ok () ->
        Lwt.return 0
    | Error [ Cli_entries.Help command ] ->
        Cli_entries.usage
          Format.std_formatter
          ~executable_name
          ~global_options
          (match command with None -> [] | Some c -> [ c ]) ;
        Lwt.return 0
    | Error errs ->
        Cli_entries.pp_cli_errors
          Format.err_formatter
          ~executable_name
          ~global_options
          ~default:Error_monad.pp
          errs ;
        Lwt.return 1
  end begin function
    | Client_commands.Version_not_found ->
        Format.eprintf "@{<error>@{<title>Fatal error@}@} unknown protocol version." ;
        Lwt.return 1
    | Failure message ->
        Format.eprintf "@{<error>@{<title>Fatal error@}@} %s." message ;
        Lwt.return 1
    | exn ->
        Format.printf "@{<error>@{<title>Fatal error@}@} %s." (Printexc.to_string exn) ;
        Lwt.return 1
  end >>= fun retcode ->
  Format.fprintf Format.std_formatter "@." ;
  Format.fprintf Format.err_formatter "@." ;
  Lwt.return retcode
