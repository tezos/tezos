(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Main Program *)

(* Main (lwt) entry *)
let main select_commands =
  let cctxt ~base_dir ~block rpc_config =
    Client_context.make_context
      ~base_dir ~block ~rpc_config
      (Client_context.default_log ~base_dir) in
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
        (cctxt ~base_dir:Client_context.default_base_dir
           ~block:Client_context.default_block
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
      select_commands ctxt parsed_args >>=? fun commands ->
      let commands =
        Cli_entries.add_manual
          ~executable_name
          ~global_options
          (if Unix.isatty Unix.stdout then Cli_entries.Ansi else Cli_entries.Plain)
          Format.std_formatter
          (config_commands @ commands) in
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

(* Where all the user friendliness starts *)
let run select_commands =
  Pervasives.exit (Lwt_main.run (main select_commands))
