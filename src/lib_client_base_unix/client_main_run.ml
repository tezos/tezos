(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Main Program *)

open Client_context_unix

let builtin_commands =
  let open Clic in
  [
    command
      ~desc: "List the protocol versions that this client understands."
      no_options
      (fixed [ "list" ; "understood" ; "protocols" ])
      (fun () (cctxt : #Client_context.full) ->
         Lwt_list.iter_s
           (fun (ver, _) -> cctxt#message "%a" Protocol_hash.pp_short ver)
           (Client_commands.get_versions ()) >>= fun () ->
         return_unit) ;
  ]

(* Duplicated from the node, here for now since the client still
   embeds the baker. To be moved where appropriate when the baker is
   definitively moved/factorized in its own binary. *)
let find_log_rules () =
  match Option.try_with (fun () -> Sys.getenv "TEZOS_LOG"),
        Option.try_with (fun () -> Sys.getenv "LWT_LOG")
  with
  | Some rules, None -> "environment variable TEZOS_LOG", Some rules
  | None, Some rules -> "environment variable LWT_LOG", Some rules
  | None, None -> "default rules", None
  | Some rules, Some _ ->
      Format.eprintf
        "@[<v 2>@{<warning>@{<title>Warning@}@} \
         Both environment variables TEZOS_LOG and LWT_LOG \
         defined, using TEZOS_LOG.@]@\n@." ;
      "environment varible TEZOS_LOG", Some rules

let init_logger () =
  Lwt_log_core.add_rule "*" Lwt_log_core.Notice ;
  let origin, rules = find_log_rules () in
  Option.iter rules ~f:begin fun rules ->
    try Lwt_log_core.load_rules rules ~fail_on_error:true
    with _ ->
      Pervasives.failwith
        (Format.asprintf "Incorrect log rules defined in %s." origin)
  end ;
  Logging_unix.(init ~template:"$(message)" Stderr)

(* Main (lwt) entry *)
let main select_commands =
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
  ignore Clic.(setup_formatter Format.std_formatter
                 (if Unix.isatty Unix.stdout then Ansi else Plain) Short) ;
  ignore Clic.(setup_formatter Format.err_formatter
                 (if Unix.isatty Unix.stderr then Ansi else Plain) Short) ;
  init_logger () >>= fun () ->
  Lwt.catch begin fun () -> begin
      Client_config.parse_config_args
        (new unix_full
          ~block:Client_config.default_block
          ~confirmations:None
          ~base_dir:Client_config.default_base_dir
          ~rpc_config:RPC_client.default_config)
        original_args
      >>=? fun (parsed_config_file, parsed_args, config_commands, remaining) ->
      let rpc_config : RPC_client.config = {
        RPC_client.default_config with
        host = parsed_config_file.node_addr ;
        port = parsed_config_file.node_port ;
        tls = parsed_config_file.tls ;
      } in
      let ctxt = new RPC_client.http_ctxt rpc_config Media_type.all_media_types in
      let rpc_config =
        if parsed_args.print_timings then
          { rpc_config with
            logger = RPC_client.timings_logger Format.err_formatter }
        else if parsed_args.log_requests
        then { rpc_config with logger = RPC_client.full_logger Format.err_formatter }
        else rpc_config
      in
      let client_config =
        new unix_full
          ~block:parsed_args.block
          ~confirmations:parsed_args.confirmations
          ~base_dir:parsed_config_file.base_dir
          ~rpc_config:rpc_config in
      Client_keys.register_signer
        (module Tezos_signer_backends.Unencrypted) ;
      Client_keys.register_signer
        (module Tezos_signer_backends.Encrypted.Make(struct
             let cctxt = (client_config :> Client_context.prompter)
           end)) ;
      let module Remote_params = struct
        let authenticate pkhs payload =
          Client_keys.list_keys client_config >>=? fun keys ->
          match List.filter_map
                  (function
                    | (_, known_pkh, _, Some known_sk_uri)
                      when List.exists (fun pkh -> Signature.Public_key_hash.equal pkh known_pkh) pkhs ->
                        Some known_sk_uri
                    | _ -> None)
                  keys with
          | sk_uri :: _ ->
              Client_keys.sign client_config sk_uri payload
          | [] -> failwith
                    "remote signer expects authentication signature, \
                     but no authorized key was found in the wallet"
        let logger = rpc_config.logger
      end in
      let module Https = Tezos_signer_backends.Https.Make(Remote_params) in
      let module Http = Tezos_signer_backends.Http.Make(Remote_params) in
      let module Socket = Tezos_signer_backends.Socket.Make(Remote_params) in
      Client_keys.register_signer (module Https) ;
      Client_keys.register_signer (module Http) ;
      Client_keys.register_signer (module Socket.Unix) ;
      Client_keys.register_signer (module Socket.Tcp) ;
      Option.iter parsed_config_file.remote_signer ~f: begin fun signer ->
        Client_keys.register_signer
          (module Tezos_signer_backends.Remote.Make(struct
               let default = signer
               include Remote_params
             end))
      end ;
      Client_keys.register_signer (module Tezos_signer_backends.Ledger) ;
      select_commands ctxt parsed_args >>=? fun commands ->
      let commands =
        Clic.add_manual
          ~executable_name
          ~global_options
          (if Unix.isatty Unix.stdout then Clic.Ansi else Clic.Plain)
          Format.std_formatter
          (config_commands @ builtin_commands @ commands) in
      begin match autocomplete with
        | Some (prev_arg, cur_arg, script) ->
            Clic.autocompletion
              ~script ~cur_arg ~prev_arg ~args:original_args ~global_options
              commands client_config >>=? fun completions ->
            List.iter print_endline completions ;
            return_unit
        | None ->
            Clic.dispatch commands client_config remaining
      end
    end >>= function
    | Ok () ->
        Lwt.return 0
    | Error [ Clic.Help command ] ->
        Clic.usage
          Format.std_formatter
          ~executable_name
          ~global_options
          (match command with None -> [] | Some c -> [ c ]) ;
        Lwt.return 0
    | Error errs ->
        Clic.pp_cli_errors
          Format.err_formatter
          ~executable_name
          ~global_options
          ~default:Error_monad.pp
          errs ;
        Lwt.return 1
  end begin function
    | Client_commands.Version_not_found ->
        Format.eprintf "@{<error>@{<title>Fatal error@}@} unknown protocol version.@." ;
        Lwt.return 1
    | Failure message ->
        Format.eprintf "@{<error>@{<title>Fatal error@}@}@.\
                       \  @[<h 0>%a@]@."
          Format.pp_print_text message ;
        Lwt.return 1
    | exn ->
        Format.printf "@{<error>@{<title>Fatal error@}@}@.\
                      \  @[<h 0>%a@]@."
          Format.pp_print_text (Printexc.to_string exn) ;
        Lwt.return 1
  end >>= fun retcode ->
  Format.pp_print_flush Format.err_formatter () ;
  Format.pp_print_flush Format.std_formatter () ;
  Logging_unix.close () >>= fun () ->
  Lwt.return retcode

(* Where all the user friendliness starts *)
let run select_commands =
  Pervasives.exit (Lwt_main.run (main select_commands))
