(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Logging.Node.Main

let genesis : State.Net.genesis = {
  time =
    Time.of_notation_exn "2016-11-01T00:00:00Z" ;
  block =
    Block_hash.of_b58check_exn
      "BLockGenesisGenesisGenesisGenesisGenesisGeneskvg68z" ;
  protocol =
    Protocol_hash.of_b58check_exn
      "ProtoGenesisGenesisGenesisGenesisGenesisGenesk612im" ;
}

let (//) = Filename.concat

let store_dir data_dir = data_dir // "store"
let context_dir data_dir = data_dir // "context"
let protocol_dir data_dir = data_dir // "protocol"
let lock_file data_dir = data_dir // "lock"

let init_logger ?verbosity (log_config : Node_config_file.log) =
  let open Logging in
  begin
    match verbosity with
    | Some level ->
        Lwt_log_core.add_rule "*" level
    | None ->
        Lwt_log_core.add_rule "*" log_config.default_level ;
        let rules =
          match Sys.getenv "TEZOS_LOG" with
          | rules -> Some rules
          | exception Not_found ->
              match Sys.getenv "LWT_LOG" with
              | rules -> Some rules
              | exception Not_found -> log_config.rules in
        Utils.iter_option Lwt_log_core.load_rules rules
  end ;
  Logging.init ~template:log_config.template log_config.output

let init_node ?sandbox (config : Node_config_file.t) =
  let patch_context json ctxt =
    let module Proto = (val Updater.get_exn genesis.protocol) in
    Lwt_utils.protect begin fun () ->
      Proto.configure_sandbox ctxt json
    end >|= function
    | Error err ->
        warn
          "@[Error while configuring ecoproto for the sandboxed mode:@ %a@]"
          pp_print_error err ;
        ctxt
    | Ok ctxt -> ctxt in
  begin
    match sandbox with
    | None -> Lwt.return_none
    | Some sandbox_param ->
        match sandbox_param with
        | None -> Lwt.return (Some (patch_context None))
        | Some file ->
            Data_encoding_ezjsonm.read_file file >>= function
            | Error err ->
                lwt_warn
                  "Can't parse sandbox parameters: %s" file >>= fun () ->
                lwt_debug "%a" pp_print_error err >>= fun () ->
                Lwt.return (Some (patch_context None))
            | Ok json ->
                Lwt.return (Some (patch_context (Some json)))
  end >>= fun patch_context ->
  begin
    match sandbox with
    | Some _ -> return None
    | None ->
        Node_identity_file.read
          (config.data_dir //
           Node_identity_file.default_name) >>=? fun identity ->
        lwt_log_notice
          "Peer's global id: %a"
          P2p.Peer_id.pp identity.peer_id >>= fun () ->
        (* TODO "WARN" when pow is below our expectation. *)
        begin
          match config.net.listen_addr with
          | None ->
              lwt_log_notice "Not listening to RPC calls." >>= fun () ->
              return (None, None)
          | Some addr ->
              Node_config_file.resolve_listening_addrs addr >>= function
              | [] ->
                  failwith "Cannot resolve RPC listening address: %S" addr
              | (addr, port) :: _ -> return (Some addr, Some port)
        end >>=? fun (listening_addr, listening_port) ->
        Node_config_file.resolve_bootstrap_addrs
          config.net.bootstrap_peers >>= fun trusted_points ->
        let p2p_config : P2p.config =
          { listening_addr ;
            listening_port ;
            trusted_points ;
            peers_file =
              (config.data_dir // "peers.json") ;
            closed_network = config.net.closed ;
            identity ;
            proof_of_work_target =
              Crypto_box.make_target config.net.expected_pow ;
          }
        in
        return (Some (p2p_config, config.net.limits))
  end >>=? fun p2p_config ->
  let node_config : Node.config = {
    genesis ;
    patch_context ;
    store_root = store_dir config.data_dir ;
    context_root = context_dir config.data_dir ;
    p2p = p2p_config ;
    test_network_max_tll = Some (48 * 3600) ; (* 2 days *)
  } in
  Node.create node_config

let () =
  let old_hook = !Lwt.async_exception_hook in
  Lwt.async_exception_hook := function
    | Ssl.Read_error _ -> ()
    | Ssl.Write_error _ -> ()
    | exn -> old_hook exn

let init_rpc (rpc_config: Node_config_file.rpc) node =
  match rpc_config.listen_addr with
  | None ->
      lwt_log_notice "Not listening to RPC calls." >>= fun () ->
      return None
  | Some addr ->
      Node_config_file.resolve_rpc_listening_addrs addr >>= function
      | [] ->
          failwith "Cannot resolve listening address: %S" addr
      | (addr, port) :: _ ->
          let host = Ipaddr.V6.to_string addr in
          let dir = Node_rpc.build_rpc_directory node in
          let mode =
            match rpc_config.tls with
            | None -> `TCP (`Port port)
            | Some { cert ; key } ->
                `TLS (`Crt_file_path cert, `Key_file_path key,
                      `No_password, `Port port) in
          lwt_log_notice
            "Starting the RPC server listening on port %d%s."
            port
            (if rpc_config.tls = None then "" else " (TLS enabled)") >>= fun () ->
          RPC_server.launch ~host mode dir
            rpc_config.cors_origins rpc_config.cors_headers >>= fun server ->
          return (Some server)

let init_signal () =
  let handler id = try Lwt_exit.exit id with _ -> () in
  ignore (Lwt_unix.on_signal Sys.sigint handler : Lwt_unix.signal_handler_id) ;
  ignore (Lwt_unix.on_signal Sys.sigterm handler : Lwt_unix.signal_handler_id)

let run ?verbosity ?sandbox (config : Node_config_file.t) =
  Lwt_utils.create_dir config.data_dir >>= fun () ->
  Lwt_utils.Lock_file.create
    ~unlink_on_exit:true (lock_file config.data_dir) >>=? fun () ->
  init_signal () ;
  init_logger ?verbosity config.log >>= fun () ->
  Updater.init (protocol_dir config.data_dir) ;
  lwt_log_notice "Starting the Tezos node..." >>= fun () ->
  init_node ?sandbox config >>=? fun node ->
  init_rpc config.rpc node >>=? fun rpc ->
  lwt_log_notice "The Tezos node is now running!" >>= fun () ->
  Lwt_exit.termination_thread >>= fun x ->
  lwt_log_notice "Shutting down the Tezos node..." >>= fun () ->
  Node.shutdown node >>= fun () ->
  lwt_log_notice "Shutting down the RPC server..." >>= fun () ->
  Lwt_utils.may RPC_server.shutdown rpc >>= fun () ->
  lwt_log_notice "BYE (%d)" x >>= fun () ->
  return ()

let process sandbox verbosity args =
  let verbosity =
    match verbosity with
    | [] -> None
    | [_] -> Some Logging.Info
    | _ -> Some Logging.Debug in
  let run =
    Node_shared_arg.read_and_patch_config_file args >>=? fun config ->
    Lwt_utils.Lock_file.is_locked
      (lock_file config.data_dir) >>=? function
    | false ->
        run ?sandbox ?verbosity config
    | true -> failwith "Data directory is locked by another process" in
  match Lwt_main.run run with
  | Ok () -> `Ok ()
  | Error err -> `Error (false, Format.asprintf "%a" pp_print_error err)

module Term = struct

  let verbosity =
    let open Cmdliner in
    let doc =
      "Increase log level. Using $(b,-v) is equivalent to \
       using $(b,TEZOS_LOG='* -> info'), and $(b,-vv) is equivalent to using \
       $(b,TEZOS_LOG='* -> debug')." in
    Arg.(value & flag_all &
         info ~docs:Node_shared_arg.Manpage.misc_section ~doc ["v"])

  let sandbox =
    let open Cmdliner in
    let doc =
      "Run the daemon in sandbox mode. P2P is disabled, and constants of \
       the economic protocol can be altered with an optional JSON file. \
       $(b,IMPORTANT): Using sandbox mode affects the node state and \
       subsequent runs of Tezos node must also use sandbox mode. \
       In order to run the node in normal mode afterwards, a full reset \
       must be performed (by removing the node's data directory)."
    in
    Arg.(value & opt ~vopt:(Some None) (some (some string)) None &
         info ~docs:Node_shared_arg.Manpage.misc_section
           ~doc ~docv:"FILE.json" ["sandbox"])

  let term =
    Cmdliner.Term.(ret (const process $ sandbox $ verbosity $
                        Node_shared_arg.Term.args))

end

module Manpage = struct

  let command_description =
    "The $(b,run) command is meant to run the Tezos node. \
     Most of its command line arguments corresponds to config file \
     entries, and will have priority over the latter if used."

  let description = [
    `S "DESCRIPTION" ;
    `P command_description ;
  ]

  let debug =
    let log_sections = String.concat " " (List.rev !Logging.sections) in
    [
      `S "DEBUG" ;
      `P ("The environment variable $(b,TEZOS_LOG) is used to fine-tune \
           what is going to be logged. The syntax is \
           $(b,TEZOS_LOG='<section> -> <level> [ ; ...]') \
           where section is one of $(i,"
          ^ log_sections ^
          ") and level is one of $(i,fatal), $(i,error), $(i,warn), \
           $(i,notice), $(i,info) or $(i,debug). \
           A $(b,*) can be used as a wildcard \
           in sections, i.e. $(b, client* -> debug). \
           The rules are matched left to right, \
           therefore the leftmost rule is the most prioritary one."
         ) ;
    ]

  let examples =
    [
      `S "EXAMPLES" ;
      `I ("$(b,Run in sandbox mode listening to RPC commands \
           at localhost port 8732)",
          "$(mname) run --sandbox --data-dir /custom/data/dir \
           --rpc-addr localhost:8732" ) ;
      `I ("$(b,Run a node that accepts network connections)",
          "$(mname) run" ) ;
    ]

  let man =
    description @
    Node_shared_arg.Manpage.args @
    examples @
    Node_shared_arg.Manpage.bugs

  let info =
    Cmdliner.Term.info
      ~doc:"Run the Tezos node"
      ~man
      "run"

end

let cmd = Term.term, Manpage.info
