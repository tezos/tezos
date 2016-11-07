(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad
open Logging.Node.Main

let genesis_block =
  Block_hash.of_b48check
    "qBeeesNtMrdyRDj6hSK2PxEN9R67brGSm64EFRjJSBTTqLcQCRHNR"

let genesis_protocol =
  Protocol_hash.of_b48check
    "TnrnfGHMCPAcxtMAHXdpfebbnn2XvPAxq7DHbpeJbKTkJQPgcgRGr"

let test_protocol =
  Some (Protocol_hash.of_b48check
          "JF7Fxgeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee")

let genesis_time =
  Time.of_notation_exn "2016-08-01T00:00:00Z"

let genesis = {
  Store.time = genesis_time ;
  block = genesis_block ;
  protocol = genesis_protocol ;
}

module Globals = struct

  open Config_file

  let (//) = Filename.concat

  let home =
    try Sys.getenv "HOME"
    with Not_found -> "/root"

  class string_option_cp ?group name ?short_name default help =
    object (self)
      inherit [string] option_cp
          string_wrappers ?group name ?short_name default help
    method get_spec =
      let set = function
        | ""
        | "none" -> self#set None | s -> self#set (Some s) in
      Arg.String set
  end

  let addr_wrappers = {
    to_raw = (fun v -> Raw.String (Ipaddr.to_string v));
    of_raw = function
      | Raw.String v -> Ipaddr.of_string_exn v
      | r -> raise (Wrong_type (fun outchan -> Printf.fprintf outchan
                                   "Raw.Int expected, got %a\n%!" Raw.to_channel r))}
  class addr_cp = [Ipaddr.t] cp_custom_type addr_wrappers

  (** Command line options *)

  let cli_group = new group

  let base_dir =
    new filename_cp ~group:cli_group ["base-dir"] (home // ".tezos-node")
      "The directory where the tezos node will store all its data."

  let config_file =
    new filename_cp ~group:cli_group ["config-file"] (base_dir#get // "config")
      "The main configuration file."

  let () =
    let config_file_forced = ref false in
    let update_config _old_file _new_file = config_file_forced := true in
    let update_base_dir old_dir new_dir =
      if new_dir <> old_dir then
        if not !config_file_forced then begin
          config_file#set (new_dir // "config");
          config_file_forced := false
        end
    in
    config_file#add_hook update_config;
    base_dir#add_hook update_base_dir

  let sandbox =
    new string_option_cp ~group:cli_group ["sandbox"] None
      "Run a sandboxed daemon \
      \ (P2P is disabled, \
       \ data are stored in custom directory)."

  let sandbox_param =
    new string_option_cp ~group:cli_group ["sandbox-param"] None
      "Custom paramater for the ecoproto."

  let () =
    let sandboxed _ = function
      | None -> base_dir#reset
      | Some dir -> base_dir#set dir in
    sandbox#add_hook sandboxed

  let verbose_param =
    new string_option_cp ~group:cli_group ["verbosity"] ~short_name:"v" (Some "notice")
      "Verbosity level (fatal, error, warning, notice, info, debug)"

  (** File options *)

  let file_group = new group

  let store_root =
    new filename_cp ~group:file_group ["db"; "store"]
      "DUMMY" (* See update default *) "TODO"

  let context_root =
    new filename_cp ~group:file_group ["db"; "context"]
      "DUMMY" (* See update default *) "TODO"

  let protocol_dir =
    new filename_cp ~group:file_group ["protocol"; "dir"]
      "DUMMY" (* See update default *) "TODO"

  let peers_file =
    new filename_cp ~group:file_group ["net"; "peers"]
      "DUMMY" (* See update default *)
      "A file storing information about known peers"

  (** Network options *)

  let in_both_groups cp =
    file_group # add cp ; cli_group # add cp ; cp

  let min_connections = in_both_groups @@
    new int_cp [ "net" ; "min-connections" ] 4
      "The number of connections under which aggressive peer discovery mode must be entered"

  let max_connections = in_both_groups @@
    new int_cp [ "net" ; "max-connections" ] 400
      "The number of connections over which some have to be closed"

  let expected_connections = in_both_groups @@
    new int_cp [ "net" ; "expected-connections" ] 20
      "The minimum number of connections to be ensured by the cruise control"

  let incoming_port = in_both_groups @@
    new option_cp int_wrappers [ "net" ; "port" ] ~short_name:"P" (Some 9732)
      "The TCP address at which this instance can be reached"

  let discovery_port = in_both_groups @@
    new bool_cp [ "net" ; "local-discovery" ] ~short_name:"D" false
      "Automatic discovery of peers on the local network"

  let bootstrap_peers = in_both_groups @@
    new list_cp (tuple2_wrappers addr_wrappers int_wrappers)
      [ "net" ; "bootstrap-peers" ] ~short_name:"B" [ ]
      "The peers to bootstrap the networks from"

  let closed_network = in_both_groups @@
    new bool_cp
      [ "net" ; "closed" ] ~short_name:"X" false
      "Only accept connections from the bootstrap peers"


  (** Logging *)

  let log_kind =
    new string_cp ~group:file_group [ "log" ; "kind" ] "stderr"
      "Which logger to use: 'stderr', 'stdout', 'file', 'null' or 'syslog'."

  let log_file =
    new filename_cp ~group:file_group ["log"; "file"]
      "DUMMY" (* See update default *)
      "The log-file path when 'log_kind = file'."

  (** RPC *)

  let rpc_listening_port = in_both_groups @@
    new option_cp int_wrappers [ "rpc" ; "port" ] ~short_name:"P" None
      "The TCP port at which this RPC-server instance can be reached"

  let rpc_listening_addr = in_both_groups @@
    new string_option_cp [ "rpc" ; "addr" ] ~short_name:"A" None
      "The TCP address at which this RPC-server instance can be reached"

  (** Entry point *)

  let update_defaults () =
    (* Set default path relatively to [base_dir]. *)
    store_root#set (base_dir#get // "store");
    context_root#set (base_dir#get // "context");
    protocol_dir#set (base_dir#get // "protocol");
    peers_file#set (base_dir#get // "peers-cache");
    log_file#set (base_dir#get // "tezos-node.log")

  let parse_args () =
    let args = cli_group#command_line_args "-" in
    let anon_fun str =
      Arg.usage args
        (Printf.sprintf
           "\nError: Unknown command line argument %S.\n\nUsage:" str);
      Utils.exit 1
    in
    Arg.parse args anon_fun "Usage:";
    update_defaults ();
    if Sys.file_exists config_file#get then begin
      try
        file_group#read config_file#get ;
        (* parse once again to overwrite file options by cli ones *)
        Arg.parse_argv ~current:(ref 0) Sys.argv args anon_fun "Usage:"
      with Sys_error msg ->
        Printf.eprintf "Error: can't read the configuration file: %s\n%!" msg;
        Utils.exit 1
    end else begin
      try
        Lwt_main.run (Utils.create_dir (Filename.dirname config_file#get));
        file_group#write config_file#get
      with Sys_error msg ->
        Printf.eprintf
          "Warning: can't create the default configuration file: %s\n%!" msg
    end

end

let init_logger () =
  let open Logging in
  begin
    let open Lwt_log_core in
    match Globals.verbose_param#get with
    | Some "fatal" -> add_rule "*" Fatal
    | Some "error" -> add_rule "*" Error
    | Some "warning" -> add_rule "*" Warning
    | Some "notice" -> add_rule "*" Notice
    | Some "info" -> add_rule "*" Info
    | Some "debug" -> add_rule "*" Debug
    | _ -> ()
  end;
  match Globals.log_kind#get with
  | "" | "stderr" -> Logging.init Stderr
  | "stdout" -> Logging.init Stdout
  | "file" -> Logging.init (File Globals.log_file#get)
  | "null" -> Logging.init Null
  | "syslog" -> Logging.init Syslog
  | kind -> Printf.eprintf "Warning: unknown log_kind \"%s\".\n%!" kind

let init_node () =
  let patch_context json ctxt =
    let module Proto = (val Updater.get_exn genesis_protocol) in
    Lwt.catch
      (fun () ->
         Proto.configure_sandbox ctxt json >|= function
         | Error _ ->
             warn "Error while configuring ecoproto for the sandboxed mode." ;
             ctxt
         | Ok ctxt -> ctxt)
      (fun exn ->
         warn "Error while configuring ecoproto for the sandboxed mode. (%s)"
           (Printexc.to_string exn) ;
         Lwt.return ctxt) in
  begin
    match Globals.sandbox#get with
    | None -> Lwt.return_none
    | Some _ ->
        match Globals.sandbox_param#get with
        | None -> Lwt.return (Some (patch_context None))
        | Some file ->
            Data_encoding.Json.read_file file >>= function
            | None ->
                lwt_warn
                  "Can't parse sandbox parameters (%s)" file >>= fun () ->
                Lwt.return (Some (patch_context None))
            | Some _ as json ->
                Lwt.return (Some (patch_context json))
  end >>= fun patch_context ->
  let net_params =
    let open P2p in
    match Globals.sandbox#get with
    | Some _ -> None
    | None ->
        let limits =
          { max_packet_size = 10_000 ;
            peer_answer_timeout = 5. ;
            expected_connections = Globals.expected_connections#get ;
            min_connections = Globals.min_connections#get ;
            max_connections = Globals.max_connections#get ;
            blacklist_time = 30. }
        and config =
          { incoming_port = Globals.incoming_port#get ;
            discovery_port =
              if Globals.discovery_port#get then Some 7732 else None ;
            supported_versions = Node.supported_versions ;
            known_peers = Globals.bootstrap_peers#get ;
            peers_file = Globals.peers_file#get ;
            closed_network = Globals.closed_network#get }
        in
        Some (config, limits) in
  Node.create
    ~genesis
    ~store_root:Globals.store_root#get
    ~context_root:Globals.context_root#get
    ?test_protocol
    ?patch_context
    net_params

let init_rpc node =
  match Globals.rpc_listening_port#get, Globals.rpc_listening_addr#get with
  | None, None ->
      lwt_log_notice "Not listening to RPC calls." >>= fun () ->
      Lwt.return None
  | port, addr ->
      let addr = match addr with Some a -> a | None -> "127.0.0.1" in
      let port = match port with Some p -> p | None -> 8732 in
      lwt_log_notice "Starting the RPC server at %s:%d." addr port >>= fun () ->
      let dir = Node_rpc.build_rpc_directory node in
      RPC.(launch addr port dir) >>= fun server ->
      Lwt.return (Some server)

let may f = function
  | None -> Lwt.return_unit
  | Some x -> f x

let init_signal () =
  let handler id = try Utils.exit id with _ -> () in
  ignore (Lwt_unix.on_signal Sys.sigint handler : Lwt_unix.signal_handler_id)

let main () =
  Random.self_init () ;
  Sodium.Random.stir () ;
  Globals.parse_args ();
  init_logger ();
  Updater.init Globals.protocol_dir#get;
  lwt_log_notice "Starting the Tezos node..." >>= fun () ->
  init_node () >>=? fun node ->
  init_rpc node >>= fun rpc ->
  init_signal ();
  lwt_log_notice "The Tezos node is now running!" >>= fun () ->
  Utils.termination_thread >>= fun x ->
  lwt_log_notice "Shutting down the Tezos node..." >>= fun () ->
  Node.shutdown node >>= fun () ->
  lwt_log_notice "Shutting down the RPC server..." >>= fun () ->
  may RPC.shutdown rpc >>= fun () ->
  lwt_log_notice "BYE (%d)" x >>= fun () ->
  return ()

let () =
  Lwt_main.run begin
    main () >>= function
    | Ok () -> Lwt.return_unit
    | Error err ->
        lwt_log_error "%a@." Error_monad.pp_print_error err
  end
