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
    "grHGHkVfgJb5gPaRd5AtQsa65g9GyLcXgQsHbSnQ5SD5DEp2ctqck"

let genesis_protocol =
  Protocol_hash.of_b48check
    "4p64VagsbXchSF88eaPy5XrkqMLEjBCaSnaGv2vQkhv8e37Nnqmrd"

let test_protocol =
  Some (Protocol_hash.of_b48check
          "2gagsSEvTKAHRjxAamgSdBNkv39VtNCqpaDXrrH4K8R4KQAAHrhe3")

let genesis_time =
  Time.of_notation_exn "2016-11-01T00:00:00Z"

let genesis = {
  Store.time = genesis_time ;
  block = genesis_block ;
  protocol = genesis_protocol ;
}

let (//) = Filename.concat

let home =
  try Sys.getenv "HOME"
  with Not_found -> "/root"

let default_base_dir = home // ".tezos-node"

type cfg = {
    (* cli *)
    base_dir : string ;
    sandbox : string option ;
    sandbox_param : string option ;

    (* db *)
    store : string ;
    context : string ;
    protocol : string ;

    (* net *)
    min_connections : int ;
    max_connections : int ;
    expected_connections : int ;
    net_addr : Ipaddr.t ;
    net_port : int ;
    local_discovery : int option ;
    peers : (Ipaddr.t * int) list ;
    peers_cache : string ;
    closed : bool ;

    (* rpc *)
    rpc_addr : (Ipaddr.t * int) option ;

    (* log *)
    log_output : [`Stderr | `File of string | `Syslog | `Null] ;
    log_level : Lwt_log.level option ;
  }

let default_cfg_of_base_dir base_dir = {
  (* cli *)
  base_dir ;
  sandbox = None ;
  sandbox_param = None ;

  (* db *)
  store = base_dir // "store" ;
  context = base_dir // "context" ;
  protocol = base_dir // "protocol" ;

  (* net *)
  min_connections = 4 ;
  max_connections = 400 ;
  expected_connections = 20 ;
  net_addr = Ipaddr.(V6 V6.unspecified) ;
  net_port = 9732 ;
  local_discovery = None ;
  peers = [] ;
  closed = false ;
  peers_cache = base_dir // "peers_cache" ;

  (* rpc *)
  rpc_addr = None ;

  (* log *)
  log_output = `Stderr ;
  log_level = None ;
}

let default_cfg = default_cfg_of_base_dir default_base_dir

let log_of_string s = match Utils.split ':' ~limit:2 s with
  | ["stderr"] -> `Stderr
  | ["file"; fn] -> `File fn
  | ["syslog"] -> `Syslog
  | ["null"] -> `Null
  | _ -> invalid_arg "log_of_string"

let string_of_log = function
  | `Stderr -> "stderr"
  | `File fn -> "file:" ^ fn
  | `Syslog -> "syslog"
  | `Null -> "null"

let sockaddr_of_string str =
  match String.rindex str ':' with
  | exception Not_found -> `Error "not a sockaddr"
  | pos ->
      let len = String.length str in
      let addr, port = String.sub str 0 pos, String.sub str (pos+1) (len - pos - 1) in
      match Ipaddr.of_string_exn addr, int_of_string port with
      | exception Failure _ -> `Error "not a sockaddr"
      | ip, port -> `Ok (ip, port)

let sockaddr_of_string_exn str =
  match sockaddr_of_string str with
  | `Ok saddr -> saddr
  | `Error msg -> invalid_arg msg

let pp_sockaddr fmt (ip, port) = Format.fprintf fmt "%a:%d" Ipaddr.pp_hum ip port
let string_of_sockaddr saddr = Format.asprintf "%a" pp_sockaddr saddr

module Cfg_file = struct
  open Data_encoding

  let db =
    obj3
      (opt "store" string)
      (opt "context" string)
      (opt "protocol" string)

  let net =
    obj8
      (opt "min-connections" uint16)
      (opt "max-connections" uint16)
      (opt "expected-connections" uint16)
      (opt "addr" string)
      (opt "local-discovery" uint16)
      (opt "peers" (list string))
      (dft "closed" bool false)
      (opt "peers-cache" string)

  let rpc =
    obj1
      (opt "addr" string)

  let log =
    obj1
      (opt "output" string)

  let t =
    conv
      (fun { store ; context ; protocol ;
             min_connections ; max_connections ; expected_connections;
             net_addr ; net_port ; local_discovery ; peers;
             closed ; peers_cache ; rpc_addr; log_output } ->
        let net_addr = string_of_sockaddr (net_addr, net_port) in
        let rpc_addr = Utils.map_option string_of_sockaddr rpc_addr in
        let peers = ListLabels.map peers ~f:string_of_sockaddr in
        let log_output = string_of_log log_output in
        ((Some store, Some context, Some protocol),
         (Some min_connections, Some max_connections, Some expected_connections,
          Some net_addr, local_discovery, Some peers, closed, Some peers_cache),
         rpc_addr, Some log_output))
      (fun (
         (store, context, protocol),
         (min_connections, max_connections, expected_connections,
          net_addr, local_discovery, peers, closed, peers_cache), rpc_addr, log_output) ->
         let open Utils in
         let store = unopt default_cfg.store store in
         let context = unopt default_cfg.context context in
         let protocol = unopt default_cfg.protocol protocol in
         let net_addr = map_option sockaddr_of_string_exn net_addr in
         let net_addr, net_port = unopt (default_cfg.net_addr, default_cfg.net_port) net_addr in
         let rpc_addr = map_option sockaddr_of_string_exn rpc_addr in
         let peers = unopt [] peers in
         let peers = ListLabels.map peers ~f:sockaddr_of_string_exn in
         let peers_cache = unopt default_cfg.peers_cache peers_cache in
         let log_output = unopt default_cfg.log_output (map_option log_of_string log_output) in
         let min_connections = unopt default_cfg.min_connections min_connections in
         let max_connections = unopt default_cfg.max_connections max_connections in
         let expected_connections = unopt default_cfg.expected_connections expected_connections in
         { default_cfg with
           store ; context ; protocol ;
           min_connections; max_connections; expected_connections;
           net_addr; net_port ; local_discovery; peers; closed; peers_cache;
           rpc_addr; log_output
         }
      )
      (obj4
         (req "db" db)
         (req "net" net)
         (req "rpc" rpc)
         (req "log" log))

  let read fp =
    let open Data_encoding.Json in
    read_file fp >|= function
    | None -> None
    | Some json -> Some (destruct t json)

  let from_json json = Data_encoding.Json.destruct t json
  let write out cfg =
    Utils.write_file ~bin:false out Data_encoding.Json.(construct t cfg |> to_string)
end

module Cmdline = struct
  open Cmdliner

  (* custom converters *)
  let sockaddr_converter = sockaddr_of_string, pp_sockaddr

  (* cli args *)
  let misc_sect = "MISC"
  let base_dir =
    let doc = "The directory where the tezos node will store all its data." in
    Arg.(value & opt (some string) None & info ~docs:"CONFIG" ~doc ~docv:"DIR" ["base-dir"])
  let config_file =
    let doc = "The main configuration file." in
    Arg.(value & opt (some string) None & info ~docs:"CONFIG" ~doc ~docv:"FILE" ["config-file"])
  let sandbox =
    let doc = "Run a sandboxed daemon  (P2P is disabled, data is stored in custom directory)." in
    Arg.(value & opt (some string) None & info ~docs:"NETWORK" ~doc ~docv:"DIR" ["sandbox"])
  let sandbox_param =
    let doc = "Custom parameter for the economical protocol." in
    Arg.(value & opt (some string) None & info ~docs:"NETWORK" ~doc ["sandbox-param"])
  let v =
    let doc = "Increase log level. Use several time to increase log level, e.g. `-vv'." in
    Arg.(value & flag_all & info ~docs:misc_sect ~doc ["v"])
  (* net args *)
  let min_connections =
    let doc = "The number of connections under which aggressive peer discovery mode must be entered." in
    Arg.(value & opt int default_cfg.min_connections & info ~docs:"NETWORK" ~doc ~docv:"NUM" ["min-connections"])
  let max_connections =
    let doc = "The number of connections over which some have to be closed." in
    Arg.(value & opt int default_cfg.max_connections & info ~docs:"NETWORK" ~doc ~docv:"NUM" ["max-connections"])
  let expected_connections =
    let doc = "The minimum number of connections to be ensured by the cruise control." in
    Arg.(value & opt int default_cfg.expected_connections & info ~docs:"NETWORK" ~doc ~docv:"NUM" ["expected-connections"])
  let net_addr =
    let doc = "The TCP socket address at which this instance can be reached." in
    Arg.(value & opt sockaddr_converter (default_cfg.net_addr, default_cfg.net_port) & info ~docs:"NETWORK" ~doc ~docv:"ADDR:PORT" ["net-addr"])
  let local_discovery =
    let doc = "Automatic discovery of peers on the local network." in
    Arg.(value & opt (some int) default_cfg.local_discovery & info ~docs:"NETWORK" ~doc ~docv:"ADDR:PORT" ["local-discovery"])
  let peers =
    let doc = "A peer to bootstrap the networks from. Can be used several times to add several peers." in
    Arg.(value & opt_all sockaddr_converter [] & info ~docs:"NETWORK" ~doc ~docv:"ADDR:PORT" ["peer"])
  let closed =
    let doc = "Only accept connections from the bootstrap peers." in
    Arg.(value & flag & info ~docs:"NETWORK" ~doc ["closed"])
  let reset_config =
    let doc = "Overwrite config file with factory defaults." in
    Arg.(value & flag & info ~docs:"CONFIG" ~doc ["reset-config"])
  let update_config =
    let doc = "Update config file with values from the command line." in
    Arg.(value & flag & info ~docs:"CONFIG" ~doc ["update-config"])

  (* rpc args *)
  let rpc_addr =
    let doc = "The TCP socket address at which this RPC-server instance can be reached" in
    Arg.(value & opt (some sockaddr_converter) None & info ~docs:"RPC" ~doc ~docv:"ADDR:PORT" ["rpc-addr"])

  let parse base_dir config_file sandbox sandbox_param log_level
      min_connections max_connections expected_connections
      (net_addr, net_port) local_discovery peers closed rpc_addr reset_cfg update_cfg =
    let base_dir = Utils.(unopt (unopt default_cfg.base_dir base_dir) sandbox) in
    let config_file = Utils.(unopt ((unopt base_dir sandbox) // "config")) config_file in
    let no_config () =
      warn "Found no config file at %s" config_file;
      warn "Using factory defaults";
      default_cfg_of_base_dir base_dir
    in
    let corrupted_config msg =
      log_error "Config file %s corrupted: %s" config_file msg;
      warn "Using factory defaults";
      default_cfg_of_base_dir base_dir
    in
    let cfg =
      match Utils.read_file ~bin:false config_file |> Data_encoding.Json.from_string with
      | exception _ -> no_config ()
      | Error msg -> corrupted_config msg
      | Ok cfg -> try Cfg_file.from_json cfg with
        | Invalid_argument msg
        | Failure msg -> corrupted_config msg
    in
    let log_level = match List.length log_level with
      | 0 -> None
      | 1 -> Some Lwt_log.Info
      | _ -> Some Lwt_log.Debug
    in
    let cfg =
      { cfg with
        base_dir ;
        sandbox ;
        sandbox_param ;
        log_level ;
        min_connections ;
        max_connections ;
        expected_connections ;
        net_addr ;
        net_port ;
        local_discovery ;
        peers = List.rev_append peers cfg.peers ;
        closed ;
        rpc_addr = Utils.first_some rpc_addr cfg.rpc_addr ;
        log_output = cfg.log_output ;
      }
    in
    if update_cfg then Cfg_file.write config_file cfg;
    `Ok (config_file, reset_cfg, update_cfg, cfg)

  let cmd =
    let open Term in
    ret (const parse $ base_dir $ config_file
                     $ sandbox $ sandbox_param $ v
                     $ min_connections $ max_connections $ expected_connections
                     $ net_addr $ local_discovery $ peers $ closed $ rpc_addr
                     $ reset_config $ update_config
      ),
    let doc = "The Tezos daemon" in
    let man = [
      `S "NETWORK";
      `S "RPC";
      `S "CONFIG";
      `S misc_sect;
      `S "EXAMPLES" ;
      `P "Use `$(mname) --sandbox /path/to/a/custom/data/dir --rpc-addr :::8732' \
          to run a single instance in sandbox mode, \
          listening to RPC commands at localhost port 8732.";
      `P "Use `$(mname)' for a node that accepts network connections.";
      `S "BUGS"; `P "Check bug reports at https://github.com/tezos/tezos/issues.";
    ]
    in
    info ~sdocs:misc_sect ~man ~doc "tezos-node"

  let parse () = Term.eval cmd
end

let init_logger { log_output ; log_level } =
  let open Logging in
  Utils.iter_option log_level ~f:(Lwt_log_core.add_rule "*") ;
  match log_output with
  | `Stderr -> Logging.init Stderr
  | `File fp -> Logging.init (File fp)
  | `Null -> Logging.init Null
  | `Syslog -> Logging.init Syslog

let init_node { sandbox ; sandbox_param ;
                store ; context ;
                min_connections ; max_connections ; expected_connections ;
                net_port ; peers ; peers_cache ; local_discovery ; closed } =
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
    match sandbox with
    | None -> Lwt.return_none
    | Some _ ->
        match sandbox_param with
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
    match sandbox with
    | Some _ -> None
    | None ->
        let limits =
          { max_packet_size = 10_000 ;
            peer_answer_timeout = 5. ;
            expected_connections ;
            min_connections ;
            max_connections ;
            blacklist_time = 30. }
        in
        let config =
          { incoming_port = Some net_port ;
            discovery_port = local_discovery ;
            known_peers = peers ;
            peers_file = peers_cache ;
            closed_network = closed }
        in
        Some (config, limits) in
  Node.create
    ~genesis
    ~store_root:store
    ~context_root:context
    ?test_protocol
    ?patch_context
    net_params

let init_rpc { rpc_addr } node =
  match rpc_addr with
  | None ->
      lwt_log_notice "Not listening to RPC calls." >>= fun () ->
      Lwt.return None
  | Some (_addr, port) ->
      lwt_log_notice "Starting the RPC server listening on port %d." port >>= fun () ->
      let dir = Node_rpc.build_rpc_directory node in
      RPC.(launch port dir) >>= fun server ->
      Lwt.return (Some server)

let init_signal () =
  let handler id = try Utils.exit id with _ -> () in
  ignore (Lwt_unix.on_signal Sys.sigint handler : Lwt_unix.signal_handler_id)

let main cfg =
  Random.self_init () ;
  Sodium.Random.stir () ;
  init_logger cfg;
  Updater.init cfg.protocol;
  lwt_log_notice "Starting the Tezos node..." >>= fun () ->
  init_node cfg >>=? fun node ->
  init_rpc cfg node >>= fun rpc ->
  init_signal ();
  lwt_log_notice "The Tezos node is now running!" >>= fun () ->
  Utils.termination_thread >>= fun x ->
  lwt_log_notice "Shutting down the Tezos node..." >>= fun () ->
  Node.shutdown node >>= fun () ->
  lwt_log_notice "Shutting down the RPC server..." >>= fun () ->
  Lwt_utils.may RPC.shutdown rpc >>= fun () ->
  lwt_log_notice "BYE (%d)" x >>= fun () ->
  return ()

let () =
  match Cmdline.parse () with
  | `Error _ -> exit 1
  | `Help -> exit 1
  | `Version -> exit 1
  | `Ok (config_file, resetted, updated, cfg) ->
      if resetted then log_notice "Overwriting %s to factory defaults." config_file;
      if updated then log_notice "Updated %s from command line arguments." config_file;
      Lwt_main.run begin
        if not @@ Sys.file_exists cfg.base_dir then begin
          Unix.mkdir cfg.base_dir 0o700;
          log_notice "Created base directory %s." cfg.base_dir
        end;
        log_notice "Using config file %s" config_file;
        if not @@ Sys.file_exists config_file then begin
          Cfg_file.write config_file cfg;
          log_notice "Created config file %s" config_file
        end;
        main cfg >>= function
        | Ok () -> Lwt.return_unit
        | Error err ->
            lwt_log_error "%a@." Error_monad.pp_print_error err
      end
