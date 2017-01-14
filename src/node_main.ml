(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module V6 = Ipaddr.V6

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
    net_addr : V6.t ;
    net_port : int ;
    (* local_discovery : (string * int) option ; *)
    peers : (V6.t * int) list ;
    peers_cache : string ;
    closed : bool ;

    (* rpc *)
    rpc_addr : (V6.t * int) option ;
    cors_origins : string list ;
    cors_headers : string list ;
    rpc_crt : string option ;
    rpc_key : string option ;

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
  net_addr = V6.unspecified ;
  net_port = 9732 ;
  (* local_discovery = None ; *)
  peers = [] ;
  closed = false ;
  peers_cache = base_dir // "peers_cache" ;

  (* rpc *)
  rpc_addr = None ;
  cors_origins = [] ;
  cors_headers = ["content-type"] ;
  rpc_crt = None ;
  rpc_key = None ;

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
      | V4 ipv4, port -> `Ok (Ipaddr.v6_of_v4 ipv4, port)
      | V6 ipv6, port -> `Ok (ipv6, port)

let sockaddr_of_string_exn str =
  match sockaddr_of_string str with
  | `Ok saddr -> saddr
  | `Error msg -> invalid_arg msg

let pp_sockaddr fmt (ip, port) = Format.fprintf fmt "%a:%d" V6.pp_hum ip port
let string_of_sockaddr saddr = Format.asprintf "%a" pp_sockaddr saddr

let mcast_params_of_string s = match Utils.split ':' s with
  | [iface; port] -> iface, int_of_string port
  | _ -> invalid_arg "mcast_params_of_string"

module Cfg_file = struct
  open Data_encoding

  let db =
    obj3
      (opt "store" string)
      (opt "context" string)
      (opt "protocol" string)

  let net =
    obj7
      (opt "min-connections" uint16)
      (opt "max-connections" uint16)
      (opt "expected-connections" uint16)
      (opt "addr" string)
      (* (opt "local-discovery" string) *)
      (opt "peers" (list string))
      (dft "closed" bool false)
      (opt "peers-cache" string)

  let rpc =
    obj3
      (opt "addr" string)
      (dft "cors-origin" (list string) [])
      (dft "cors-header" (list string) [])

  let log =
    obj1
      (opt "output" string)

  let t =
    conv
      (fun { store ; context ; protocol ;
             min_connections ; max_connections ; expected_connections ;
             net_addr ; net_port ;
             (* local_discovery ; *)
             peers ;
             closed ; peers_cache ; rpc_addr ; cors_origins ; cors_headers ; log_output } ->
        let net_addr = string_of_sockaddr (net_addr, net_port) in
        (* let local_discovery = Utils.map_option local_discovery *)
            (* ~f:(fun (iface, port) -> iface ^ ":" ^ string_of_int port) *)
        (* in *)
        let rpc_addr = Utils.map_option string_of_sockaddr rpc_addr in
        let peers = ListLabels.map peers ~f:string_of_sockaddr in
        let log_output = string_of_log log_output in
        ((Some store, Some context, Some protocol),
         (Some min_connections, Some max_connections, Some expected_connections,
          Some net_addr,
          (* local_discovery, *)
          Some peers, closed, Some peers_cache),
         (rpc_addr, cors_origins, cors_headers),
         Some log_output))
      (fun (
         (store, context, protocol),
         (min_connections, max_connections, expected_connections, net_addr,
          (* local_discovery, *)
          peers, closed, peers_cache),
         (rpc_addr, cors_origins, cors_headers),
         log_output) ->
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
         (* let local_discovery = map_option local_discovery ~f:mcast_params_of_string in *)
         { default_cfg with
           store ; context ; protocol ;
           min_connections ; max_connections ; expected_connections ;
           net_addr ; net_port ;
           (* local_discovery ; *)
           peers ; closed ; peers_cache ;
           rpc_addr ; cors_origins ; cors_headers ; log_output ;
         }
      )
      (obj4
         (req "db" db)
         (req "net" net)
         (req "rpc" rpc)
         (req "log" log))

  let read fp =
    Data_encoding_ezjsonm.read_file fp >|= function
    | None -> None
    | Some json -> Some (Data_encoding.Json.destruct t json)

  let from_json json = Data_encoding.Json.destruct t json
  let write out cfg =
    Utils.write_file ~bin:false out
      (Data_encoding.Json.construct t cfg |>
       Data_encoding_ezjsonm.to_string)
end

module Cmdline = struct
  open Cmdliner

  (* custom converters *)
  let sockaddr_converter = sockaddr_of_string, pp_sockaddr

  (* cli args *)
  let misc_sect = "MISC"
  let base_dir =
    let doc = "The directory where the Tezos node will store all its data." in
    Arg.(value & opt (some string) None & info ~docs:"CONFIG" ~doc ~docv:"DIR" ["base-dir"])
  let config_file =
    let doc = "The main configuration file." in
    Arg.(value & opt (some string) None & info ~docs:"CONFIG" ~doc ~docv:"FILE" ["config-file"])
  let sandbox =
    let doc = "Run the daemon in a sandbox (P2P is disabled, data is stored in a custom directory)." in
    Arg.(value & opt (some string) None & info ~docs:"NETWORK" ~doc ~docv:"DIR" ["sandbox"])
  let sandbox_param =
    let doc = "Custom parameter for the economical protocol." in
    Arg.(value & opt (some string) None & info ~docs:"NETWORK" ~doc ["sandbox-param"])
  let v =
    let doc = "Increase log level. Use several times to increase log level, e.g. `-vv'." in
    Arg.(value & flag_all & info ~docs:misc_sect ~doc ["v"])
  (* net args *)
  let min_connections =
    let doc = "The number of connections below which aggressive peer discovery mode is entered." in
    Arg.(value & opt (some int) None & info ~docs:"NETWORK" ~doc ~docv:"NUM" ["min-connections"])
  let max_connections =
    let doc = "The number of connections above which some connections will be closed." in
    Arg.(value & opt (some int) None & info ~docs:"NETWORK" ~doc ~docv:"NUM" ["max-connections"])
  let expected_connections =
    let doc = "The minimum number of connections to be ensured by the cruise control." in
    Arg.(value & opt (some int) None & info ~docs:"NETWORK" ~doc ~docv:"NUM" ["expected-connections"])
  let net_addr =
    let doc = "The TCP address and port at which this instance can be reached." in
    Arg.(value & opt (some sockaddr_converter) None & info ~docs:"NETWORK" ~doc ~docv:"ADDR:PORT" ["net-addr"])
  (* let local_discovery = *)
    (* let doc = "Automatic discovery of peers on the local network." in *)
    (* Arg.(value & opt (some @@ pair string int) None & info ~docs:"NETWORK" ~doc ~docv:"IFACE:PORT" ["local-discovery"]) *)
  let peers =
    let doc = "A peer to bootstrap the network from. Can be used several times to add several peers." in
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
    let doc = "The TCP socket address at which this RPC server instance can be reached." in
    Arg.(value & opt (some sockaddr_converter) None & info ~docs:"RPC" ~doc ~docv:"ADDR:PORT" ["rpc-addr"])
  let rpc_tls =
    let doc = "Enable TLS for this RPC server with the provided certificate and key." in
    Arg.(value & opt (some (pair string string)) None & info ~docs:"RPC" ~doc ~docv:"crt,key" ["rpc-tls"])
  let cors_origins =
    let doc = "CORS origin allowed by the RPC server via Access-Control-Allow-Origin; may be used multiple times" in
    Arg.(value & opt_all string [] & info ~docs:"RPC" ~doc ~docv:"ORIGIN" ["cors-origin"])
  let cors_headers =
    let doc = "Header reported by Access-Control-Allow-Headers reported during CORS preflighting; may be used multiple times" in
    Arg.(value & opt_all string [] & info ~docs:"RPC" ~doc ~docv:"HEADER" ["cors-header"])

  let parse base_dir config_file sandbox sandbox_param log_level
      min_connections max_connections expected_connections
      net_saddr
      (* local_discovery *)
      peers closed rpc_addr tls cors_origins cors_headers reset_cfg update_cfg =

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
      match Utils.read_file ~bin:false config_file |> Data_encoding_ezjsonm.from_string with
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
    let rpc_crt, rpc_key = match tls with
      | None -> None, None
      | Some (crt, key) -> Some crt, Some key
    in
    let cfg =
      { cfg with
        base_dir ;
        sandbox = Utils.first_some sandbox cfg.sandbox ;
        sandbox_param = Utils.first_some sandbox_param cfg.sandbox_param ;
        log_level = Utils.first_some log_level cfg.log_level ;
        min_connections = Utils.unopt cfg.min_connections min_connections ;
        max_connections = Utils.unopt cfg.max_connections max_connections ;
        expected_connections = Utils.unopt cfg.expected_connections expected_connections ;
        net_addr = (match net_saddr with None -> cfg.net_addr | Some (addr, _) -> addr) ;
        net_port = (match net_saddr with None -> cfg.net_port | Some (_, port) -> port) ;
        (* local_discovery = Utils.first_some local_discovery cfg.local_discovery ; *)
        peers = (match peers with [] -> cfg.peers | _ -> peers) ;
        closed = closed || cfg.closed ;
        rpc_addr = Utils.first_some rpc_addr cfg.rpc_addr ;
        cors_origins = (match cors_origins with [] -> cfg.cors_origins | _ -> cors_origins) ;
        cors_headers = (match cors_headers with [] -> cfg.cors_headers | _ -> cors_headers) ;
        rpc_crt ;
        rpc_key ;
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
                     $ net_addr
                     (* $ local_discovery *)
                     $ peers $ closed
                     $ rpc_addr $ rpc_tls $ cors_origins $ cors_headers
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

let init_node
    { sandbox ; sandbox_param ;
      store ; context ;
      min_connections ; max_connections ; expected_connections ;
      net_port ; peers ; peers_cache ; closed } =
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
            Data_encoding_ezjsonm.read_file file >>= function
            | None ->
                lwt_warn
                  "Can't parse sandbox parameters. (%s)" file >>= fun () ->
                Lwt.return (Some (patch_context None))
            | Some _ as json ->
                Lwt.return (Some (patch_context json))
  end >>= fun patch_context ->
  let net_params =
    let open P2p in
    match sandbox with
    | Some _ -> None
    | None ->
        (* TODO add parameters... *)
        let authentification_timeout = 5.
        and backlog = 20
        and max_incoming_connections = 20
        and max_download_speed = None
        and max_upload_speed = None
        and read_buffer_size = 1 lsl 14
        and read_queue_size = None
        and write_queue_size = None
        and incoming_app_message_queue_size = None
        and incoming_message_queue_size = None
        and outgoing_message_queue_size = None in
        let limits =
          { authentification_timeout ;
            min_connections ;
            expected_connections ;
            max_connections ;
            backlog ;
            max_incoming_connections ;
            max_download_speed ;
            max_upload_speed ;
            read_buffer_size ;
            read_queue_size ;
            write_queue_size ;
            incoming_app_message_queue_size ;
            incoming_message_queue_size ;
            outgoing_message_queue_size ;
          }
        in
        (* TODO add parameters... *)
        let identity = P2p.Identity.generate Crypto_box.default_target
        and listening_addr = None
        and proof_of_work_target = Crypto_box.default_target in
        let config =
          { listening_port = Some net_port ;
            listening_addr ;
            identity ;
            trusted_points = peers ;
            peers_file = peers_cache ;
            closed_network = closed ;
            proof_of_work_target ;
          }
        in
        Some (config, limits) in
  Node.create
    ~genesis
    ~store_root:store
    ~context_root:context
    ?test_protocol
    ?patch_context
    net_params

let init_rpc { rpc_addr ; rpc_crt; rpc_key ; cors_origins ; cors_headers } node =
  match rpc_addr, rpc_crt, rpc_key with
  | Some (addr, port), Some crt, Some key ->
      lwt_log_notice "Starting the RPC server listening on port %d (TLS enabled)." port >>= fun () ->
      let dir = Node_rpc.build_rpc_directory node in
      let mode = `TLS (`Crt_file_path crt, `Key_file_path key, `No_password, `Port port) in
      let host = Ipaddr.V6.to_string addr in
      let () =
        let old_hook = !Lwt.async_exception_hook in
        Lwt.async_exception_hook := function
          | Ssl.Read_error _ -> ()
          | exn -> old_hook exn in
      RPC_server.launch ~host mode dir cors_origins cors_headers >>= fun server ->
      Lwt.return (Some server)
  | Some (_addr, port), _, _ ->
      lwt_log_notice "Starting the RPC server listening on port %d (TLS disabled)." port >>= fun () ->
      let dir = Node_rpc.build_rpc_directory node in
      RPC_server.launch (`TCP (`Port port)) dir cors_origins cors_headers >>= fun server ->
      Lwt.return (Some server)
  | _ ->
      lwt_log_notice "Not listening to RPC calls." >>= fun () ->
      Lwt.return None

let init_signal () =
  let handler id = try Lwt_exit.exit id with _ -> () in
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
  Lwt_exit.termination_thread >>= fun x ->
  lwt_log_notice "Shutting down the Tezos node..." >>= fun () ->
  Node.shutdown node >>= fun () ->
  lwt_log_notice "Shutting down the RPC server..." >>= fun () ->
  Lwt_utils.may RPC_server.shutdown rpc >>= fun () ->
  lwt_log_notice "BYE (%d)" x >>= fun () ->
  return ()

let () =
  match Cmdline.parse () with
  | `Error _ -> exit 1
  | `Help -> exit 1
  | `Version -> exit 1
  | `Ok (config_file, was_reset, updated, cfg) ->
      if was_reset then log_notice "Overwriting %s with factory defaults." config_file;
      if updated then log_notice "Updated %s from command line arguments." config_file;
      Lwt_main.run begin
        if not @@ Sys.file_exists cfg.base_dir then begin
          Unix.mkdir cfg.base_dir 0o700;
          log_notice "Created base directory %s." cfg.base_dir
        end;
        log_notice "Using config file %s." config_file;
        if not @@ Sys.file_exists config_file then begin
          Cfg_file.write config_file cfg;
          log_notice "Created config file %s." config_file
        end;
        main cfg >>= function
        | Ok () -> Lwt.return_unit
        | Error err ->
            lwt_log_error "%a@." Error_monad.pp_print_error err
      end
