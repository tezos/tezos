(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module V6 = Ipaddr.V6

open Hash
open Error_monad
open Logging.Node.Main

let (//) = Filename.concat

let home =
  try Sys.getenv "HOME"
  with Not_found -> "/root"

let default_base_dir = home // ".tezos-node"

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

module Sockaddr = struct

  type t = V6.t * int

  let of_string str =
    match String.rindex str ':' with
    | exception Not_found -> `Error "not a sockaddr"
    | pos ->
        let len = String.length str in
        let addr, port =
          String.sub str 0 pos, String.sub str (pos+1) (len - pos - 1) in
        match Ipaddr.of_string_exn addr, int_of_string port with
        | exception Failure _ -> `Error "not a sockaddr"
        | V4 ipv4, port -> `Ok (Ipaddr.v6_of_v4 ipv4, port)
        | V6 ipv6, port -> `Ok (ipv6, port)

  let of_string_exn str =
    match of_string str with
    | `Ok saddr -> saddr
    | `Error msg -> invalid_arg msg

  let pp fmt (ip, port) = Format.fprintf fmt "%a:%d" V6.pp_hum ip port
  let to_string saddr = Format.asprintf "%a" pp saddr

  let encoding =
    Data_encoding.conv to_string of_string_exn Data_encoding.string

  let converter : t Cmdliner.Arg.converter = of_string, pp

end

module Cfg_file = struct

  open Data_encoding

  type t = {
    db : db ;
    net : net ;
    rpc : rpc ;
    log : log ;
  }

  and db = {
    store : string ;
    context : string ;
    protocol : string ;
  }

  and net = {
    identity : string ;
    expected_pow : float ;
    bootstrap_peers : Sockaddr.t list ;
    peers_metadata : string ;
    listen_addr : Sockaddr.t option ;
    closed : bool ;
    limits : P2p.limits ;
  }

  and rpc = {
    listen_addr : Sockaddr.t option ;
    cors_origins : string list ;
    cors_headers : string list ;
    tls : tls option ;
  }

  and tls = {
    cert : string ;
    key : string ;
  }

  and log = {
    output : Logging.kind ;
    default_level : Logging.level ;
    rules : string option ;
    template : Logging.template ;
  }

  let default_net_limits : P2p.limits = {
    authentification_timeout = 5. ;
    min_connections = 50 ;
    expected_connections = 100 ;
    max_connections = 200 ;
    backlog = 20 ;
    max_incoming_connections = 20 ;
    max_download_speed = None ;
    max_upload_speed = None ;
    read_buffer_size = 1 lsl 14 ;
    read_queue_size = None ;
    write_queue_size = None ;
    incoming_app_message_queue_size = None ;
    incoming_message_queue_size = None ;
    outgoing_message_queue_size = None ;
  }

  let default_net base_dir = {
    identity = base_dir // "identity.json" ;
    expected_pow = 24. ;
    bootstrap_peers  = [] ;
    peers_metadata = base_dir // "peers.json" ;
    listen_addr  = Some (V6.unspecified, 9732) ;
    closed  = false ;
    limits = default_net_limits ;
  }

  let default_rpc = {
    listen_addr = None ;
    cors_origins = [] ;
    cors_headers = [] ;
    tls = None ;
  }

  let default_log = {
    output = Stderr ;
    default_level = Notice ;
    rules = None ;
    template = Logging.default_template ;
  }

  let default_db base_dir = {
    store = base_dir // "store" ;
    context = base_dir // "context"  ;
    protocol = base_dir // "protocol"  ;
  }

  let default_config base_dir = {
    db = default_db base_dir ;
    net = default_net base_dir ;
    rpc = default_rpc ;
    log = default_log ;
  }

  let db =
    let default = default_db default_base_dir in
    conv
      (fun { store ; context ; protocol } ->
         (store, context, protocol))
      (fun (store, context, protocol) ->
         { store ; context ; protocol })
      (obj3
         (dft "store" string default.store)
         (dft "context" string default.context)
         (dft "protocol" string default.protocol))

  let limit : P2p.limits Data_encoding.t =
    conv
      (fun { P2p.authentification_timeout ;
             min_connections ; expected_connections ; max_connections ;
             backlog ; max_incoming_connections ;
             max_download_speed ; max_upload_speed ;
             read_buffer_size ; read_queue_size ; write_queue_size ;
             incoming_app_message_queue_size ;
             incoming_message_queue_size ; outgoing_message_queue_size } ->
        ( ( authentification_timeout, min_connections, expected_connections,
            max_connections, backlog, max_incoming_connections,
            max_download_speed, max_upload_speed) ,
          ( read_buffer_size, read_queue_size, write_queue_size,
            incoming_app_message_queue_size,
            incoming_message_queue_size, outgoing_message_queue_size )))
      (fun ( ( authentification_timeout, min_connections, expected_connections,
               max_connections, backlog, max_incoming_connections,
               max_download_speed, max_upload_speed) ,
             ( read_buffer_size, read_queue_size, write_queue_size,
               incoming_app_message_queue_size,
               incoming_message_queue_size, outgoing_message_queue_size ) ) ->
        { authentification_timeout ; min_connections ; expected_connections ;
          max_connections ; backlog ; max_incoming_connections ;
          max_download_speed ; max_upload_speed ;
          read_buffer_size ; read_queue_size ; write_queue_size ;
          incoming_app_message_queue_size ;
          incoming_message_queue_size ; outgoing_message_queue_size })
      (merge_objs
        (obj8
           (dft "authentification_timeout"
              float default_net_limits.authentification_timeout)
           (dft "min_connections" int31
              default_net_limits.min_connections)
           (dft "expected_connections" int31
              default_net_limits.expected_connections)
           (dft "max_connections" int31
              default_net_limits.max_connections)
           (dft "backlog" int31
              default_net_limits.backlog)
           (dft "max_incoming_connections" int31
              default_net_limits.max_incoming_connections)
           (opt "max_download_speed" int31)
           (opt "max_upload_speed" int31))
        (obj6
           (dft "read_buffer_size" int31
              default_net_limits.read_buffer_size)
           (opt "read_queue_size" int31)
           (opt "write_queue_size" int31)
           (opt "incoming_app_message_queue_size" int31)
           (opt "incoming_message_queue_size" int31)
           (opt "outgoing_message_queue_size" int31)))

  let net =
    let default = default_net default_base_dir in
    conv
      (fun { identity ; expected_pow ; bootstrap_peers ; peers_metadata ;
             listen_addr ; closed ; limits } ->
        ( identity, expected_pow, bootstrap_peers, peers_metadata,
          listen_addr, closed, limits ))
      (fun ( identity, expected_pow, bootstrap_peers, peers_metadata,
             listen_addr, closed, limits ) ->
        { identity ; expected_pow ; bootstrap_peers ; peers_metadata ;
          listen_addr ; closed ; limits })
      (obj7
         (dft "identity" string default.identity)
         (dft "expected-proof-or-work" float default.expected_pow)
         (dft "bootstrap_peers"
            (list Sockaddr.encoding) default.bootstrap_peers)
         (dft "peers-metadata" string default.peers_metadata)
         (opt "listen-addr" Sockaddr.encoding)
         (dft "closed" bool false)
         (dft "limits" limit default_net_limits))

  let rpc : rpc Data_encoding.t =
    conv
      (fun { cors_origins ; cors_headers ; listen_addr ; tls } ->
         let cert, key =
           match tls with
           | None -> None, None
           | Some { cert ; key } -> Some cert, Some key in
         (listen_addr, cors_origins, cors_headers, cert, key ))
      (fun (listen_addr, cors_origins, cors_headers, cert, key ) ->
         let tls =
           match cert, key with
           | None, _ | _, None -> None
           | Some cert, Some key -> Some { cert ; key } in
         { listen_addr ; cors_origins ; cors_headers ; tls })
      (obj5
         (opt "listen-addr" Sockaddr.encoding)
         (dft "cors-origin" (list string) default_rpc.cors_origins)
         (dft "cors-headers" (list string) default_rpc.cors_headers)
         (opt "crt" string)
         (opt "key" string))

  let log =
    conv
      (fun {output ; default_level ; rules ; template } ->
         (output, default_level, rules, template))
      (fun (output, default_level, rules, template) ->
         { output ; default_level ; rules ; template })
      (obj4
         (dft "output" Logging.kind_encoding default_log.output)
         (dft "level" Logging.level_encoding default_log.default_level)
         (opt "rules" string)
         (dft "template" string default_log.template))

  let encoding =
    conv
      (fun { db ; rpc ; net ; log } -> (db, rpc, net, log))
      (fun (db, rpc, net, log) -> { db ; rpc ; net ; log })
      (obj4
         (dft "db" db (default_db default_base_dir))
         (dft "rpc" rpc default_rpc)
         (req "net" net)
         (dft "log" log default_log))

  let read fp =
    Data_encoding_ezjsonm.read_file fp >>=? fun json ->
    try return (Data_encoding.Json.destruct encoding json)
    with exn -> fail (Exn exn)

  let write fp cfg =
    Data_encoding_ezjsonm.write_file fp
      (Data_encoding.Json.construct encoding cfg)

end

module Cmdline = struct

  type t = {
    sandbox : string option option ;
    verbosity : Logging.level option ;
    generate_identity : bool ;
    write_cfg : 'a 'b 'c 'd. (string * (string -> 'a, 'b, 'c, 'a) format4) option ;
  }

  open Cmdliner

  (* cli args *)
  let misc_sect = "MISC"

  let base_dir =
    let doc =
      "The directory where the Tezos node will store all its data." in
    Arg.(value & opt (some string) None &
         info ~docs:"CONFIG" ~doc ~docv:"DIR" ["base-dir"])

  let config_file =
    let doc = "The main configuration file." in
    Arg.(value & opt (some string) None &
         info ~docs:"CONFIG" ~doc ~docv:"FILE" ["config-file"])

  let sandbox =
    let doc =
      "Run the daemon in a sandbox: P2P is disabled, and constants of \
       the economical protocol might be altered by the optionnal JSON file."
    in
    Arg.(value & opt ~vopt:(Some None) (some (some string)) None &
         info ~docs:"NETWORK" ~doc ~docv:"FILE.json" ["sandbox"])

  let verbosity =
    let doc =
      "Increase log level. \
       Use several times to increase log level, e.g. `-vv'." in
    Arg.(value & flag_all & info ~docs:misc_sect ~doc ["v"])

  let reset_config =
    let doc = "Overwrite config file with factory defaults." in
    Arg.(value & flag & info ~docs:"CONFIG" ~doc ["reset-config"])

  let update_config =
    let doc = "Update config file with values from the command line." in
    Arg.(value & flag & info ~docs:"CONFIG" ~doc ["update-config"])

  let generate_identity =
    let doc =
      "Generate a new cryptographic identity for the node. \
       It also generates the associated stamp of proof-of-work. \
       See `--expected-pow` for adjusting the required amount of \
       proof-of-work" in
    Arg.(value & flag & info ~docs:"CONFIG" ~doc ["generate-identity"])

  (* net args *)
  let expected_connections =
    let doc =
      "The number of running connections that we must try to maintain
       (approximativaly)." in
    Arg.(value & opt (some int) None &
         info ~docs:"NETWORK" ~doc ~docv:"NUM" ["expected-connections"])

  let max_download_speed =
    let doc =
      "The maximum number of bytes read per second." in
    Arg.(value & opt (some int) None &
         info ~docs:"NETWORK" ~doc ~docv:"NUM" ["max-download-speed"])

  let max_upload_speed =
    let doc =
      "The maximum number of bytes sent per second." in
    Arg.(value & opt (some int) None &
         info ~docs:"NETWORK" ~doc ~docv:"NUM" ["max-upload-speed"])

  let listen_addr =
    let doc =
      "The TCP address and port at which this instance can be reached." in
    Arg.(value & opt (some Sockaddr.converter) None &
         info ~docs:"NETWORK" ~doc ~docv:"ADDR:PORT" ["net-addr"])

  let peers =
    let doc =
      "A peer to bootstrap the network from. \
       Can be used several times to add several peers." in
    Arg.(value & opt_all Sockaddr.converter [] &
         info ~docs:"NETWORK" ~doc ~docv:"ADDR:PORT" ["peer"])

  let expected_pow =
    let doc =
      "Expected level of proof-of-work for peers identity." in
    Arg.(value & opt (some float) None &
         info ~docs:"NETWORK" ~doc ~docv:"FLOAT" ["expected-pow"])

  let closed =
    let doc =
      "Only accept connections from the configured bootstrap peers." in
    Arg.(value & flag & info ~docs:"NETWORK" ~doc ["closed"])

  (* rpc args *)
  let rpc_listen_addr =
    let doc =
      "The TCP socket address at which this RPC server \
       instance can be reached." in
    Arg.(value & opt (some Sockaddr.converter) None &
         info ~docs:"RPC" ~doc ~docv:"ADDR:PORT" ["rpc-addr"])

  let rpc_tls =
    let doc =
      "Enable TLS for this RPC server \
       with the provided certificate and key." in
    Arg.(value & opt (some (pair string string)) None &
         info ~docs:"RPC" ~doc ~docv:"crt,key" ["rpc-tls"])

  let cors_origins =
    let doc =
      "CORS origin allowed by the RPC server \
       via Access-Control-Allow-Origin; may be used multiple times" in
    Arg.(value & opt_all string [] &
         info ~docs:"RPC" ~doc ~docv:"ORIGIN" ["cors-origin"])

  let cors_headers =
    let doc =
      "Header reported by Access-Control-Allow-Headers \
       reported during CORS preflighting; may be used multiple times" in
    Arg.(value & opt_all string [] &
         info ~docs:"RPC" ~doc ~docv:"HEADER" ["cors-header"])

  exception Fail of string
  let fail fmt =
    Format.kasprintf (fun msg -> Lwt.fail (Fail msg)) fmt

  let parse
      base_dir config_file
      sandbox verbosity
      expected_connections
      max_download_speed max_upload_speed
      listen_addr bootstrap_peers closed expected_pow
      rpc_listen_addr rpc_tls cors_origins cors_headers
      reset_cfg update_cfg generate_identity =

    let actual_base_dir =
      match base_dir with
      | None -> default_base_dir
      | Some dir -> dir in

    let config_file =
      match config_file with
      | None -> actual_base_dir // "config.json"
      | Some file -> file in

    (* When --base-dir is provided, we ignore the `db`, `net.identity`
       and `net.peers_metadata` of the configuration file. *)
    let db = Utils.map_option Cfg_file.default_db base_dir in
    let identity, peers_metadata =
      let default_net = Utils.map_option Cfg_file.default_net base_dir in
      Utils.map_option
        ~f:(fun net -> net.Cfg_file.identity) default_net,
      Utils.map_option
        ~f:(fun net -> net.Cfg_file.peers_metadata) default_net in

    let read () =
      if reset_cfg && update_cfg then
        fail "The options --reset-config and --update-config \
              cannot be used together"
      else if reset_cfg then
        Lwt.return
          (Cfg_file.default_config actual_base_dir, true)
      else if update_cfg && not (Sys.file_exists config_file) then
        fail "Cannot update a non-existant configuration file."
      else if not (Sys.file_exists config_file) then
        Lwt.return
          (Cfg_file.default_config actual_base_dir, true)
      else
        Cfg_file.read config_file >>= function
        | Error err ->
            fail
              "@[Corrupted configuration file, \
               fix it or use --reset-config.@ %a@]"
              pp_print_error err
        | Ok cfg -> Lwt.return (cfg, update_cfg)
      in

      let verbosity =
        match verbosity with
        | [] -> None
        | [_] -> Some Logging.Info
        | _ -> Some Logging.Debug
      in

      let rpc_tls =
        Utils.map_option
          (fun (cert, key) -> { Cfg_file.cert ; key })
          rpc_tls in

      let unopt_list ~default = function
        | [] -> default
        | l -> l in

      (* when `--expected-connections` is used,
         override all the bounds defined in the configuration file. *)
      let min_connections, expected_connections, max_connections =
        match expected_connections with
        | None -> None, None, None
        | Some x -> Some (x/2), Some x, Some (3*x/2) in

      try
        Lwt_main.run begin
          Lwt_utils.create_dir ~perm:0o700 actual_base_dir >>= fun () ->
          read () >>= fun (cfg, write_cfg) ->
          let db = Utils.unopt ~default:cfg.db db in
          let limits : P2p.limits = {
            cfg.net.limits with
            min_connections =
              Utils.unopt
                ~default:cfg.net.limits.min_connections
                min_connections ;
            expected_connections =
              Utils.unopt
                ~default:cfg.net.limits.expected_connections
                expected_connections ;
            max_connections =
              Utils.unopt
                ~default:cfg.net.limits.max_connections
                max_connections ;
            max_download_speed =
              Utils.first_some
                max_download_speed cfg.net.limits.max_download_speed ;
            max_upload_speed =
                Utils.first_some
                max_upload_speed cfg.net.limits.max_upload_speed ;
          } in
          let net : Cfg_file.net = {
            identity =
              Utils.unopt ~default:cfg.net.identity identity ;
            expected_pow =
              Utils.unopt ~default:cfg.net.expected_pow expected_pow ;
            bootstrap_peers =
              unopt_list ~default:cfg.net.bootstrap_peers bootstrap_peers ;
            peers_metadata =
              Utils.unopt ~default:cfg.net.peers_metadata peers_metadata ;
            listen_addr =
              Utils.first_some listen_addr cfg.net.listen_addr ;
            closed = cfg.net.closed || closed ;
            limits ;
          }
          and rpc : Cfg_file.rpc = {
            listen_addr =
              Utils.first_some rpc_listen_addr cfg.rpc.listen_addr ;
            cors_origins =
              unopt_list ~default:cfg.rpc.cors_origins cors_origins ;
            cors_headers =
              unopt_list ~default:cfg.rpc.cors_headers cors_headers ;
            tls =
              Utils.first_some rpc_tls cfg.rpc.tls ;
          } in
          let cfg_file = { Cfg_file.db ; net ; rpc ; log = cfg.log } in
          let write_cfg : (string * _ format6) option =
            if not write_cfg then None
            else if reset_cfg then
              Some (config_file, "Reseting configuration file '%s'.")
            else if update_cfg then
              Some (config_file, "Updating configuration file '%s'.")
            else
              Some (config_file, "Writing initial configuration file '%s'.")
          in
          let cmdline =
            { sandbox ; verbosity ; generate_identity ; write_cfg } in
          Lwt.return (`Ok (cfg_file, cmdline))
        end
      with Fail msg -> `Error (false, msg)

  let cmd =
    let open Term in
    ret (const parse $ base_dir $ config_file
                     $ sandbox $ verbosity
                     $ expected_connections
                     $ max_download_speed $ max_upload_speed
                     $ listen_addr $ peers $ closed $ expected_pow
                     $ rpc_listen_addr $ rpc_tls $ cors_origins $ cors_headers
                     $ reset_config $ update_config $ generate_identity
      ),
    let doc = "The Tezos daemon" in
    let man = [
      `S "NETWORK";
      `S "RPC";
      `S "CONFIG";
      `S misc_sect;
      `S "EXAMPLES" ;
      `P "Use `$(mname) --sandbox \
                        --base-dir /path/to/a/custom/data/dir \
                        --rpc-addr :::8732' \
          to run a single instance in sandbox mode, \
          listening to RPC commands at localhost port 8732.";
      `P "Use `$(mname)' for a node that accepts network connections.";
      `S "BUGS"; `P "Check bug reports at https://github.com/tezos/tezos/issues.";
    ]
    in
    info ~sdocs:misc_sect ~man ~doc "tezos-node"

  let parse () = Term.eval cmd

end

let init_logger ?verbosity (log_config : Cfg_file.log) =
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

type error += No_identity
type error += Existent_identity_file 

let read_identity target file =
  Lwt_unix.file_exists file >>= function
  | true ->
      Data_encoding_ezjsonm.read_file file >>=? fun json ->
      let id = Data_encoding.Json.destruct P2p.Identity.encoding json in
      Lwt_utils.unless
        (Crypto_box.check_proof_of_work
           id.public_key id.proof_of_work_stamp target)
        (fun () ->
           lwt_warn "The amount of proof-of-work stamp in the node's identity \
                     is below your own expectations.") >>= fun () ->
      return id
  | false ->
      fail No_identity

let init_node ?sandbox (config : Cfg_file.t) =
  let patch_context json ctxt =
    let module Proto = (val Updater.get_exn genesis_protocol) in
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
    let open P2p in
    match sandbox with
    | Some _ -> return None
    | None ->
        let proof_of_work_target =
          Crypto_box.make_target config.net.expected_pow in
        read_identity
          proof_of_work_target config.net.identity >>=? fun identity ->
        lwt_log_notice "Peers' id: %a" P2p.Gid.pp identity.gid >>= fun () ->
        let p2p_config : P2p.config =
          { listening_port = Utils.map_option snd config.net.listen_addr ;
            listening_addr = Utils.map_option fst config.net.listen_addr ;
            trusted_points = config.net.bootstrap_peers ;
            peers_file = config.net.peers_metadata ;
            closed_network = config.net.closed ;
            identity ;
            proof_of_work_target ;
          }
        in
        return (Some (p2p_config, config.net.limits))
  end >>=? fun p2p_config ->
  let node_config : Node.config = {
    genesis ;
    test_protocol ;
    patch_context ;
    store_root = config.db.store ;
    context_root = config.db.context ;
    p2p = p2p_config ;
  } in
  Node.create node_config

let () =
  let old_hook = !Lwt.async_exception_hook in
  Lwt.async_exception_hook := function
    | Ssl.Read_error _ -> ()
    | exn -> old_hook exn

let init_rpc (rpc_config: Cfg_file.rpc) node =
  match rpc_config.listen_addr with
  | None ->
      lwt_log_notice "Not listening to RPC calls." >>= fun () ->
      Lwt.return_none
  | Some (addr, port) ->
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
      Lwt.return (Some server)


let init_signal () =
  let handler id = try Lwt_exit.exit id with _ -> () in
  ignore (Lwt_unix.on_signal Sys.sigint handler : Lwt_unix.signal_handler_id)

module Identity = struct

  let generate (command : Cmdline.t) (config : Cfg_file.t) =
    let file = config.net.identity in
    if not command.generate_identity then
      return ()
    else if Sys.file_exists file then
      fail Existent_identity_file
    else
      let target = Crypto_box.make_target config.net.expected_pow in
      Format.eprintf "Generating a new identity... " ;
      let identity =
        P2p.Identity.generate_with_animation Format.err_formatter target in
      Data_encoding_ezjsonm.write_file file
        (Data_encoding.Json.construct P2p.Identity.encoding identity)
      >>=? fun () ->
      Format.eprintf
        "Stored the new identity (%a) into '%s'@."
        P2p.Gid.pp identity.gid file ;
      return ()

end

module Node = struct

  let may_write_config (command : Cmdline.t) (config : Cfg_file.t) =
    match command.write_cfg with
    | None -> return ()
    | Some (file, fmt) ->
        Format.eprintf "%(%s%)@." fmt file ;
        Cfg_file.write file config

  let run (command : Cmdline.t) (config : Cfg_file.t) =
    may_write_config command config >>=? fun () ->
    init_signal () ;
    init_logger ?verbosity:command.verbosity config.log >>= fun () ->
    Updater.init config.db.protocol ;
    lwt_log_notice "Starting the Tezos node..." >>= fun () ->
    init_node ?sandbox:command.sandbox config >>=? fun node ->
    init_rpc config.rpc node >>= fun rpc ->
    lwt_log_notice "The Tezos node is now running!" >>= fun () ->
    Lwt_exit.termination_thread >>= fun x ->
    lwt_log_notice "Shutting down the Tezos node..." >>= fun () ->
    Node.shutdown node >>= fun () ->
    lwt_log_notice "Shutting down the RPC server..." >>= fun () ->
    Lwt_utils.may RPC_server.shutdown rpc >>= fun () ->
    lwt_log_notice "BYE (%d)" x >>= fun () ->
    return ()

end

let main (command : Cmdline.t) (config : Cfg_file.t) =
  Random.self_init () ;
  Sodium.Random.stir () ;
  Identity.generate command config >>=? fun () ->
  Node.run command config

let () =
  match Cmdline.parse () with
  | `Error _ -> exit 1
  | `Help -> exit 1
  | `Version -> exit 1
  | `Ok (config, command) ->
      Lwt_main.run begin
        main command config >>= function
        | Ok () -> Lwt.return_unit
        | Error [No_identity] ->
            Format.eprintf
              "Cannot find the identity file '%s'!\n%a@."
              config.net.identity
              Utils.display_paragraph
              (Format.sprintf
                 "In order to proceed, Tezos needs a cryptographic identity. \
                  You may generate a new identity by running:\n\
                  \n\
                     %s --generate-identity --expected-pow %.1f\n\
                  where `%.1f` is the expected level of proof-of-work in \
                  the stamp associated to the new identity. \
                  For quick testing, you may use '--expected-pow 0'."
                 Sys.argv.(0)
                 config.net.expected_pow
                 config.net.expected_pow) ;
            exit 2
        | Error [Existent_identity_file] ->
            Format.eprintf
              "Error: Cannot implicitely overwrite an existing identity.\n\
               \n\
              \   Please remove the old identity file '%s'.@."
              config.net.identity ;
            exit 2
        | Error err ->
            lwt_log_error
              "@[<v 2>Unexpected error while initializing the node:@ %a@]@."
              pp_print_error err >>= fun () ->
            exit 1
      end
