(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let (//) = Filename.concat

let home =
  try Sys.getenv "HOME"
  with Not_found -> "/root"

let default_data_dir = home // ".tezos-node"
let default_net_port = 9732
let default_rpc_port = 8732

type t = {
  data_dir : string ;
  net : net ;
  rpc : rpc ;
  log : log ;
  shell : shell ;
}

and net = {
  expected_pow : float ;
  bootstrap_peers : string list ;
  listen_addr : string option ;
  closed : bool ;
  limits : P2p.limits ;
}

and rpc = {
  listen_addr : string option ;
  cors_origins : string list ;
  cors_headers : string list ;
  tls : tls option ;
}

and tls = {
  cert : string ;
  key : string ;
}

and log = {
  output : Logging_unix.Output.t ;
  default_level : Logging.level ;
  rules : string option ;
  template : Logging.template ;
}

and shell = {
  block_validator_limits : Node.block_validator_limits ;
  prevalidator_limits : Node.prevalidator_limits ;
  peer_validator_limits : Node.peer_validator_limits ;
  net_validator_limits : Node.net_validator_limits ;
}

let default_net_limits : P2p.limits = {
  authentification_timeout = 5. ;
  min_connections = 10 ;
  expected_connections = 50 ;
  max_connections = 100 ;
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
  known_points_history_size = 500 ;
  known_peer_ids_history_size = 500 ;
  max_known_points = Some (400, 300) ;
  max_known_peer_ids = Some (400, 300) ;
  swap_linger = 30. ;
  binary_chunks_size = None ;
}

let default_net = {
  expected_pow = 24. ;
  bootstrap_peers  = ["bootstrap.tezos.com"] ;
  listen_addr  = Some ("[::]:" ^ string_of_int default_net_port) ;
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

let default_shell = {
  block_validator_limits = {
    protocol_timeout = 120. ;
    worker_limits = {
      backlog_size = 1000 ;
      backlog_level = Logging.Debug ;
      zombie_lifetime = 3600. ;
      zombie_memory = 1800. ;
    }
  } ;
  prevalidator_limits = {
    operation_timeout = 10. ;
    max_refused_operations = 1000 ;
    worker_limits = {
      backlog_size = 1000 ;
      backlog_level = Logging.Info ;
      zombie_lifetime = 600. ;
      zombie_memory = 120. ;
    }
  } ;
  peer_validator_limits = {
    block_header_timeout = 60. ;
    block_operations_timeout = 60. ;
    protocol_timeout = 120. ;
    new_head_request_timeout = 90. ;
    worker_limits = {
      backlog_size = 1000 ;
      backlog_level = Logging.Info ;
      zombie_lifetime = 600. ;
      zombie_memory = 120. ;
    }
  } ;
  net_validator_limits = {
    bootstrap_threshold = 4 ;
    worker_limits = {
      backlog_size = 1000 ;
      backlog_level = Logging.Info ;
      zombie_lifetime = 600. ;
      zombie_memory = 120. ;
    }
  }
}

let default_config = {
  data_dir = default_data_dir ;
  net = default_net ;
  rpc = default_rpc ;
  log = default_log ;
  shell = default_shell ;
}

let limit : P2p.limits Data_encoding.t =
  let open Data_encoding in
  conv
    (fun { P2p.authentification_timeout ;
           min_connections ; expected_connections ; max_connections ;
           backlog ; max_incoming_connections ;
           max_download_speed ; max_upload_speed ;
           read_buffer_size ; read_queue_size ; write_queue_size ;
           incoming_app_message_queue_size ;
           incoming_message_queue_size ; outgoing_message_queue_size ;
           known_points_history_size ; known_peer_ids_history_size ;
           max_known_points ; max_known_peer_ids ;
           swap_linger ; binary_chunks_size
         } ->
      ( ( authentification_timeout, min_connections, expected_connections,
          max_connections, backlog, max_incoming_connections,
          max_download_speed, max_upload_speed, swap_linger,
          binary_chunks_size) ,
        ( read_buffer_size, read_queue_size, write_queue_size,
          incoming_app_message_queue_size,
          incoming_message_queue_size, outgoing_message_queue_size,
          known_points_history_size, known_peer_ids_history_size,
          max_known_points, max_known_peer_ids
        )))
    (fun ( ( authentification_timeout, min_connections, expected_connections,
             max_connections, backlog, max_incoming_connections,
             max_download_speed, max_upload_speed, swap_linger,
             binary_chunks_size) ,
           ( read_buffer_size, read_queue_size, write_queue_size,
             incoming_app_message_queue_size,
             incoming_message_queue_size, outgoing_message_queue_size,
             known_points_history_size, known_peer_ids_history_size,
             max_known_points, max_known_peer_ids
           ) ) ->
      { authentification_timeout ; min_connections ; expected_connections ;
        max_connections ; backlog ; max_incoming_connections ;
        max_download_speed ; max_upload_speed ;
        read_buffer_size ; read_queue_size ; write_queue_size ;
        incoming_app_message_queue_size ;
        incoming_message_queue_size ; outgoing_message_queue_size ;
        known_points_history_size ; known_peer_ids_history_size ;
        max_known_points ; max_known_peer_ids ; swap_linger ;
        binary_chunks_size
      })
    (merge_objs
       (obj10
          (dft "authentification-timeout"
             float default_net_limits.authentification_timeout)
          (dft "min-connections" uint16
             default_net_limits.min_connections)
          (dft "expected-connections" uint16
             default_net_limits.expected_connections)
          (dft "max-connections" uint16
             default_net_limits.max_connections)
          (dft "backlog" uint8
             default_net_limits.backlog)
          (dft "max-incoming-connections" uint8
             default_net_limits.max_incoming_connections)
          (opt "max-download-speed" int31)
          (opt "max-upload-speed" int31)
          (dft "swap-linger" float default_net_limits.swap_linger)
          (opt "binary-chunks-size" uint8))
       (obj10
          (dft "read-buffer-size" int31
             default_net_limits.read_buffer_size)
          (opt "read-queue-size" int31)
          (opt "write-queue-size" int31)
          (opt "incoming-app-message-queue-size" int31)
          (opt "incoming-message-queue-size" int31)
          (opt "outgoing-message-queue-size" int31)
          (dft "known_points_history_size" uint16
             default_net_limits.known_points_history_size)
          (dft "known_peer_ids_history_size" uint16
             default_net_limits.known_points_history_size)
          (opt "max_known_points" (tup2 uint16 uint16))
          (opt "max_known_peer_ids" (tup2 uint16 uint16))
       ))

let net =
  let open Data_encoding in
  conv
    (fun { expected_pow ; bootstrap_peers ;
           listen_addr ; closed ; limits } ->
      ( expected_pow, bootstrap_peers,
        listen_addr, closed, limits ))
    (fun ( expected_pow, bootstrap_peers,
           listen_addr, closed, limits ) ->
      { expected_pow ; bootstrap_peers ;
        listen_addr ; closed ; limits })
    (obj5
       (dft "expected-proof-of-work" float default_net.expected_pow)
       (dft "bootstrap-peers"
          (list string) default_net.bootstrap_peers)
       (opt "listen-addr" string)
       (dft "closed" bool false)
       (dft "limits" limit default_net_limits))

let rpc : rpc Data_encoding.t =
  let open Data_encoding in
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
       (opt "listen-addr" string)
       (dft "cors-origin" (list string) default_rpc.cors_origins)
       (dft "cors-headers" (list string) default_rpc.cors_headers)
       (opt "crt" string)
       (opt "key" string))

let level_encoding =
  let open Logging in
  let open Data_encoding in
  conv
    (function
      | Fatal -> "fatal"
      | Error -> "error"
      | Warning -> "warning"
      | Notice -> "notice"
      | Info -> "info"
      | Debug -> "debug")
    (function
      | "error" -> Error
      | "warn" -> Warning
      | "notice" -> Notice
      | "info" -> Info
      | "debug" -> Debug
      | "fatal" -> Fatal
      | _ -> invalid_arg "Logging.level")
    string

let log =
  let open Data_encoding in
  conv
    (fun {output ; default_level ; rules ; template } ->
       (output, default_level, rules, template))
    (fun (output, default_level, rules, template) ->
       { output ; default_level ; rules ; template })
    (obj4
       (dft "output" Logging_unix.Output.encoding default_log.output)
       (dft "level" level_encoding default_log.default_level)
       (opt "rules" string)
       (dft "template" string default_log.template))


let worker_limits_encoding
    default_size
    default_level
    default_zombie_lifetime
    default_zombie_memory =
  let open Data_encoding in
  conv
    (fun { Worker_types.backlog_size ; backlog_level ; zombie_lifetime ; zombie_memory } ->
       (backlog_size, backlog_level, zombie_lifetime, zombie_memory))
    (fun (backlog_size, backlog_level, zombie_lifetime, zombie_memory) ->
       { backlog_size ; backlog_level ; zombie_lifetime ; zombie_memory })
    (obj4
       (dft "worker_backlog_size" uint16 default_size)
       (dft "worker_backlog_level" level_encoding default_level)
       (dft "worker_zombie_lifetime" float default_zombie_lifetime)
       (dft "worker_zombie_memory" float default_zombie_memory))

let timeout_encoding =
  Data_encoding.ranged_float 0. 500.

let block_validator_limits_encoding =
  let open Data_encoding in
  conv
    (fun { Node.protocol_timeout ; worker_limits } ->
       (protocol_timeout, worker_limits))
    (fun (protocol_timeout, worker_limits) ->
       { protocol_timeout ; worker_limits})
    (merge_objs
       (obj1
          (dft "protocol_request_timeout" timeout_encoding
             default_shell.block_validator_limits.protocol_timeout))
       (worker_limits_encoding
          default_shell.block_validator_limits.worker_limits.backlog_size
          default_shell.block_validator_limits.worker_limits.backlog_level
          default_shell.block_validator_limits.worker_limits.zombie_lifetime
          default_shell.block_validator_limits.worker_limits.zombie_memory))

let prevalidator_limits_encoding =
  let open Data_encoding in
  conv
    (fun { Node.operation_timeout ; max_refused_operations ; worker_limits } ->
       ((operation_timeout, max_refused_operations), worker_limits))
    (fun ((operation_timeout, max_refused_operations), worker_limits) ->
       { operation_timeout ; max_refused_operations ; worker_limits})
    (merge_objs
       (obj2
          (dft "operations_request_timeout" timeout_encoding
             default_shell.prevalidator_limits.operation_timeout)
          (dft "max_refused_operations" uint16
             default_shell.prevalidator_limits.max_refused_operations))
       (worker_limits_encoding
          default_shell.prevalidator_limits.worker_limits.backlog_size
          default_shell.prevalidator_limits.worker_limits.backlog_level
          default_shell.prevalidator_limits.worker_limits.zombie_lifetime
          default_shell.prevalidator_limits.worker_limits.zombie_memory))

let peer_validator_limits_encoding =
  let open Data_encoding in
  let default_limits = default_shell.peer_validator_limits in
  conv
    (fun { Node.block_header_timeout ; block_operations_timeout ;
           protocol_timeout ; new_head_request_timeout ; worker_limits } ->
      ((block_header_timeout, block_operations_timeout,
        protocol_timeout, new_head_request_timeout), worker_limits))
    (fun ((block_header_timeout, block_operations_timeout,
           protocol_timeout, new_head_request_timeout), worker_limits) ->
      { block_header_timeout ; block_operations_timeout ;
        protocol_timeout ; new_head_request_timeout ; worker_limits })
    (merge_objs
       (obj4
          (dft "block_header_request_timeout" timeout_encoding default_limits.block_header_timeout)
          (dft "block_operations_request_timeout" timeout_encoding default_limits.block_operations_timeout)
          (dft "protocol_request_timeout" timeout_encoding default_limits.protocol_timeout)
          (dft "new_head_request_timeout" timeout_encoding default_limits.new_head_request_timeout))
       (worker_limits_encoding
          default_limits.worker_limits.backlog_size
          default_limits.worker_limits.backlog_level
          default_limits.worker_limits.zombie_lifetime
          default_limits.worker_limits.zombie_memory))

let net_validator_limits_encoding =
  let open Data_encoding in
  conv
    (fun { Node.bootstrap_threshold ; worker_limits } ->
       (bootstrap_threshold, worker_limits))
    (fun (bootstrap_threshold, worker_limits) ->
       { bootstrap_threshold ; worker_limits})
    (merge_objs
       (obj1
          (dft "bootstrap_threshold" uint8
             default_shell.net_validator_limits.bootstrap_threshold))
       (worker_limits_encoding
          default_shell.net_validator_limits.worker_limits.backlog_size
          default_shell.net_validator_limits.worker_limits.backlog_level
          default_shell.net_validator_limits.worker_limits.zombie_lifetime
          default_shell.net_validator_limits.worker_limits.zombie_memory))

let shell =
  let open Data_encoding in
  conv
    (fun { peer_validator_limits ; block_validator_limits ;
           prevalidator_limits ; net_validator_limits } ->
      (peer_validator_limits, block_validator_limits,
       prevalidator_limits, net_validator_limits))
    (fun (peer_validator_limits, block_validator_limits,
          prevalidator_limits, net_validator_limits) ->
      { peer_validator_limits ; block_validator_limits ;
        prevalidator_limits ; net_validator_limits })
    (obj4
       (dft "peer_validator" peer_validator_limits_encoding default_shell.peer_validator_limits)
       (dft "block_validator" block_validator_limits_encoding default_shell.block_validator_limits)
       (dft "prevalidator" prevalidator_limits_encoding default_shell.prevalidator_limits)
       (dft "net_validator" net_validator_limits_encoding default_shell.net_validator_limits)
    )

let encoding =
  let open Data_encoding in
  conv
    (fun { data_dir ; rpc ; net ; log ; shell } ->
       (data_dir, rpc, net, log, shell))
    (fun (data_dir, rpc, net, log, shell) ->
       { data_dir ; rpc ; net ; log ; shell })
    (obj5
       (dft "data-dir" string default_data_dir)
       (dft "rpc" rpc default_rpc)
       (req "net" net)
       (dft "log" log default_log)
       (dft "shell" shell default_shell))

let read fp =
  if Sys.file_exists fp then begin
    Data_encoding_ezjsonm.read_file fp >>=? fun json ->
    try return (Data_encoding.Json.destruct encoding json)
    with exn -> fail (Exn exn)
  end else
    return default_config

let write fp cfg =
  Node_data_version.ensure_data_dir (Filename.dirname fp) >>=? fun () ->
  Data_encoding_ezjsonm.write_file fp
    (Data_encoding.Json.construct encoding cfg)

let to_string cfg =
  Data_encoding.Json.to_string
    (Data_encoding.Json.construct encoding cfg)

let update
    ?data_dir
    ?min_connections
    ?expected_connections
    ?max_connections
    ?max_download_speed
    ?max_upload_speed
    ?binary_chunks_size
    ?peer_table_size
    ?expected_pow
    ?bootstrap_peers
    ?listen_addr
    ?rpc_listen_addr
    ?(closed = false)
    ?(cors_origins = [])
    ?(cors_headers = [])
    ?rpc_tls
    ?log_output
    ?bootstrap_threshold
    cfg = let data_dir = Option.unopt ~default:cfg.data_dir data_dir in
  Node_data_version.ensure_data_dir data_dir >>=? fun () ->
  let peer_table_size =
    Option.map peer_table_size ~f:(fun i -> i, i / 4 * 3) in
  let unopt_list ~default = function
    | [] -> default
    | l -> l in
  let limits : P2p.limits = {
    cfg.net.limits with
    min_connections =
      Option.unopt
        ~default:cfg.net.limits.min_connections
        min_connections ;
    expected_connections =
      Option.unopt
        ~default:cfg.net.limits.expected_connections
        expected_connections ;
    max_connections =
      Option.unopt
        ~default:cfg.net.limits.max_connections
        max_connections ;
    max_download_speed =
      Option.first_some
        max_download_speed cfg.net.limits.max_download_speed ;
    max_upload_speed =
      Option.first_some
        max_upload_speed cfg.net.limits.max_upload_speed ;
    max_known_points =
      Option.first_some
        peer_table_size cfg.net.limits.max_known_points ;
    max_known_peer_ids =
      Option.first_some
        peer_table_size cfg.net.limits.max_known_peer_ids ;
    binary_chunks_size =
      Option.map ~f:(fun x -> x lsl 10) binary_chunks_size ;
  } in
  let net : net = {
    expected_pow =
      Option.unopt ~default:cfg.net.expected_pow expected_pow ;
    bootstrap_peers =
      Option.unopt ~default:cfg.net.bootstrap_peers bootstrap_peers ;
    listen_addr =
      Option.first_some listen_addr cfg.net.listen_addr ;
    closed = cfg.net.closed || closed ;
    limits ;
  }
  and rpc : rpc = {
    listen_addr =
      Option.first_some rpc_listen_addr cfg.rpc.listen_addr ;
    cors_origins =
      unopt_list ~default:cfg.rpc.cors_origins cors_origins ;
    cors_headers =
      unopt_list ~default:cfg.rpc.cors_headers cors_headers ;
    tls =
      Option.first_some rpc_tls cfg.rpc.tls ;
  }
  and log : log = {
    cfg.log with
    output = Option.unopt ~default:cfg.log.output log_output ;
  }
  and shell : shell = {
    peer_validator_limits = cfg.shell.peer_validator_limits ;
    block_validator_limits = cfg.shell.block_validator_limits ;
    prevalidator_limits = cfg.shell.prevalidator_limits ;
    net_validator_limits =
      Option.unopt_map
        ~default:cfg.shell.net_validator_limits
        ~f:(fun bootstrap_threshold ->
            { cfg.shell.net_validator_limits
              with bootstrap_threshold })
        bootstrap_threshold
  }
  in
  return { data_dir ; net ; rpc ; log ; shell }

let resolve_addr ?default_port ?(passive = false) peer =
  let addr, port = P2p_point.Id.parse_addr_port peer in
  let node = if addr = "" || addr = "_" then "::" else addr
  and service =
    match port, default_port with
    | "", None ->
        invalid_arg ""
    | "", Some default_port -> string_of_int default_port
    | port, _ -> port in
  Lwt_utils_unix.getaddrinfo ~passive ~node ~service

let resolve_addrs ?default_port ?passive peers =
  Lwt_list.fold_left_s begin fun a peer ->
    resolve_addr ?default_port ?passive peer >>= fun points ->
    Lwt.return (List.rev_append points a)
  end [] peers

let resolve_listening_addrs listen_addr =
  resolve_addr
    ~default_port:default_net_port
    ~passive:true
    listen_addr

let resolve_rpc_listening_addrs listen_addr =
  resolve_addr
    ~default_port:default_rpc_port
    ~passive:true
    listen_addr

let resolve_bootstrap_addrs peers =
  resolve_addrs
    ~default_port:default_net_port
    peers
let check_listening_addr config =
  match config.net.listen_addr with
  | None -> Lwt.return_unit
  | Some addr ->
      Lwt.catch begin fun () ->
        resolve_listening_addrs addr >>= function
        | [] ->
            Format.eprintf "Warning: failed to resolve %S\n@." addr ;
            Lwt.return_unit
        | _ :: _ ->
            Lwt.return_unit
      end begin function
        | (Invalid_argument msg) ->
            Format.eprintf "Warning: failed to parse %S:\   %s\n@." addr msg ;
            Lwt.return_unit
        | exn -> Lwt.fail exn
      end

let check_rpc_listening_addr config =
  match config.rpc.listen_addr with
  | None -> Lwt.return_unit
  | Some addr ->
      Lwt.catch begin fun () ->
        resolve_rpc_listening_addrs addr >>= function
        | [] ->
            Format.eprintf "Warning: failed to resolve %S\n@." addr ;
            Lwt.return_unit
        | _ :: _ ->
            Lwt.return_unit
      end begin function
        | (Invalid_argument msg) ->
            Format.eprintf "Warning: failed to parse %S:\   %s\n@." addr msg ;
            Lwt.return_unit
        | exn -> Lwt.fail exn
      end

let check_bootstrap_peer addr =
  Lwt.catch begin fun () ->
    resolve_bootstrap_addrs [addr] >>= function
    | [] ->
        Format.eprintf "Warning: cannot resolve %S\n@." addr ;
        Lwt.return_unit
    | _ :: _ ->
        Lwt.return_unit
  end begin function
    | (Invalid_argument msg) ->
        Format.eprintf "Warning: failed to parse %S:\   %s\n@." addr msg ;
        Lwt.return_unit
    | exn -> Lwt.fail exn
  end


let check_bootstrap_peers config =
  Lwt_list.iter_p check_bootstrap_peer config.net.bootstrap_peers

let check config =
  check_listening_addr config >>= fun () ->
  check_rpc_listening_addr config >>= fun () ->
  check_bootstrap_peers config >>= fun () ->
  Lwt.return_unit
