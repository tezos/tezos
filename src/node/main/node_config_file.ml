(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open P2p_types

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
  output : Logging.Output.t ;
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

let default_config = {
  data_dir = default_data_dir ;
  net = default_net ;
  rpc = default_rpc ;
  log = default_log ;
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

let log =
  let open Data_encoding in
  conv
    (fun {output ; default_level ; rules ; template } ->
       (output, default_level, rules, template))
    (fun (output, default_level, rules, template) ->
       { output ; default_level ; rules ; template })
    (obj4
       (dft "output" Logging.Output.encoding default_log.output)
       (dft "level" Logging.level_encoding default_log.default_level)
       (opt "rules" string)
       (dft "template" string default_log.template))

let encoding =
  let open Data_encoding in
  conv
    (fun { data_dir ; rpc ; net ; log } -> (data_dir, rpc, net, log))
    (fun (data_dir, rpc, net, log) -> { data_dir ; rpc ; net ; log })
    (obj4
       (dft "data-dir" string default_data_dir)
       (dft "rpc" rpc default_rpc)
       (req "net" net)
       (dft "log" log default_log))

let read fp =
  if Sys.file_exists fp then begin
    Data_encoding_ezjsonm.read_file fp >>=? fun json ->
    try return (Data_encoding.Json.destruct encoding json)
    with exn -> fail (Exn exn)
  end else
    return default_config

let write fp cfg =
  Lwt_utils.create_dir ~perm:0o700 (Filename.dirname fp) >>= fun () ->
  Data_encoding_ezjsonm.write_file fp
    (Data_encoding.Json.construct encoding cfg)

let to_string cfg =
  Data_encoding_ezjsonm.to_string
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
    cfg =
  let peer_table_size =
    map_option peer_table_size ~f:(fun i -> i, i / 4 * 3) in
  let unopt_list ~default = function
    | [] -> default
    | l -> l in
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
    max_known_points =
      Utils.first_some
        peer_table_size cfg.net.limits.max_known_points ;
    max_known_peer_ids =
      Utils.first_some
        peer_table_size cfg.net.limits.max_known_peer_ids ;
    binary_chunks_size =
      Utils.map_option (fun x -> x lsl 10) binary_chunks_size ;
  } in
  let net : net = {
    expected_pow =
      Utils.unopt ~default:cfg.net.expected_pow expected_pow ;
    bootstrap_peers =
      Utils.unopt ~default:cfg.net.bootstrap_peers bootstrap_peers ;
    listen_addr =
      Utils.first_some listen_addr cfg.net.listen_addr ;
    closed = cfg.net.closed || closed ;
    limits ;
  }
  and rpc : rpc = {
    listen_addr =
      Utils.first_some rpc_listen_addr cfg.rpc.listen_addr ;
    cors_origins =
      unopt_list ~default:cfg.rpc.cors_origins cors_origins ;
    cors_headers =
      unopt_list ~default:cfg.rpc.cors_headers cors_headers ;
    tls =
      Utils.first_some rpc_tls cfg.rpc.tls ;
  }
  and log : log = {
    cfg.log with
    output = Utils.unopt ~default:cfg.log.output log_output ;
  }
  in
  { data_dir = Utils.unopt ~default:cfg.data_dir data_dir ;
    net ; rpc ; log }

let resolve_addr ?default_port ?(passive = false) peer =
  let addr, port = Utils.parse_addr_port peer in
  let node = if addr = "" || addr = "_" then "::" else addr
  and service =
    match port, default_port with
    | "", None ->
        invalid_arg ""
    | "", Some default_port -> string_of_int default_port
    | port, _ -> port in
  Lwt_utils.getaddrinfo ~passive ~node ~service

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
