(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Error_monad
open P2p_types
include Logging.Make (struct let name = "test-p2p-connection-pool" end)

type message =
  | Ping


let msg_config : message P2p_connection_pool.message_config = {
  encoding = [
    P2p_connection_pool.Encoding {
      tag = 0x10 ;
      encoding = Data_encoding.empty ;
      wrap = (function () -> Ping) ;
      unwrap = (function Ping -> Some ()) ;
      max_length = None ;
    } ;
  ] ;
  versions =  Version.[ { name = "TEST" ; major = 0 ; minor = 0 } ] ;
}

type metadata = unit

let meta_config : metadata P2p_connection_pool.meta_config = {
  encoding = Data_encoding.empty ;
  initial = () ;
  score = fun () -> 0. ;
}

let rec connect ~timeout pool point =
  lwt_log_info "Connect to %a" Point.pp point >>= fun () ->
  P2p_connection_pool.connect pool point ~timeout >>= function
  | Error [P2p_connection_pool.Connected] -> begin
      match P2p_connection_pool.Points.find_connection pool point with
      | Some conn -> return conn
      | None -> failwith "Woops..."
    end
  | Error ([ P2p_connection_pool.Connection_refused
           | P2p_connection_pool.Pending_connection
           | P2p_connection.Rejected
           | Lwt_utils.Canceled
           | Lwt_utils.Timeout
           | P2p_connection_pool.Rejected _
           ] as err) ->
      lwt_log_info "@[Connection to %a failed:@ %a@]"
        Point.pp point pp_print_error err >>= fun () ->
      Lwt_unix.sleep (0.5 +. Random.float 2.) >>= fun () ->
      connect ~timeout pool point
  | Ok _ | Error _ as res -> Lwt.return res

let connect_all ~timeout pool points =
  map_p (connect ~timeout pool) points

type error += Connect | Write | Read

let write_all conns msg  =
  iter_p
    (fun conn ->
       trace Write @@ P2p_connection_pool.write_sync conn msg)
    conns

let read_all conns =
  iter_p
    (fun conn ->
       trace Read @@ P2p_connection_pool.read conn >>=? fun Ping ->
       return ())
    conns

let rec connect_random pool total rem point n =
  Lwt_unix.sleep (0.2 +. Random.float 1.0) >>= fun () ->
  (trace Connect @@ connect ~timeout:2. pool point) >>=? fun conn ->
  (trace Write @@ P2p_connection_pool.write conn Ping) >>= fun _ ->
  (trace Read @@ P2p_connection_pool.read conn) >>=? fun Ping ->
  Lwt_unix.sleep (0.2 +. Random.float 1.0) >>= fun () ->
  P2p_connection_pool.disconnect conn >>= fun () ->
  begin
    decr rem ;
    if !rem mod total = 0 then
      lwt_log_notice "Remaining: %d." (!rem / total)
    else
      Lwt.return ()
  end >>= fun () ->
  if n > 1 then
    connect_random pool total rem point (pred n)
  else
    return ()

let connect_random_all pool points n =
  let total = List.length points in
  let rem = ref (n * total) in
  iter_p (fun point -> connect_random pool total rem point n) points

let close_all conns =
  Lwt_list.iter_p P2p_connection_pool.disconnect conns


let run_net config repeat points addr port =
  Lwt_unix.sleep (Random.float 2.0) >>= fun () ->
  let sched = P2p_io_scheduler.create ~read_buffer_size:(1 lsl 12) () in
  P2p_connection_pool.create
    config meta_config msg_config sched >>= fun pool ->
  P2p_welcome.run ~backlog:10 pool ~addr port >>= fun welcome ->
  connect_all ~timeout:2. pool points >>=? fun conns ->
  lwt_log_notice "Bootstrap OK" >>= fun () ->
  write_all conns Ping >>=? fun () ->
  lwt_log_notice "Sent all messages." >>= fun () ->
  read_all conns >>=? fun () ->
  lwt_log_notice "Read all messages." >>= fun () ->
  close_all conns >>= fun () ->
  lwt_log_notice "Begin random connections." >>= fun () ->
  connect_random_all pool points repeat >>=? fun () ->
  lwt_log_notice "Shutting down" >>= fun () ->
  P2p_welcome.shutdown welcome >>= fun () ->
  P2p_connection_pool.destroy pool >>= fun () ->
  P2p_io_scheduler.shutdown sched >>= fun () ->
  lwt_log_notice "Shutdown Ok" >>= fun () ->
  return ()

let make_net points repeat n =
  let point, points = Utils.select n points in
  let proof_of_work_target = Crypto_box.make_target 0. in
  let identity = Identity.generate proof_of_work_target in
  let nb_points = List.length points in
  let config = P2p_connection_pool.{
    identity ;
    proof_of_work_target ;
    trusted_points = points ;
    peers_file = "/dev/null" ;
    closed_network = true ;
    listening_port = Some (snd point) ;
    min_connections = nb_points ;
    max_connections = nb_points ;
    max_incoming_connections = nb_points ;
    authentification_timeout = 2. ;
    incoming_app_message_queue_size = None ;
    incoming_message_queue_size = None ;
    outgoing_message_queue_size = None ;
    known_peer_ids_history_size = 100 ;
    known_points_history_size = 100 ;
    max_known_points = None ;
    max_known_peer_ids = None ;
    } in
  Process.detach
    ~prefix:(Format.asprintf "%a " Peer_id.pp identity.peer_id)
    begin fun () ->
      run_net config repeat points (fst point) (snd point) >>= function
      | Ok () -> Lwt.return_unit
      | Error err ->
          lwt_log_error "@[<v 2>Unexpected error: %d@ %a@]"
            (List.length err)
            pp_print_error err >>= fun () ->
          exit 1
    end

let addr = ref Ipaddr.V6.localhost
let port = ref (1024 + Random.int 8192)
let clients = ref 10
let repeat_connections = ref 5

let spec = Arg.[

    "--port", Int (fun p -> port := p), " Listening port of the first peer.";

    "--addr", String (fun p -> addr := Ipaddr.V6.of_string_exn p),
    " Listening addr";

    "--clients", Set_int clients,  " Number of concurrent clients." ;

    "--repeat", Set_int repeat_connections,
    " Number of connections/disconnections." ;

    "-v", Unit (fun () -> Lwt_log_core.(add_rule "p2p.connection-pool" Info)),
    " Log up to info msgs" ;

    "-vv", Unit (fun () -> Lwt_log_core.(add_rule "p2p.connection-pool" Debug)),
    " Log up to debug msgs";

  ]

let main () =
  let open Utils in
  Logging.init Stderr >>= fun () ->
  let anon_fun num_peers = raise (Arg.Bad "No anonymous argument.") in
  let usage_msg = "Usage: %s <num_peers>.\nArguments are:" in
  Arg.parse spec anon_fun usage_msg ;
  let ports = !port -- (!port + !clients - 1) in
  let points = List.map (fun port -> !addr, port) ports in
  Lwt_list.iter_p (make_net points !repeat_connections) (0 -- (!clients - 1))

let () =
  Sys.catch_break true ;
  try Lwt_main.run @@ main ()
  with _ -> ()
