(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* TODO Use Kaputt on the client side and remove `assert` from the
        server. *)

open Error_monad
open P2p_types
include Logging.Make (struct let name = "test.p2p.connection" end)

let default_addr = Ipaddr.V6.localhost

let proof_of_work_target = Crypto_box.make_target 16.
let id1 = Identity.generate proof_of_work_target
let id2 = Identity.generate proof_of_work_target

let id0 =
  (* Luckilly, this will be an insuficient proof of work! *)
  Identity.generate (Crypto_box.make_target 0.)

let versions = Version.[{ name = "TEST" ; minor = 0 ; major = 0 }]

let rec listen ?port addr =
  let tentative_port =
    match port with
    | None -> 1024 + Random.int 8192
    | Some port -> port in
  let uaddr = Ipaddr_unix.V6.to_inet_addr addr in
  let main_socket = Lwt_unix.(socket PF_INET6 SOCK_STREAM 0) in
  Lwt_unix.(setsockopt main_socket SO_REUSEADDR true) ;
  Lwt.catch begin fun () ->
    Lwt_unix.Versioned.bind_2 main_socket
      (ADDR_INET (uaddr, tentative_port)) >>= fun () ->
    Lwt_unix.listen main_socket 1 ;
    Lwt.return (main_socket, tentative_port)
  end begin function
    | Unix.Unix_error
        ((Unix.EADDRINUSE | Unix.EADDRNOTAVAIL), _, _) when port = None ->
        listen addr
    | exn -> Lwt.fail exn
  end

let sync ch =
  Process.Channel.push ch () >>=? fun () ->
  Process.Channel.pop ch >>=? fun () ->
  return ()

let rec sync_nodes nodes =
  iter_p
    (fun { Process.channel } -> Process.Channel.pop channel)
    nodes >>=? fun () ->
  iter_p
    (fun { Process.channel } -> Process.Channel.push channel ())
    nodes >>=? fun () ->
  sync_nodes nodes

let sync_nodes nodes =
  sync_nodes nodes >>= function
  | Ok () | Error (Exn End_of_file :: _) ->
      return ()
  | Error _ as err ->
      Lwt.return err

let run_nodes client server =
  listen default_addr >>= fun (main_socket, port) ->
  Process.detach ~prefix:"server: " begin fun channel ->
    let sched = P2p_io_scheduler.create ~read_buffer_size:(1 lsl 12) () in
    server channel sched main_socket >>=? fun () ->
    P2p_io_scheduler.shutdown sched >>= fun () ->
    return ()
  end >>= fun server_node ->
  Process.detach ~prefix:"client: " begin fun channel ->
    Lwt_utils.safe_close main_socket >>= fun () ->
    let sched = P2p_io_scheduler.create ~read_buffer_size:(1 lsl 12) () in
    client channel sched default_addr port >>=? fun () ->
    P2p_io_scheduler.shutdown sched >>= fun () ->
    return ()
  end >>= fun client_node ->
  let nodes = [ server_node ; client_node ] in
  Lwt.ignore_result (sync_nodes nodes) ;
  Process.wait_all nodes

let raw_accept sched main_socket =
  Lwt_unix.accept main_socket >>= fun (fd, sockaddr) ->
  let fd = P2p_io_scheduler.register sched fd in
  let point =
    match sockaddr with
    | Lwt_unix.ADDR_UNIX _ -> assert false
    | Lwt_unix.ADDR_INET (addr, port) ->
        Ipaddr_unix.V6.of_inet_addr_exn addr, port in
  Lwt.return (fd, point)

let accept sched main_socket =
  raw_accept sched main_socket >>= fun (fd, point) ->
  P2p_connection.authenticate
    ~proof_of_work_target
    ~incoming:true fd point id1 versions

let raw_connect sched addr port =
  let fd = Lwt_unix.socket PF_INET6 SOCK_STREAM 0 in
  let uaddr =
    Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
  Lwt_unix.connect fd uaddr >>= fun () ->
  let fd = P2p_io_scheduler.register sched fd in
  Lwt.return fd

let connect sched addr port id =
  raw_connect sched addr port >>= fun fd ->
  P2p_connection.authenticate
    ~proof_of_work_target
    ~incoming:false fd (addr, port) id versions >>=? fun (info, auth_fd) ->
  _assert (not info.incoming) __LOC__ "" >>=? fun () ->
  _assert (Peer_id.compare info.peer_id id1.peer_id = 0)
    __LOC__ "" >>=? fun () ->
  return auth_fd

let is_connection_closed = function
  | Error [P2p_io_scheduler.Connection_closed] -> true
  | Ok _ -> false
  | Error err ->
      log_notice "Error: %a" pp_print_error err ;
      false

let is_decoding_error = function
  | Error [P2p_connection.Decoding_error] -> true
  | Ok _ -> false
  | Error err ->
      log_notice "Error: %a" pp_print_error err ;
      false

module Low_level = struct

  let simple_msg = MBytes.create (1 lsl 4)

  let client _ch sched addr port =
    let msg = MBytes.create (MBytes.length simple_msg) in
    raw_connect sched addr port >>= fun fd ->
    P2p_io_scheduler.read_full fd msg >>=? fun () ->
    _assert (MBytes.compare simple_msg msg = 0) __LOC__ "" >>=? fun () ->
    P2p_io_scheduler.close fd >>=? fun () ->
    return ()

  let server _ch sched socket =
    raw_accept sched socket >>= fun (fd, _point) ->
    P2p_io_scheduler.write fd simple_msg >>=? fun () ->
    P2p_io_scheduler.close fd >>=? fun _ ->
    return ()

  let run _dir = run_nodes client server

end

module Kick = struct

  let encoding = Data_encoding.bytes

  let is_rejected = function
    | Error [P2p_connection.Rejected] -> true
    | Ok _ -> false
    | Error err ->
        log_notice "Error: %a" pp_print_error err ;
        false

  let server _ch sched socket =
    accept sched socket >>=? fun (info, auth_fd) ->
    _assert (info.incoming) __LOC__ "" >>=? fun () ->
    _assert (Peer_id.compare info.peer_id id2.peer_id = 0)
      __LOC__ "" >>=? fun () ->
    P2p_connection.kick auth_fd >>= fun () ->
    return ()

  let client _ch sched addr port =
    connect sched addr port id2 >>=? fun auth_fd ->
    P2p_connection.accept auth_fd encoding >>= fun conn ->
    _assert (is_rejected conn) __LOC__ "" >>=? fun () ->
    return ()

  let run _dir = run_nodes client server

end

module Kicked = struct

  let encoding = Data_encoding.bytes

  let server _ch sched socket =
    accept sched socket >>=? fun (_info, auth_fd) ->
    P2p_connection.accept auth_fd encoding >>= fun conn ->
    _assert (Kick.is_rejected conn) __LOC__ "" >>=? fun () ->
    return ()

  let client _ch sched addr port =
    connect sched addr port id2 >>=? fun auth_fd ->
    P2p_connection.kick auth_fd >>= fun () ->
    return ()

  let run _dir = run_nodes client server

end

module Simple_message = struct

  let encoding = Data_encoding.bytes

  let simple_msg = MBytes.create (1 lsl 4)
  let simple_msg2 = MBytes.create (1 lsl 4)

  let server ch sched socket =
    accept sched socket >>=? fun (_info, auth_fd) ->
    P2p_connection.accept auth_fd encoding >>=? fun conn ->
    P2p_connection.write_sync conn simple_msg >>=? fun () ->
    P2p_connection.read conn >>=? fun (_msg_size, msg) ->
    _assert (MBytes.compare simple_msg2 msg = 0) __LOC__ "" >>=? fun () ->
    sync ch >>=? fun () ->
    P2p_connection.close conn >>= fun _stat ->
    return ()

  let client ch sched addr port =
    connect sched addr port id2 >>=? fun auth_fd ->
    P2p_connection.accept auth_fd encoding >>=? fun conn ->
    P2p_connection.write_sync conn simple_msg2 >>=? fun () ->
    P2p_connection.read conn >>=? fun (_msg_size, msg) ->
    _assert (MBytes.compare simple_msg msg = 0) __LOC__ "" >>=? fun () ->
    sync ch >>=? fun () ->
    P2p_connection.close conn >>= fun _stat ->
    return ()

  let run _dir = run_nodes client server

end

module Chunked_message = struct

  let encoding = Data_encoding.bytes

  let simple_msg = MBytes.create (1 lsl 8)
  let simple_msg2 = MBytes.create (1 lsl 8)

  let server ch sched socket =
    accept sched socket >>=? fun (_info, auth_fd) ->
    P2p_connection.accept
      ~binary_chunks_size:21 auth_fd encoding >>=? fun conn ->
    P2p_connection.write_sync conn simple_msg >>=? fun () ->
    P2p_connection.read conn >>=? fun (_msg_size, msg) ->
    _assert (MBytes.compare simple_msg2 msg = 0) __LOC__ "" >>=? fun () ->
    sync ch >>=? fun () ->
    P2p_connection.close conn >>= fun _stat ->
    return ()

  let client ch sched addr port =
    connect sched addr port id2 >>=? fun auth_fd ->
    P2p_connection.accept
      ~binary_chunks_size:21 auth_fd encoding >>=? fun conn ->
    P2p_connection.write_sync conn simple_msg2 >>=? fun () ->
    P2p_connection.read conn >>=? fun (_msg_size, msg) ->
    _assert (MBytes.compare simple_msg msg = 0) __LOC__ "" >>=? fun () ->
    sync ch >>=? fun () ->
    P2p_connection.close conn >>= fun _stat ->
    return ()

  let run _dir = run_nodes client server

end

module Close_on_read = struct

  let encoding = Data_encoding.bytes

  let simple_msg = MBytes.create (1 lsl 4)

  let server _ch sched socket =
    accept sched socket >>=? fun (_info, auth_fd) ->
    P2p_connection.accept auth_fd encoding >>=? fun conn ->
    P2p_connection.close conn >>= fun _stat ->
    return ()

  let client _ch sched addr port =
    connect sched addr port id2 >>=? fun auth_fd ->
    P2p_connection.accept auth_fd encoding >>=? fun conn ->
    P2p_connection.read conn >>= fun err ->
    _assert (is_connection_closed err) __LOC__ "" >>=? fun () ->
    P2p_connection.close conn >>= fun _stat ->
    return ()

  let run _dir = run_nodes client server

end

module Close_on_write = struct

  let encoding = Data_encoding.bytes

  let simple_msg = MBytes.create (1 lsl 4)

  let server ch sched socket =
    accept sched socket >>=? fun (_info, auth_fd) ->
    P2p_connection.accept auth_fd encoding >>=? fun conn ->
    P2p_connection.close conn >>= fun _stat ->
    sync ch >>=? fun ()->
    return ()

  let client ch sched addr port =
    connect sched addr port id2 >>=? fun auth_fd ->
    P2p_connection.accept auth_fd encoding >>=? fun conn ->
    sync ch >>=? fun ()->
    P2p_connection.write_sync conn simple_msg >>= fun err ->
    _assert (is_connection_closed err) __LOC__ "" >>=? fun () ->
    P2p_connection.close conn >>= fun _stat ->
    return ()

  let run _dir = run_nodes client server

end

module Garbled_data = struct

  let encoding =
    let open Data_encoding in
    dynamic_size @@ option @@ string

  (* generate a fixed garbled_msg to avoid 'Data_encoding.Binary.Await
     _', which blocks 'make test' *)
  let garbled_msg =
    let buf = MBytes.create (1 lsl 4) in
    MBytes.set_int32 buf 0 (Int32.of_int 4);
    MBytes.set_int32 buf 4 (Int32.of_int (-1));
    MBytes.set_int32 buf 8 (Int32.of_int (-1));
    MBytes.set_int32 buf 12 (Int32.of_int (-1));
    buf

  let server _ch sched socket =
    accept sched socket >>=? fun (_info, auth_fd) ->
    P2p_connection.accept auth_fd encoding >>=? fun conn ->
    P2p_connection.raw_write_sync conn garbled_msg >>=? fun () ->
    P2p_connection.read conn >>= fun err ->
    _assert (is_connection_closed err) __LOC__ "" >>=? fun () ->
    P2p_connection.close conn >>= fun _stat ->
    return ()

  let client _ch sched addr port =
    connect sched addr port id2 >>=? fun auth_fd ->
    P2p_connection.accept auth_fd encoding >>=? fun conn ->
    P2p_connection.read conn >>= fun err ->
    _assert (is_decoding_error err) __LOC__ "" >>=? fun () ->
    P2p_connection.close conn >>= fun _stat ->
    return ()

  let run _dir = run_nodes client server

end

let spec = Arg.[

    "-v", Unit (fun () ->
        Lwt_log_core.(add_rule "test.p2p.connection" Info) ;
        Lwt_log_core.(add_rule "p2p.connection" Info)),
    " Log up to info msgs" ;

    "-vv", Unit (fun () ->
        Lwt_log_core.(add_rule "test.p2p.connection" Debug) ;
        Lwt_log_core.(add_rule "p2p.connection" Debug)),
    " Log up to debug msgs";

  ]

let main () =
  let open Utils in
  let anon_fun _num_peers = raise (Arg.Bad "No anonymous argument.") in
  let usage_msg = "Usage: %s.\nArguments are:" in
  Arg.parse spec anon_fun usage_msg ;
  Test.run "p2p-connection." [
    "low-level", Low_level.run ;
    "kick", Kick.run ;
    "kicked", Kicked.run ;
    "simple-message", Simple_message.run ;
    "chunked-message", Chunked_message.run ;
    "close-on-read", Close_on_read.run ;
    "close-on-write", Close_on_write.run ;
    "garbled-data", Garbled_data.run ;
  ]

let () =
  Sys.catch_break true ;
  try main ()
  with _ -> ()
