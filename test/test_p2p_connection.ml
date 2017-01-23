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
include Logging.Make (struct let name = "test-p2p-connection" end)

let proof_of_work_target =
  Crypto_box.make_target [Int64.shift_left 1L 48]
let id1 = Identity.generate proof_of_work_target
let id2 = Identity.generate proof_of_work_target

let id0 =
  (* Luckilly, this will be an insuficient proof of work! *)
  Identity.generate (Crypto_box.make_target [])

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
  assert (not info.incoming) ;
  assert (Gid.compare info.gid id1.gid = 0) ;
  return auth_fd

let simple_msg =
  MBytes.create (1 lsl 1)

let is_rejected = function
  | Error [P2p_connection.Rejected] -> true
  | Ok _ -> false
  | Error err ->
      log_notice "Error: %a" pp_print_error err ;
      false

let is_connection_closed = function
  | Error [P2p_io_scheduler.Connection_closed] -> true
  | Ok _ -> false
  | Error err ->
      log_notice "Error: %a" pp_print_error err ;
      false

let bytes_encoding = Data_encoding.Variable.bytes

let server main_socket =
  let sched = P2p_io_scheduler.create ~read_buffer_size:(1 lsl 12) () in
  (* Low-level test. *)
  raw_accept sched main_socket >>= fun (fd, point) ->
  lwt_log_notice "Low_level" >>= fun () ->
  P2p_io_scheduler.write fd simple_msg >>=? fun () ->
  P2p_io_scheduler.close fd >>=? fun _ ->
  lwt_log_notice "Low_level OK" >>= fun () ->
  (* Kick the first connection. *)
  accept sched main_socket >>=? fun (info, auth_fd) ->
  lwt_log_notice "Kick" >>= fun () ->
  assert (info.incoming) ;
  assert (Gid.compare info.gid id2.gid = 0) ;
  P2p_connection.kick auth_fd >>= fun () ->
  lwt_log_notice "Kick OK" >>= fun () ->
  (* Let's be rejected. *)
  accept sched main_socket >>=? fun (info, auth_fd) ->
  P2p_connection.accept auth_fd bytes_encoding >>= fun conn ->
  assert (is_rejected conn) ;
  lwt_log_notice "Kicked OK" >>= fun () ->
  (* Accept and send a single message. *)
  accept sched main_socket >>=? fun (info, auth_fd) ->
  lwt_log_notice "Single" >>= fun () ->
  P2p_connection.accept auth_fd bytes_encoding >>=? fun conn ->
  P2p_connection.write_sync conn simple_msg >>=? fun () ->
  P2p_connection.close conn >>= fun _stat ->
  lwt_log_notice "Single OK" >>= fun () ->
  (* Accept and send a single message, while the client expected 2. *)
  accept sched main_socket >>=? fun (info, auth_fd) ->
  lwt_log_notice "Early close (read)" >>= fun () ->
  P2p_connection.accept auth_fd bytes_encoding >>=? fun conn ->
  P2p_connection.write_sync conn simple_msg >>=? fun () ->
  P2p_connection.close conn >>= fun _stat ->
  lwt_log_notice "Early close (read) OK" >>= fun () ->
  (* Accept and wait for the client to close the connection. *)
  accept sched main_socket >>=? fun (info, auth_fd) ->
  lwt_log_notice "Early close (write)" >>= fun () ->
  P2p_connection.accept auth_fd bytes_encoding >>=? fun conn ->
  P2p_connection.close conn >>= fun _stat ->
  lwt_log_notice "Early close (write) OK" >>= fun () ->
  P2p_io_scheduler.shutdown sched >>= fun () ->
  Lwt_unix.sleep 0.2 >>= fun () ->
  lwt_log_notice "Success" >>= fun () ->
  return ()

let client addr port =
  let msg = MBytes.create (MBytes.length simple_msg) in
  let sched = P2p_io_scheduler.create ~read_buffer_size:(1 lsl 12) () in
  raw_connect sched addr port >>= fun fd ->
  P2p_io_scheduler.read_full fd msg >>=? fun () ->
  assert (MBytes.compare simple_msg msg = 0) ;
  P2p_io_scheduler.close fd >>=? fun () ->
  lwt_log_notice "Low_level OK" >>= fun () ->
  (* let's be rejected. *)
  connect sched addr port id2 >>=? fun auth_fd ->
  P2p_connection.accept auth_fd bytes_encoding >>= fun conn ->
  assert (is_rejected conn) ;
  lwt_log_notice "Kick OK" >>= fun () ->
  (* let's reject! *)
  lwt_log_notice "Kicked" >>= fun () ->
  connect sched addr port id2 >>=? fun auth_fd ->
  P2p_connection.kick auth_fd >>= fun () ->
  (* let's exchange a simple message. *)
  connect sched addr port id2 >>=? fun auth_fd ->
  P2p_connection.accept auth_fd bytes_encoding >>=? fun conn ->
  P2p_connection.read conn >>=? fun msg ->
  assert (MBytes.compare simple_msg msg = 0) ;
  P2p_connection.close conn >>= fun _stat ->
  lwt_log_notice "Simple OK" >>= fun () ->
  (* let's detect a closed connection on `read`. *)
  connect sched addr port id2 >>=? fun auth_fd ->
  P2p_connection.accept auth_fd bytes_encoding >>=? fun conn ->
  P2p_connection.read conn >>=? fun msg ->
  assert (MBytes.compare simple_msg msg = 0) ;
  P2p_connection.read conn >>= fun msg ->
  assert (is_connection_closed msg) ;
  P2p_connection.close conn >>= fun _stat ->
  lwt_log_notice "Early close (read) OK" >>= fun () ->
  (* let's detect a closed connection on `write`. *)
  connect sched addr port id2 >>=? fun auth_fd ->
  P2p_connection.accept auth_fd bytes_encoding >>=? fun conn ->
  Lwt_unix.sleep 0.1 >>= fun () ->
  P2p_connection.write_sync conn simple_msg >>= fun unit ->
  assert (is_connection_closed unit) ;
  P2p_connection.close conn >>= fun _stat ->
  lwt_log_notice "Early close (write) OK" >>= fun () ->
  P2p_io_scheduler.shutdown sched >>= fun () ->
  lwt_log_notice "Success" >>= fun () ->
  return ()

let default_addr = Ipaddr.V6.localhost

let main () =
  listen default_addr >>= fun (main_socket, port) ->
  let server =
    Process.detach ~prefix:"server " begin fun () ->
      Process.handle_error begin fun () ->
        server main_socket
      end
    end in
  let client =
    Process.detach ~prefix:"client " begin fun () ->
      Lwt_utils.safe_close main_socket >>= fun () ->
      Process.handle_error begin fun () ->
        client default_addr port
      end
    end in
  Process.wait [ server ; client ]

let () =
  Lwt_main.run (main ())
