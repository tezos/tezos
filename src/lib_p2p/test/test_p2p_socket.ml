(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

include
  Internal_event.Legacy_logging.Make
    (struct let name = "test.p2p.connection" end)

let addr = ref Ipaddr.V6.localhost

let canceler = Lwt_canceler.create () (* unused *)

let proof_of_work_target = Crypto_box.make_target 16.
let id1 = P2p_identity.generate proof_of_work_target
let id2 = P2p_identity.generate proof_of_work_target

let id0 =
  (* Luckilly, this will be an insuficient proof of work! *)
  P2p_identity.generate (Crypto_box.make_target 0.)

let version =
  { Network_version.
    chain_name = Distributed_db_version.sandboxed_chain_name ;
    distributed_db_version = Distributed_db_version.zero ;
    p2p_version = P2p_version.zero ;
  }

type metadata = unit
let conn_meta_config : metadata P2p_socket.metadata_config = {
  conn_meta_encoding = Data_encoding.empty ;
  conn_meta_value = (fun _ -> ()) ;
  private_node = (fun _ -> false) ;
}

let rec listen ?port addr =
  let tentative_port =
    match port with
    | None -> 1024 + Random.int 8192
    | Some port -> port in
  let uaddr = Ipaddr_unix.V6.to_inet_addr addr in
  let main_socket = Lwt_unix.(socket PF_INET6 SOCK_STREAM 0) in
  Lwt_unix.(setsockopt main_socket SO_REUSEADDR true) ;
  Lwt.catch begin fun () ->
    Lwt_unix.bind main_socket
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
  return_unit

let rec sync_nodes nodes =
  iter_p
    (fun { Process.channel ; _ } -> Process.Channel.pop channel)
    nodes >>=? fun () ->
  iter_p
    (fun { Process.channel ; _ } -> Process.Channel.push channel ())
    nodes >>=? fun () ->
  sync_nodes nodes

let sync_nodes nodes =
  sync_nodes nodes >>= function
  | Ok () | Error (Exn End_of_file :: _) ->
      return_unit
  | Error _ as err ->
      Lwt.return err

let run_nodes client server =
  listen !addr >>= fun (main_socket, port) ->
  Process.detach ~prefix:"server: " begin fun channel ->
    let sched = P2p_io_scheduler.create ~read_buffer_size:(1 lsl 12) () in
    server channel sched main_socket >>=? fun () ->
    P2p_io_scheduler.shutdown sched >>= fun () ->
    return_unit
  end >>= fun server_node ->
  Process.detach ~prefix:"client: " begin fun channel ->
    Lwt_utils_unix.safe_close main_socket >>= fun () ->
    let sched = P2p_io_scheduler.create ~read_buffer_size:(1 lsl 12) () in
    client channel sched !addr port >>=? fun () ->
    P2p_io_scheduler.shutdown sched >>= fun () ->
    return_unit
  end >>= fun client_node ->
  let nodes = [ server_node ; client_node ] in
  Lwt.ignore_result (sync_nodes nodes) ;
  Process.wait_all nodes

let raw_accept sched main_socket =
  P2p_fd.accept main_socket >>= fun (fd, sockaddr) ->
  let fd = P2p_io_scheduler.register sched fd in
  let point =
    match sockaddr with
    | Lwt_unix.ADDR_UNIX _ -> assert false
    | Lwt_unix.ADDR_INET (addr, port) ->
        Ipaddr_unix.V6.of_inet_addr_exn addr, port in
  Lwt.return (fd, point)

let accept sched main_socket =
  raw_accept sched main_socket >>= fun (fd, point) ->
  P2p_socket.authenticate
    ~canceler
    ~proof_of_work_target
    ~incoming:true fd point id1 version
    conn_meta_config

let raw_connect sched addr port =
  let fd = P2p_fd.socket PF_INET6 SOCK_STREAM 0 in
  let uaddr =
    Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
  P2p_fd.connect fd uaddr >>= fun () ->
  let fd = P2p_io_scheduler.register sched fd in
  Lwt.return fd

let connect sched addr port id =
  raw_connect sched addr port >>= fun fd ->
  P2p_socket.authenticate
    ~canceler
    ~proof_of_work_target
    ~incoming:false fd
    (addr, port) id version
    conn_meta_config >>=? fun (info, auth_fd) ->
  _assert (not info.incoming) __LOC__ "" >>=? fun () ->
  _assert (P2p_peer.Id.compare info.peer_id id1.peer_id = 0)
    __LOC__ "" >>=? fun () ->
  return auth_fd

let is_connection_closed = function
  | Error [P2p_errors.Connection_closed] -> true
  | Ok _ -> false
  | Error err ->
      log_notice "Error: %a" pp_print_error err ;
      false

let is_decoding_error = function
  | Error [P2p_errors.Decoding_error] -> true
  | Ok _ -> false
  | Error err ->
      log_notice "Error: %a" pp_print_error err ;
      false

module Low_level = struct

  let simple_msg = Rand.generate (1 lsl 4)

  let client _ch sched addr port =
    let msg = MBytes.create (MBytes.length simple_msg) in
    raw_connect sched addr port >>= fun fd ->
    P2p_io_scheduler.read_full fd msg >>=? fun () ->
    _assert (MBytes.compare simple_msg msg = 0) __LOC__ "" >>=? fun () ->
    P2p_io_scheduler.close fd >>=? fun () ->
    return_unit

  let server _ch sched socket =
    raw_accept sched socket >>= fun (fd, _point) ->
    P2p_io_scheduler.write fd simple_msg >>=? fun () ->
    P2p_io_scheduler.close fd >>=? fun _ ->
    return_unit

  let run _dir = run_nodes client server

end

module Kick = struct

  let encoding = Data_encoding.bytes

  let is_rejected = function
    | Error [P2p_errors.Rejected_socket_connection] -> true
    | Ok _ -> false
    | Error err ->
        log_notice "Error: %a" pp_print_error err ;
        false

  let server _ch sched socket =
    accept sched socket >>=? fun (info, auth_fd) ->
    _assert (info.incoming) __LOC__ "" >>=? fun () ->
    _assert (P2p_peer.Id.compare info.peer_id id2.peer_id = 0)
      __LOC__ "" >>=? fun () ->
    P2p_socket.kick auth_fd >>= fun () ->
    return_unit

  let client _ch sched addr port =
    connect sched addr port id2 >>=? fun auth_fd ->
    P2p_socket.accept ~canceler auth_fd encoding >>= fun conn ->
    _assert (is_rejected conn) __LOC__ "" >>=? fun () ->
    return_unit

  let run _dir = run_nodes client server

end

module Kicked = struct

  let encoding = Data_encoding.bytes

  let server _ch sched socket =
    accept sched socket >>=? fun (_info, auth_fd) ->
    P2p_socket.accept ~canceler auth_fd encoding >>= fun conn ->
    _assert (Kick.is_rejected conn) __LOC__ "" >>=? fun () ->
    return_unit

  let client _ch sched addr port =
    connect sched addr port id2 >>=? fun auth_fd ->
    P2p_socket.kick auth_fd >>= fun () ->
    return_unit

  let run _dir = run_nodes client server

end

module Simple_message = struct

  let encoding = Data_encoding.bytes

  let simple_msg = Rand.generate (1 lsl 4)
  let simple_msg2 = Rand.generate (1 lsl 4)

  let server ch sched socket =
    accept sched socket >>=? fun (_info, auth_fd) ->
    P2p_socket.accept ~canceler auth_fd encoding >>=? fun conn ->
    P2p_socket.write_sync conn simple_msg >>=? fun () ->
    P2p_socket.read conn >>=? fun (_msg_size, msg) ->
    _assert (MBytes.compare simple_msg2 msg = 0) __LOC__ "" >>=? fun () ->
    sync ch >>=? fun () ->
    P2p_socket.close conn >>= fun _stat ->
    return_unit

  let client ch sched addr port =
    connect sched addr port id2 >>=? fun auth_fd ->
    P2p_socket.accept ~canceler auth_fd encoding >>=? fun conn ->
    P2p_socket.write_sync conn simple_msg2 >>=? fun () ->
    P2p_socket.read conn >>=? fun (_msg_size, msg) ->
    _assert (MBytes.compare simple_msg msg = 0) __LOC__ "" >>=? fun () ->
    sync ch >>=? fun () ->
    P2p_socket.close conn >>= fun _stat ->
    return_unit

  let run _dir = run_nodes client server

end

module Chunked_message = struct

  let encoding = Data_encoding.bytes

  let simple_msg = Rand.generate (1 lsl 8)
  let simple_msg2 = Rand.generate (1 lsl 8)

  let server ch sched socket =
    accept sched socket >>=? fun (_info, auth_fd) ->
    P2p_socket.accept
      ~canceler
      ~binary_chunks_size:21 auth_fd encoding >>=? fun conn ->
    P2p_socket.write_sync conn simple_msg >>=? fun () ->
    P2p_socket.read conn >>=? fun (_msg_size, msg) ->
    _assert (MBytes.compare simple_msg2 msg = 0) __LOC__ "" >>=? fun () ->
    sync ch >>=? fun () ->
    P2p_socket.close conn >>= fun _stat ->
    return_unit

  let client ch sched addr port =
    connect sched addr port id2 >>=? fun auth_fd ->
    P2p_socket.accept
      ~canceler
      ~binary_chunks_size:21 auth_fd encoding >>=? fun conn ->
    P2p_socket.write_sync conn simple_msg2 >>=? fun () ->
    P2p_socket.read conn >>=? fun (_msg_size, msg) ->
    _assert (MBytes.compare simple_msg msg = 0) __LOC__ "" >>=? fun () ->
    sync ch >>=? fun () ->
    P2p_socket.close conn >>= fun _stat ->
    return_unit

  let run _dir = run_nodes client server

end

module Oversized_message = struct

  let encoding = Data_encoding.bytes

  let simple_msg = Rand.generate (1 lsl 17)
  let simple_msg2 = Rand.generate (1 lsl 17)

  let server ch sched socket =
    accept sched socket >>=? fun (_info, auth_fd) ->
    P2p_socket.accept ~canceler auth_fd encoding >>=? fun conn ->
    P2p_socket.write_sync conn simple_msg >>=? fun () ->
    P2p_socket.read conn >>=? fun (_msg_size, msg) ->
    _assert (MBytes.compare simple_msg2 msg = 0) __LOC__ "" >>=? fun () ->
    sync ch >>=? fun () ->
    P2p_socket.close conn >>= fun _stat ->
    return_unit

  let client ch sched addr port =
    connect sched addr port id2 >>=? fun auth_fd ->
    P2p_socket.accept ~canceler auth_fd encoding >>=? fun conn ->
    P2p_socket.write_sync conn simple_msg2 >>=? fun () ->
    P2p_socket.read conn >>=? fun (_msg_size, msg) ->
    _assert (MBytes.compare simple_msg msg = 0) __LOC__ "" >>=? fun () ->
    sync ch >>=? fun () ->
    P2p_socket.close conn >>= fun _stat ->
    return_unit

  let run _dir = run_nodes client server

end

module Close_on_read = struct

  let encoding = Data_encoding.bytes

  let simple_msg = Rand.generate (1 lsl 4)

  let server ch sched socket =
    accept sched socket >>=? fun (_info, auth_fd) ->
    P2p_socket.accept ~canceler auth_fd encoding >>=? fun conn ->
    sync ch >>=? fun () ->
    P2p_socket.close conn >>= fun _stat ->
    return_unit

  let client ch sched addr port =
    connect sched addr port id2 >>=? fun auth_fd ->
    P2p_socket.accept ~canceler auth_fd encoding >>=? fun conn ->
    sync ch >>=? fun () ->
    P2p_socket.read conn >>= fun err ->
    _assert (is_connection_closed err) __LOC__ "" >>=? fun () ->
    P2p_socket.close conn >>= fun _stat ->
    return_unit

  let run _dir = run_nodes client server

end

module Close_on_write = struct

  let encoding = Data_encoding.bytes

  let simple_msg = Rand.generate (1 lsl 4)

  let server ch sched socket =
    accept sched socket >>=? fun (_info, auth_fd) ->
    P2p_socket.accept ~canceler auth_fd encoding >>=? fun conn ->
    P2p_socket.close conn >>= fun _stat ->
    sync ch >>=? fun ()->
    return_unit

  let client ch sched addr port =
    connect sched addr port id2 >>=? fun auth_fd ->
    P2p_socket.accept ~canceler auth_fd encoding >>=? fun conn ->
    sync ch >>=? fun ()->
    Lwt_unix.sleep 0.1 >>= fun () ->
    P2p_socket.write_sync conn simple_msg >>= fun err ->
    _assert (is_connection_closed err) __LOC__ "" >>=? fun () ->
    P2p_socket.close conn >>= fun _stat ->
    return_unit

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
    P2p_socket.accept ~canceler auth_fd encoding >>=? fun conn ->
    P2p_socket.raw_write_sync conn garbled_msg >>=? fun () ->
    P2p_socket.read conn >>= fun err ->
    _assert (is_connection_closed err) __LOC__ "" >>=? fun () ->
    P2p_socket.close conn >>= fun _stat ->
    return_unit

  let client _ch sched addr port =
    connect sched addr port id2 >>=? fun auth_fd ->
    P2p_socket.accept ~canceler auth_fd encoding >>=? fun conn ->
    P2p_socket.read conn >>= fun err ->
    _assert (is_decoding_error err) __LOC__ "" >>=? fun () ->
    P2p_socket.close conn >>= fun _stat ->
    return_unit

  let run _dir = run_nodes client server

end

let log_config = ref None

let spec = Arg.[

    "--addr", String (fun p -> addr := Ipaddr.V6.of_string_exn p),
    " Listening addr";

    "-v", Unit (fun () ->
        log_config := Some (
            Lwt_log_sink_unix.create_cfg
              ~rules:("test.p2p.connection -> info; p2p.connection -> info")
              () )),
    " Log up to info msgs" ;

    "-vv", Unit (fun () ->
        log_config := Some (
            Lwt_log_sink_unix.create_cfg
              ~rules:("test.p2p.connection -> debug; p2p.connection -> debug")
              () )),
    " Log up to debug msgs";

  ]

let init_logs = lazy (Internal_event_unix.init ?lwt_log_sink:!log_config ()) 

let wrap n f =
  Alcotest_lwt.test_case n `Quick begin fun _ () ->
    Lazy.force init_logs >>= fun () ->
    f () >>= function
    | Ok () -> Lwt.return_unit
    | Error error ->
        Format.kasprintf Pervasives.failwith "%a" pp_print_error error
  end

let main () =
  let anon_fun _num_peers = raise (Arg.Bad "No anonymous argument.") in
  let usage_msg = "Usage: %s.\nArguments are:" in
  Arg.parse spec anon_fun usage_msg ;
  Alcotest.run ~argv:[|""|] "tezos-p2p" [
    "p2p-connection.", [
      wrap "low-level" Low_level.run ;
      wrap "kick" Kick.run ;
      wrap "kicked" Kicked.run ;
      wrap "simple-message" Simple_message.run ;
      wrap "chunked-message" Chunked_message.run ;
      wrap "oversized-message" Oversized_message.run ;
      wrap "close-on-read" Close_on_read.run ;
      wrap "close-on-write" Close_on_write.run ;
      wrap "garbled-data" Garbled_data.run ;
    ]
  ]

let () =
  Sys.catch_break true ;
  try main ()
  with _ -> ()
