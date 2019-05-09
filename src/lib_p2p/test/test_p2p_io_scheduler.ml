(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

include Internal_event.Legacy_logging.Make (struct
    let name = "test-p2p-io-scheduler"
  end)

exception Error of error list

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
    Lwt_unix.listen main_socket 50 ;
    Lwt.return (main_socket, tentative_port)
  end begin function
    | Unix.Unix_error
        ((Unix.EADDRINUSE | Unix.EADDRNOTAVAIL), _, _) when port = None ->
        listen addr
    | exn -> Lwt.fail exn
  end

let accept main_socket =
  P2p_fd.accept main_socket >>= fun (fd, _sockaddr) ->
  return fd

let rec accept_n main_socket n =
  if n <= 0 then
    return_nil
  else
    accept_n main_socket (n-1) >>=? fun acc ->
    accept main_socket >>=? fun conn ->
    return (conn :: acc)

let connect addr port =
  let fd = P2p_fd.socket PF_INET6 SOCK_STREAM 0 in
  let uaddr =
    Lwt_unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port) in
  P2p_fd.connect fd uaddr >>= fun () ->
  return fd

let simple_msgs =
  [|
    MBytes.create (1 lsl 6) ;
    MBytes.create (1 lsl 7) ;
    MBytes.create (1 lsl 8) ;
    MBytes.create (1 lsl 9) ;
    MBytes.create (1 lsl 10) ;
    MBytes.create (1 lsl 11) ;
    MBytes.create (1 lsl 12) ;
    MBytes.create (1 lsl 13) ;
    MBytes.create (1 lsl 14) ;
    MBytes.create (1 lsl 15) ;
    MBytes.create (1 lsl 16) ;
  |]
let nb_simple_msgs = Array.length simple_msgs

let receive conn =
  let buf = MBytes.create (1 lsl 16) in
  let rec loop () =
    P2p_io_scheduler.read conn buf >>= function
    | Ok _ -> loop ()
    | Error [P2p_errors.Connection_closed] ->
        Lwt.return_unit
    | Error err -> Lwt.fail (Error err)
  in
  loop ()

let server
    ?(display_client_stat = true)
    ?max_download_speed ?read_queue_size ~read_buffer_size
    main_socket n =
  let sched =
    P2p_io_scheduler.create
      ?max_download_speed
      ?read_queue_size
      ~read_buffer_size
      () in
  Moving_average.on_update begin fun () ->
    log_notice "Stat: %a" P2p_stat.pp (P2p_io_scheduler.global_stat sched) ;
    if display_client_stat then
      P2p_io_scheduler.iter_connection sched
        (fun conn ->
           log_notice
             " client(%d) %a"
             (P2p_io_scheduler.id conn)
             P2p_stat.pp (P2p_io_scheduler.stat conn)) ;
  end ;
  (* Accept and read message until the connection is closed. *)
  accept_n main_socket n >>=? fun conns ->
  let conns = List.map (P2p_io_scheduler.register sched) conns in
  Lwt.join (List.map receive conns) >>= fun () ->
  iter_p P2p_io_scheduler.close conns >>=? fun () ->
  log_notice "OK %a" P2p_stat.pp (P2p_io_scheduler.global_stat sched) ;
  return_unit

let max_size ?max_upload_speed () =
  match max_upload_speed with
  | None -> nb_simple_msgs
  | Some max_upload_speed ->
      let rec loop n =
        if n <= 1 then 1
        else if MBytes.length simple_msgs.(n-1) <= max_upload_speed then n
        else loop (n - 1)
      in
      loop nb_simple_msgs

let rec send conn nb_simple_msgs =
  Lwt_main.yield () >>= fun () ->
  let msg = simple_msgs.(Random.int nb_simple_msgs) in
  P2p_io_scheduler.write conn msg >>=? fun () ->
  send conn nb_simple_msgs

let client ?max_upload_speed ?write_queue_size addr port time _n =
  let sched =
    P2p_io_scheduler.create
      ?max_upload_speed ?write_queue_size ~read_buffer_size:(1 lsl 12) () in
  connect addr port >>=? fun conn ->
  let conn = P2p_io_scheduler.register sched conn in
  let nb_simple_msgs = max_size ?max_upload_speed () in
  Lwt.pick [ send conn nb_simple_msgs ;
             Lwt_unix.sleep time >>= return ] >>=? fun () ->
  P2p_io_scheduler.close conn >>=? fun () ->
  let stat = P2p_io_scheduler.stat conn in
  lwt_log_notice "Client OK %a" P2p_stat.pp stat >>= fun () ->
  return_unit

let run
    ?display_client_stat
    ?max_download_speed ?max_upload_speed
    ~read_buffer_size ?read_queue_size ?write_queue_size
    addr port time n =
  Internal_event_unix.init () >>= fun () ->
  listen ?port addr >>= fun (main_socket, port) ->
  Process.detach ~prefix:"server: " begin fun _ ->
    server
      ?display_client_stat ?max_download_speed
      ~read_buffer_size ?read_queue_size
      main_socket n
  end >>= fun server_node ->
  let client n =
    let prefix = Printf.sprintf "client(%d): " n in
    Process.detach ~prefix begin fun _ ->
      Lwt_utils_unix.safe_close main_socket >>= fun () ->
      client ?max_upload_speed ?write_queue_size addr port time n
    end in
  Lwt_list.map_p client (1 -- n) >>= fun client_nodes ->
  Process.wait_all (server_node :: client_nodes)

let () = Random.self_init ()

let addr = ref Ipaddr.V6.localhost
let port = ref None

let max_download_speed = ref None
let max_upload_speed = ref None

let read_buffer_size = ref (1 lsl 14)
let read_queue_size = ref (Some (1 lsl 14))
let write_queue_size = ref (Some (1 lsl 14))

let delay = ref 60.
let clients = ref 8

let display_client_stat = ref None

let spec =
  Arg.[

    "--port", Int (fun p -> port := Some p), " Listening port";

    "--addr", String (fun p -> addr := Ipaddr.V6.of_string_exn p),
    " Listening addr";

    "--max-download-speed", Int (fun i -> max_download_speed := Some i),
    " Max download speed in B/s (default: unbounded)";

    "--max-upload-speed", Int (fun i -> max_upload_speed := Some i),
    " Max upload speed in B/s (default: unbounded)";

    "--read-buffer-size", Set_int read_buffer_size,
    " Size of the read buffers";

    "--read-queue-size", Int (fun i ->
        read_queue_size := if i <= 0 then None else Some i),
    " Size of the read queue (0=unbounded)";

    "--write-queue-size", Int (fun i ->
        write_queue_size := if i <= 0 then None else Some i),
    " Size of the write queue (0=unbounded)";

    "--delay", Set_float delay,  " Client execution time.";
    "--clients", Set_int clients,  " Number of concurrent clients.";

    "--hide-clients-stat", Unit (fun () -> display_client_stat := Some false),
    " Hide the client bandwidth statistic." ;

    "--display_clients_stat", Unit (fun () -> display_client_stat := Some true),
    " Display the client bandwidth statistic." ;

  ]

let () =
  let anon_fun _num_peers = raise (Arg.Bad "No anonymous argument.") in
  let usage_msg = "Usage: %s <num_peers>.\nArguments are:" in
  Arg.parse spec anon_fun usage_msg

let init_logs = lazy (Internal_event_unix.init  ())

let wrap n f =
  Alcotest_lwt.test_case n `Quick begin fun _ () ->
    Lazy.force init_logs >>= fun () ->
    f () >>= function
    | Ok () -> Lwt.return_unit
    | Error error ->
        Format.kasprintf Pervasives.failwith "%a" pp_print_error error
  end

let () =
  Alcotest.run ~argv:[|""|] "tezos-p2p" [
    "p2p.io-scheduler", [
      wrap "trivial-quota" (fun () ->
          run
            ?display_client_stat:!display_client_stat
            ?max_download_speed:!max_download_speed
            ?max_upload_speed:!max_upload_speed
            ~read_buffer_size:!read_buffer_size
            ?read_queue_size:!read_queue_size
            ?write_queue_size:!write_queue_size
            !addr !port !delay !clients)
    ]
  ]
