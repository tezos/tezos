(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

include Logging.Make (struct let name = "p2p.welcome" end)

type pool = Pool : ('msg, 'meta) P2p_pool.t -> pool

type t = {
  socket: Lwt_unix.file_descr ;
  canceler: Lwt_canceler.t ;
  pool: pool ;
  mutable worker: unit Lwt.t ;
}

let rec worker_loop st =
  let Pool pool = st.pool in
  Lwt_unix.yield () >>= fun () ->
  Lwt_utils.protect ~canceler:st.canceler begin fun () ->
    Lwt_unix.accept st.socket >>= return
  end >>= function
  | Ok (fd, addr) ->
      let point =
        match addr with
        | Lwt_unix.ADDR_UNIX _ -> assert false
        | Lwt_unix.ADDR_INET (addr, port) ->
            (Ipaddr_unix.V6.of_inet_addr_exn addr, port) in
      P2p_pool.accept pool fd point ;
      worker_loop st
  | Error [Lwt_utils.Canceled] ->
      Lwt.return_unit
  | Error err ->
      lwt_log_error "@[<v 2>Unexpected error in the Welcome worker@ %a@]"
        pp_print_error err >>= fun () ->
      Lwt.return_unit

let create_listening_socket ~backlog ?(addr = Ipaddr.V6.unspecified) port =
  let main_socket = Lwt_unix.(socket PF_INET6 SOCK_STREAM 0) in
  Lwt_unix.(setsockopt main_socket SO_REUSEADDR true) ;
  Lwt_unix.bind main_socket
    Unix.(ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port)) >>= fun () ->
  Lwt_unix.listen main_socket backlog ;
  Lwt.return main_socket

let run ~backlog pool ?addr port  =
  Lwt.catch begin fun () ->
    create_listening_socket
      ~backlog ?addr port >>= fun socket ->
    let canceler = Lwt_canceler.create () in
    Lwt_canceler.on_cancel canceler begin fun () ->
      Lwt_utils.safe_close socket
    end ;
    let st = {
      socket ; canceler ; pool = Pool pool ;
      worker = Lwt.return_unit ;
    } in
    st.worker <-
      Lwt_utils.worker "welcome"
        ~run:(fun () -> worker_loop st)
        ~cancel:(fun () -> Lwt_canceler.cancel st.canceler) ;
    Lwt.return st
  end begin fun exn ->
    lwt_log_error
      "@[<v 2>Cannot accept incoming connections@ %a@]"
      pp_exn exn >>= fun () ->
    Lwt.fail exn
  end

let shutdown st =
  Lwt_canceler.cancel st.canceler >>= fun () ->
  st.worker
