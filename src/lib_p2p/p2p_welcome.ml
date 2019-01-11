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

include Internal_event.Legacy_logging.Make (struct let name = "p2p.welcome" end)

type pool = Pool : ('msg, 'meta, 'meta_conn) P2p_pool.t -> pool

type t = {
  socket: Lwt_unix.file_descr ;
  canceler: Lwt_canceler.t ;
  pool: pool ;
  mutable worker: unit Lwt.t ;
}

let rec worker_loop st =
  let Pool pool = st.pool in
  Lwt_unix.yield () >>= fun () ->
  protect ~canceler:st.canceler begin fun () ->
    P2p_fd.accept st.socket >>= return
  end >>= function
  | Ok (fd, addr) ->
      let point =
        match addr with
        | Lwt_unix.ADDR_UNIX _ -> assert false
        | Lwt_unix.ADDR_INET (addr, port) ->
            (Ipaddr_unix.V6.of_inet_addr_exn addr, port) in
      P2p_pool.accept pool fd point ;
      worker_loop st
  | Error [ Canceled ] ->
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

let create ?addr ~backlog pool port =
  Lwt.catch begin fun () ->
    create_listening_socket
      ~backlog ?addr port >>= fun socket ->
    let canceler = Lwt_canceler.create () in
    Lwt_canceler.on_cancel canceler begin fun () ->
      Lwt_utils_unix.safe_close socket
    end ;
    let st = {
      socket ; canceler ; pool = Pool pool ;
      worker = Lwt.return_unit ;
    } in
    Lwt.return st
  end begin fun exn ->
    lwt_log_error
      "@[<v 2>Cannot accept incoming connections@ %a@]"
      pp_exn exn >>= fun () ->
    Lwt.fail exn
  end

let activate st =
  st.worker <-
    Lwt_utils.worker "welcome"
      ~on_event:Internal_event.Lwt_worker_event.on_event
      ~run:(fun () -> worker_loop st)
      ~cancel:(fun () -> Lwt_canceler.cancel st.canceler)

let shutdown st =
  Lwt_canceler.cancel st.canceler >>= fun () ->
  st.worker
