(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open P2p_types
include Logging.Make (struct let name = "p2p.discovery" end)

type t = ()
let create _pool = ()
let restart () = (() : unit)
let shutdown () = Lwt.return_unit

let inet_addr = Unix.inet_addr_of_string "ff0e::54:455a:3053"

module Message = struct

  let encoding =
    Data_encoding.(tup3 (Fixed.string 10) Peer_id.encoding int16)

  let length = Data_encoding.Binary.fixed_length_exn encoding

  let make peer_id port =
    Data_encoding.Binary.to_bytes encoding ("DISCOMAGIC", peer_id, port)

end

(* Sends discover messages into space in an exponentially delayed loop,
   restartable using a condition *)
let sender sock saddr my_peer_id inco_port cancelation restart =
  let buf = Message.make my_peer_id inco_port in
  let rec loop delay n =
    Lwt.catch
      (fun () ->
         Lwt_bytes.sendto sock buf 0 Message.length [] saddr >>= fun _nb_sent ->
         Lwt.return_unit)
        (fun exn ->
          lwt_debug "(%a) error broadcasting a discovery request: %a"
            Peer_id.pp my_peer_id Error_monad.pp (Exn exn)) >>= fun () ->
    Lwt.pick
      [ (Lwt_unix.sleep delay >>= fun () -> Lwt.return (Some (delay, n + 1))) ;
        (cancelation () >>= fun () -> Lwt.return_none) ;
        (Lwt_condition.wait restart >>= fun () -> Lwt.return (Some (0.1, 0))) ]
    >>= function
    | Some (delay, n) when n = 10 -> loop delay 9
    | Some (delay, n) -> loop (delay *. 2.) n
    | None -> Lwt.return_unit
  in
  loop 0.2 1

let create_socket (iface, disco_addr, disco_port) =
  let usock = Unix.socket PF_INET6 SOCK_DGRAM 0 in
  let sock = Lwt_unix.of_unix_file_descr ~blocking:false usock in
  let saddr = Unix.ADDR_INET (disco_addr, disco_port) in
  Unix.setsockopt usock SO_REUSEADDR true ;
  Ipv6_multicast.Unix.bind ?iface usock saddr ;
  Ipv6_multicast.Unix.membership ?iface usock disco_addr `Join ;
  iface, sock, saddr

(*
module Answerer = struct

  (* Launch an answer machine for the discovery mechanism, takes a
     callback to fill the answers and returns a canceler function *)
  let answerer sock my_peer_id cancelation callback =
    (* the answering function *)
    let buf = MBytes.create Message.length in
    let rec step () =
      Lwt.pick
        [ (cancelation () >>= fun () -> Lwt.return_none) ;
          (Lwt_bytes.recvfrom sock buf 0 Message.length [] >>= fun r ->
           Lwt.return (Some r)) ] >>= function
      | None -> Lwt.return_unit
      | Some (len', Lwt_unix.ADDR_INET (remote_addr, _mcast_port))
        when len' = Message.length -> begin
          match (Data_encoding.Binary.of_bytes Message.encoding buf) with
          | Some ("DISCOMAGIC", remote_peer_id, remote_inco_port)
            when remote_peer_id <> my_peer_id ->
              Lwt.catch
                (fun () -> callback ~remote_addr ~remote_inco_port)
                (fun exn ->
                   lwt_debug "Error processing a discovery request: %a"
                     pp_exn exn) >>=
              step
          | _ ->
              step ()
        end
      | Some _ -> step ()
    in
    step ()

  let worker_loop st =
    let callback ~remote_addr ~remote_inco_port =
      let remote_uaddr = Ipaddr_unix.V6.of_inet_addr_exn remote_addr in
      P2p_connection_loop.notify_new_peer
    in
    Lwt.catch
      (fun () ->
         Lwt_utils.worker
           (Format.asprintf "(%a) discovery answerer" Peer_id.pp my_peer_id)
           (fun () -> answerer fd my_peer_id cancelation callback)
           cancel)
      (fun exn ->
         lwt_log_error "Discovery answerer not started: %a"
           Error_monad.pp (Exn exn))

end
let discovery_sender =
  match config.pending_authentification_port with
  | None -> Lwt.return_unit
  | Some inco_port ->
      Lwt.catch
        (fun () ->
           let sender () =
             Discovery.sender fd
               saddr my_peer_id inco_port cancelation restart_discovery in
           Lwt_utils.worker
             (Format.asprintf "(%a) discovery sender" Peer_id.pp my_peer_id)
             sender cancel)
        (fun exn ->
           lwt_log_error "Discovery sender not started: %a"
             Error_monad.pp (Exn exn))


let discovery_answerer, discovery_sender =
  match map_option ~f:create_socket st.config.local_discovery with
  | exception exn ->
      log_error "Error creating discovery socket: %a" Error_monad.pp (Exn exn) ;
      (Lwt.return_unit, Lwt.return_unit)
  | None -> Lwt.return_unit, Lwt.return_unit
  | Some (iface, fd, saddr) ->
      discovery_answerer, discovery_sender

*)
