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

let group =
  { Clic.name = "p2p" ;
    title = "Commands for monitoring and controlling p2p-layer state" }

let pp_connection_info ppf conn = P2p_connection.Info.pp (fun _ _ -> ()) ppf conn

let addr_parameter =
  let open Clic in
  param ~name:"address"
    ~desc:"<IPv4>:PORT or <IPV6>:PORT address (PORT defaults to 9732)."
    (parameter (fun _ x -> return (P2p_point.Id.of_string_exn x)))

let commands () =
  let open Clic in
  [
    command ~group ~desc: "show global network status"
      no_options
      (prefixes ["p2p" ; "stat"] stop) begin fun () (cctxt : #Client_context.full) ->
      Shell_services.P2p.stat cctxt >>=? fun stat ->
      Shell_services.P2p.Connections.list cctxt >>=? fun conns ->
      Shell_services.P2p.Peers.list cctxt >>=? fun peers ->
      Shell_services.P2p.Points.list cctxt >>=? fun points ->
      cctxt#message "GLOBAL STATS" >>= fun () ->
      cctxt#message "  %a" P2p_stat.pp stat >>= fun () ->
      cctxt#message "CONNECTIONS" >>= fun () ->
      let incoming, outgoing =
        List.partition (fun c -> c.P2p_connection.Info.incoming) conns in
      Lwt_list.iter_s begin fun conn ->
        cctxt#message "  %a" pp_connection_info conn
      end incoming >>= fun () ->
      Lwt_list.iter_s begin fun conn ->
        cctxt#message "  %a" pp_connection_info conn
      end outgoing >>= fun () ->
      cctxt#message "KNOWN PEERS" >>= fun () ->
      Lwt_list.iter_s begin fun (p, pi) ->
        cctxt#message "  %a  %.0f %a %a %s"
          P2p_peer.State.pp_digram pi.P2p_peer.Info.state
          pi.score
          P2p_peer.Id.pp p
          P2p_stat.pp pi.stat
          (if pi.trusted then "★" else " ")
      end peers >>= fun () ->
      cctxt#message "KNOWN POINTS" >>= fun () ->
      Lwt_list.iter_s begin fun (p, pi) ->
        match pi.P2p_point.Info.state with
        | Running peer_id ->
            cctxt#message "  %a  %a %a %s"
              P2p_point.State.pp_digram pi.state
              P2p_point.Id.pp p
              P2p_peer.Id.pp peer_id
              (if pi.trusted then "★" else " ")
        | _ ->
            match pi.last_seen with
            | Some (peer_id, ts) ->
                cctxt#message "  %a  %a (last seen: %a %a) %s"
                  P2p_point.State.pp_digram pi.state
                  P2p_point.Id.pp p
                  P2p_peer.Id.pp peer_id
                  Time.System.pp_hum ts
                  (if pi.trusted then "★" else " ")
            | None ->
                cctxt#message "  %a  %a %s"
                  P2p_point.State.pp_digram pi.state
                  P2p_point.Id.pp p
                  (if pi.trusted then "★" else " ")
      end points >>= fun () ->
      return_unit
    end ;

    command ~group ~desc: "Connect to a new point."
      no_options
      (prefixes [ "connect" ; "address" ]
       @@ addr_parameter
       @@ stop)
      (fun () (address, port) (cctxt : #Client_context.full) ->
         let timeout = Time.System.Span.of_seconds_exn 10. in
         P2p_services.connect cctxt ~timeout (address, port) >>=? fun () ->
         cctxt#message "Connection to %a:%d established." P2p_addr.pp address port >>= fun () ->
         return_unit
      ) ;

    command ~group ~desc: "Kick a peer."
      no_options
      (prefixes [ "kick" ; "peer" ]
       @@ P2p_peer.Id.param ~name:"peer" ~desc:"peer network identity"
       @@ stop)
      (fun () peer (cctxt : #Client_context.full) ->
         P2p_services.Connections.kick cctxt peer >>=? fun () ->
         cctxt#message "Connection to %a interrupted." P2p_peer.Id.pp peer >>= fun () ->
         return_unit
      ) ;

    command ~group ~desc: "Add an IP address and all its ports to the \
                           blacklist and kicks it. Remove the address \
                           from the whitelist if it was previously in \
                           it."
      no_options
      (prefixes [ "ban" ; "address" ]
       @@ addr_parameter
       @@ stop)
      (fun () (address, _port) (cctxt : #Client_context.full) ->
         P2p_services.Points.ban cctxt (address, 0) >>=? fun () ->
         cctxt#message "Address %a:* is now banned." P2p_addr.pp address >>= fun () ->
         return_unit
      ) ;

    command ~group ~desc: "Remove an IP address and all its ports \
                           from the blacklist."
      no_options
      (prefixes [ "unban" ; "address" ]
       @@ addr_parameter
       @@ stop)
      (fun () (address, _port) (cctxt : #Client_context.full) ->
         P2p_services.Points.unban cctxt (address, 0) >>=? fun () ->
         cctxt#message "Address %a:* is now unbanned." P2p_addr.pp address >>= fun () ->
         return_unit
      ) ;

    command ~group ~desc: "Add an IP address to the whitelist. Remove \
                           the address from the blacklist if it was \
                           previously in it."
      no_options
      (prefixes [ "trust" ; "address" ]
       @@ addr_parameter
       @@ stop)
      (fun () (address, port) (cctxt : #Client_context.full) ->
         P2p_services.Points.trust cctxt (address, port) >>=? fun () ->
         cctxt#message "Address %a:%d is now trusted."
           P2p_addr.pp address port >>= fun () ->
         return_unit
      ) ;

    command ~group ~desc: "Removes an IP address from the whitelist."
      no_options
      (prefixes [ "untrust" ; "address" ]
       @@ addr_parameter
       @@ stop)
      (fun () (address, port) (cctxt : #Client_context.full) ->
         P2p_services.Points.untrust cctxt (address, port) >>=? fun () ->
         cctxt#message "Address %a:%d is now untrusted."
           P2p_addr.pp address port >>= fun () ->
         return_unit
      ) ;

    command ~group ~desc: "Check if an IP address is banned."
      no_options
      (prefixes [ "is" ; "address" ; "banned" ]
       @@ addr_parameter
       @@ stop)
      (fun () (address, port) (cctxt : #Client_context.full) ->
         P2p_services.Points.banned cctxt (address, port) >>=? fun banned ->
         cctxt#message
           "The given ip address is %s"
           (if banned then "banned" else "not banned") >>= fun () ->
         return_unit
      ) ;

    command ~group ~desc: "Check if a peer ID is banned."
      no_options
      (prefixes [ "is" ; "peer" ; "banned" ]
       @@ P2p_peer.Id.param ~name:"peer" ~desc:"peer network identity"
       @@ stop)
      (fun () peer (cctxt : #Client_context.full) ->
         P2p_services.Peers.banned cctxt peer >>=? fun banned ->
         cctxt#message
           "The given peer ID is %s"
           (if banned then "banned" else "not banned") >>= fun () ->
         return_unit
      ) ;

    command ~group ~desc: "Add a peer ID to the blacklist and kicks \
                           it. Remove the peer ID from the blacklist \
                           if was previously in it."
      no_options
      (prefixes [ "ban" ; "peer" ]
       @@ P2p_peer.Id.param ~name:"peer" ~desc:"peer network identity"
       @@ stop)
      (fun () peer (cctxt : #Client_context.full) ->
         P2p_services.Peers.ban cctxt peer >>=? fun () ->
         cctxt#message "The peer %a is now banned."
           P2p_peer.Id.pp_short peer >>= fun () ->
         return_unit
      ) ;

    command ~group ~desc: "Removes a peer ID from the blacklist."
      no_options
      (prefixes [ "unban" ; "peer" ]
       @@ P2p_peer.Id.param ~name:"peer" ~desc:"peer network identity"
       @@ stop)
      (fun () peer (cctxt : #Client_context.full) ->
         P2p_services.Peers.unban cctxt peer >>=? fun () ->
         cctxt#message "The peer %a is now unbanned."
           P2p_peer.Id.pp_short peer >>= fun () ->
         return_unit
      ) ;

    command ~group ~desc: "Add a peer ID to the whitelist. Remove the \
                           peer ID from the blacklist if it was \
                           previously in it."
      no_options
      (prefixes [ "trust" ; "peer" ]
       @@ P2p_peer.Id.param ~name:"peer" ~desc:"peer network identity"
       @@ stop)
      (fun () peer (cctxt : #Client_context.full) ->
         P2p_services.Peers.trust cctxt peer >>=? fun () ->
         cctxt#message "The peer %a is now trusted."
           P2p_peer.Id.pp_short peer >>= fun () ->
         return_unit
      ) ;

    command ~group ~desc: "Remove a peer ID from the whitelist."
      no_options
      (prefixes [ "untrust" ; "peer" ]
       @@ P2p_peer.Id.param ~name:"peer" ~desc:"peer network identity"
       @@ stop)
      (fun () peer (cctxt : #Client_context.full) ->
         P2p_services.Peers.untrust cctxt peer >>=? fun () ->
         cctxt#message "The peer %a is now untrusted."
           P2p_peer.Id.pp_short peer >>= fun () ->
         return_unit
      ) ;

    command ~group ~desc: "Clear all access control rules."
      no_options
      (prefixes [ "clear" ; "acls" ] @@ stop)
      (fun () (cctxt : #Client_context.full) ->
         P2p_services.ACL.clear cctxt () >>=? fun () ->
         cctxt#message "The access control rules are now cleared." >>= fun () ->
         return_unit
      ) ;
  ]
