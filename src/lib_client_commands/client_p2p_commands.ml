(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let group =
  { Clic.name = "p2p" ;
    title = "Commands for monitoring and controlling p2p-layer state" }

let port_arg () =
  let open Clic in
  default_arg
    ~long:"port"
    ~placeholder:"IP port"
    ~doc:"peer-to-peer port of the node"
    ~default: "9732"
    (parameter
       (fun _ x -> try
           return (int_of_string x)
         with Failure _ ->
           failwith "Invalid peer-to-peer port"))

let pp_connection_info ppf conn = P2p_connection.Info.pp (fun _ _ -> ()) ppf conn

let commands () =
  let open Clic in
  let addr_parameter =
    parameter (fun _ x -> return (P2p_addr.of_string_exn x))
  in
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
                  Time.pp_hum ts
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
      (args1 (port_arg ()))
      (prefixes [ "connect" ; "address" ]
       @@ param ~name:"address" ~desc:"IPv4 or IPV6 address" addr_parameter
       @@ stop)
      (fun port address (cctxt : #Client_context.full) ->
         P2p_services.connect cctxt ~timeout:10. (address, port)
      ) ;

    command ~group ~desc: "Remove an IP address from the blacklist and whitelist."
      no_options
      (prefixes [ "forget" ; "address" ]
       @@ param ~name:"address" ~desc:"IPv4 or IPV6 address" addr_parameter
       @@ stop)
      (fun () address (cctxt : #Client_context.full) ->
         P2p_services.Points.forget cctxt (address, 0)
      ) ;

    command ~group ~desc: "Add an IP address to the blacklist."
      no_options
      (prefixes [ "ban" ; "address" ]
       @@ param ~name:"address" ~desc:"IPv4 or IPV6 address" addr_parameter
       @@ stop)
      (fun () address (cctxt : #Client_context.full) ->
         P2p_services.Points.ban cctxt (address, 0)
      ) ;

    command ~group ~desc: "Add an IP address to the whitelist."
      no_options
      (prefixes [ "trust" ; "address" ]
       @@ param ~name:"address" ~desc:"IPv4 or IPV6 address" addr_parameter
       @@ stop)
      (fun () address (cctxt : #Client_context.full) ->
         P2p_services.Points.trust cctxt (address, 0)
      ) ;

    command ~group ~desc: "Check if an IP address is banned."
      no_options
      (prefixes [ "is" ; "address" ; "banned" ]
       @@ param ~name:"address" ~desc:"IPv4 or IPV6 address" addr_parameter
       @@ stop)
      (fun () address (cctxt : #Client_context.full) ->
         P2p_services.Points.banned cctxt (address, 0) >>=? fun banned ->
         cctxt#message
           "The given ip address is %s"
           (if banned then "banned" else "not banned") >>= fun () ->
         return_unit
      ) ;

    command ~group ~desc: "Remove a peer ID from the blacklist and whitelist."
      no_options
      (prefixes [ "forget" ; "peer" ]
       @@ P2p_peer.Id.param ~name:"peer" ~desc:"peer network identity"
       @@ stop)
      (fun () peer (cctxt : #Client_context.full) ->
         P2p_services.Peers.forget cctxt peer
      ) ;

    command ~group ~desc: "Add a peer ID to the blacklist."
      no_options
      (prefixes [ "ban" ; "peer" ]
       @@ P2p_peer.Id.param ~name:"peer" ~desc:"peer network identity"
       @@ stop)
      (fun () peer (cctxt : #Client_context.full) ->
         P2p_services.Peers.ban cctxt peer
      ) ;

    command ~group ~desc: "Add a peer ID to the whitelist."
      no_options
      (prefixes [ "trust" ; "peer" ]
       @@ P2p_peer.Id.param ~name:"peer" ~desc:"peer network identity"
       @@ stop)
      (fun () peer (cctxt : #Client_context.full) ->
         P2p_services.Peers.trust cctxt peer
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

    command ~group ~desc: "Clear all ACLs."
      no_options
      (prefixes [ "clear" ; "acls" ] @@ stop)
      (fun () (cctxt : #Client_context.full) ->
         P2p_services.ACL.clear cctxt ()
      ) ;
  ]
