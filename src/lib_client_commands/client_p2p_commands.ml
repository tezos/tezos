(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let group =
  { Cli_entries.name = "p2p" ;
    title = "Commands for monitoring and controlling p2p-layer state" }

let commands () = [
  let open Cli_entries in
  command ~group ~desc: "show global network status"
    no_options
    (prefixes ["p2p" ; "stat"] stop) begin fun () (cctxt : #Client_context.full_context) ->
    P2p_services.stat cctxt >>=? fun stat ->
    P2p_services.Connections.list cctxt >>=? fun conns ->
    P2p_services.Peers.list cctxt >>=? fun peers ->
    P2p_services.Points.list cctxt >>=? fun points ->
    cctxt#message "GLOBAL STATS" >>= fun () ->
    cctxt#message "  %a" P2p_stat.pp stat >>= fun () ->
    cctxt#message "CONNECTIONS" >>= fun () ->
    let incoming, outgoing =
      List.partition (fun c -> c.P2p_connection.Info.incoming) conns in
    Lwt_list.iter_s begin fun conn ->
      cctxt#message "  %a" P2p_connection.Info.pp conn
    end incoming >>= fun () ->
    Lwt_list.iter_s begin fun conn ->
      cctxt#message "  %a" P2p_connection.Info.pp conn
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
    return ()
  end
]
