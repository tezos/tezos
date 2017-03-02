(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let group =
  { Cli_entries.name = "network" ;
    title = "Commands for monitoring and controlling network state" }

let commands () = [
  let open Cli_entries in
  command ~group ~desc: "show global network status"
    (prefixes ["network" ; "stat"] stop) begin fun cctxt ->
    Client_node_rpcs.Network.stat cctxt >>= fun stat ->
    Client_node_rpcs.Network.connections cctxt >>= fun conns ->
    Client_node_rpcs.Network.peers cctxt >>= fun peers ->
    Client_node_rpcs.Network.points cctxt >>= fun points ->
    cctxt.message "GLOBAL STATS" >>= fun () ->
    cctxt.message "  %a" P2p_types.Stat.pp stat >>= fun () ->
    cctxt.message "CONNECTIONS" >>= fun () ->
    let incoming, outgoing =
      List.partition (fun c -> c.P2p_types.Connection_info.incoming) conns in
    Lwt_list.iter_s begin fun conn ->
      cctxt.message "  %a" P2p_types.Connection_info.pp conn
    end incoming >>= fun () ->
    Lwt_list.iter_s begin fun conn ->
      cctxt.message "  %a" P2p_types.Connection_info.pp conn
    end outgoing >>= fun () ->
    cctxt.message "KNOWN PEERS" >>= fun () ->
    Lwt_list.iter_s begin fun (p, pi) ->
      let open P2p.RPC.Peer_id in
      cctxt.message "  %a  %.0f %a %a %s"
        pp_state_digram pi.state
        pi.score
        pp p P2p_types.Stat.pp pi.stat
        (if pi.trusted then "★" else " ")
    end peers >>= fun () ->
    cctxt.message "KNOWN POINTS" >>= fun () ->
    Lwt_list.iter_s begin fun (p, pi) ->
      let open P2p.RPC in
      match pi.Point.state with
      | Running peer_id ->
          cctxt.message "  %a  %a %a %s"
            Point.pp_state_digram pi.state
            Point.pp p
            Peer_id.pp peer_id
            (if pi.trusted then "★" else " ")
      | _ ->
          match pi.last_seen with
          | Some (peer_id, ts) ->
              cctxt.message "  %a  %a (last seen: %a %a) %s"
                Point.pp_state_digram pi.state
                Point.pp p
                Peer_id.pp peer_id
                Time.pp_hum ts
                (if pi.trusted then "★" else " ")
          | None ->
              cctxt.message "  %a  %a %s"
                Point.pp_state_digram pi.state
                Point.pp p
                (if pi.trusted then "★" else " ")
    end points >>= fun () ->
    Lwt.return_unit
  end
]
