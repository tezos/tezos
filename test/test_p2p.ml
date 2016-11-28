open Lwt.Infix
open P2p

include Logging.Make (struct let name = "test-p2p" end)

module Param = struct

  let dump_encoding = Data_encoding.(Variable.list (tup2 string string))

  type msg =
    | Create of string * string
    | Update of string * string
    | Delete of string
    | Dump of (string * string) list

  let encodings = [
    Encoding { tag = 0x10;
               encoding = Data_encoding.(tup2 string string);
               wrap = (function (k, v) -> Create (k, v));
               unwrap = (function Create (k, v) -> Some (k, v) | _ -> None);
               max_length = Some 0x400;
             };
    Encoding { tag = 0x11;
               encoding = Data_encoding.(tup2 string string);
               wrap = (function (k, v) -> Update (k, v));
               unwrap = (function Create (k, v) -> Some (k, v) | _ -> None);
               max_length = Some 0x400;
             };
    Encoding { tag = 0x12;
               encoding = Data_encoding.string;
               wrap = (function x -> Delete x);
               unwrap = (function Delete x -> Some x | _ -> None);
               max_length = Some 0x400;
             };
    Encoding { tag = 0x13;
               encoding = dump_encoding;
               wrap = (function x -> Dump x);
               unwrap = (function Dump x -> Some x | _ -> None);
               max_length = Some 0x10000;
             };
  ]

  type metadata = unit
  let initial_metadata = ()
  let metadata_encoding = Data_encoding.empty
  let score () = 0.

  let supported_versions = [ { name = "TEST"; major = 0; minor = 0; } ]
end

module Net = Make(Param)

let print_peer_info { Net.gid; addr; port; version = { name; major; minor } } =
  Printf.sprintf "%s:%d (%s.%d.%d)" (Ipaddr.to_string addr) port name major minor

let string_of_gid gid = Format.asprintf "%a" pp_gid gid

let net_monitor config limits num_nets net =
  let my_gid_str = string_of_gid @@ Net.gid net in
  let send_msgs_to_neighbours neighbours =
    Lwt_list.iter_p begin fun p ->
      let { Net.gid } = Net.peer_info net p in
      let remote_gid_str = string_of_gid gid in
      Net.send net p (Create (my_gid_str, remote_gid_str)) >>= fun _ ->
      lwt_log_notice "(%s) Done sending msg to %s" my_gid_str remote_gid_str
    end neighbours >>= fun () ->
    lwt_log_notice "(%s) Done sending all msgs." my_gid_str
  in
  let rec inner () =
    let neighbours = Net.peers net in
    let nb_neighbours = List.length neighbours in
    if nb_neighbours < num_nets - 1 then begin
      log_notice "(%s) I have %d peers" my_gid_str nb_neighbours;
      Lwt_unix.sleep 1. >>= inner end
    else begin
      log_notice "(%s) I know all my %d peers" my_gid_str nb_neighbours;
      Lwt.async (fun () -> send_msgs_to_neighbours neighbours);
      let rec recv_peer_msgs acc =
        if List.length acc = num_nets - 1 then begin
          ListLabels.iter acc ~f:(fun (k, v) -> log_info "%s %s" k v);
          Lwt.return_unit
        end
        else begin
          lwt_log_notice "(%s) recv_peers_msgs: Got %d, need %d"
            my_gid_str (List.length acc) (num_nets - 1) >>= fun () ->
          Net.recv net >>= function
          | p, (Create (their_gid, my_gid)) ->
              lwt_log_notice "(%s) Got a message from %s" my_gid_str their_gid >>= fun () ->
              recv_peer_msgs ((their_gid, my_gid) :: acc)
          | _ -> assert false
        end
      in
      recv_peer_msgs []
    end
  in inner ()

let range n =
  let rec inner acc = function
    | -1 -> acc
    | n -> inner (n :: acc) (pred n)
  in
  if n < 0 then invalid_arg "range"
  else inner [] (pred n)

let main () =
  let incoming_port = ref @@ Some 11732 in
  let discovery_port = ref @@ Some 10732 in
  let closed_network = ref false in

  let max_packet_size = ref 1024 in
  let peer_answer_timeout = ref 10. in
  let blacklist_time = ref 100. in
  let num_networks = ref 0 in

  let make_net nb_neighbours n =
    let config = {
      incoming_port = Utils.map_option !incoming_port ~f:(fun p ->  p + n);
      discovery_port = !discovery_port;
      known_peers = [];
      peers_file = "";
      closed_network = !closed_network;
    }
    in
    let limits = {
      max_message_size = !max_packet_size;
      peer_answer_timeout = !peer_answer_timeout;
      expected_connections = nb_neighbours;
      min_connections = nb_neighbours;
      max_connections = nb_neighbours;
      blacklist_time = !blacklist_time;
    }
    in
    Net.bootstrap ~config ~limits >|= fun net ->
    config, limits, net
  in
  let spec = Arg.[
      "-start-port", Int (fun p -> incoming_port := Some p), " Incoming port";
      "-dport", Int (fun p -> discovery_port := Some p), " Discovery port";
      "-closed", Set closed_network, " Closed network mode";

      "-max-packet-size", Set_int max_packet_size, "int Max size of packets";
      "-peer-answer-timeout", Set_float peer_answer_timeout, "float Number of seconds";
      "-blacklist-time", Set_float blacklist_time, "float Number of seconds";
      "-v", Unit (fun () -> Lwt_log_core.(add_rule "*" Info)), " Log up to info msgs";
      "-vv", Unit (fun () -> Lwt_log_core.(add_rule "*" Debug)), " Log up to debug msgs";
    ]
  in
  let anon_fun num_peers = num_networks := int_of_string num_peers in
  let usage_msg = "Usage: %s <num_peers>.\nArguments are:" in
  Arg.parse spec anon_fun usage_msg;
    let nets = range !num_networks in
    Lwt_list.map_p (make_net (pred !num_networks)) nets >>= fun nets ->
    Lwt_list.iter_p (fun (cfg, limits, net) -> net_monitor cfg limits !num_networks net) nets >>= fun () ->
    lwt_log_notice "All done!"

let () =
  Sys.catch_break true;
  try
    Lwt_main.run @@ main ()
  with _ -> ()
