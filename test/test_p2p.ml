open Lwt.Infix
open P2p

include Logging.Make (struct let name = "test-p2p" end)

module Param = struct
  type msg = unit
  let encodings = [Encoding { tag = 0x10;
                              encoding = Data_encoding.null;
                              wrap = (fun () -> ());
                              unwrap = (fun () -> Some ());
                              max_length = Some 0;
                            }]

  type metadata = unit
  let initial_metadata = ()
  let metadata_encoding = Data_encoding.empty
  let score () = 0.

  let supported_versions = [ { name = "TEST"; major = 0; minor = 0; } ]
end

let peer_of_string peer =
  let p = String.rindex peer ':' in
  let addr = String.sub peer 0 p in
  let port = String.(sub peer (p+1) (length peer - p - 1)) in
  Ipaddr.of_string_exn addr, int_of_string port

include Make(Param)

let print_peer_info { gid; addr; port; version = { name; major; minor } } =
  Printf.sprintf "%s:%d (%s.%d.%d)" (Ipaddr.to_string addr) port name major minor

let peers_file = ref @@ Filename.temp_file "p2p-test" ""

let main () =
  let incoming_port = ref @@ Some 11732 in
  let discovery_port = ref None in
  let known_peers = ref [] in
  let closed_network = ref false in

  let max_packet_size = ref 1024 in
  let peer_answer_timeout = ref 10. in
  let expected_connections = ref 1 in
  let min_connections = ref 0 in
  let max_connections = ref 10 in
  let blacklist_time = ref 100. in

  let spec = Arg.[
      "-iport", Int (fun p -> incoming_port := Some p), " Incoming port";
      "-dport", Int (fun p -> discovery_port := Some p), " Discovery port";
      "-peers-file", Set_string peers_file, " Peers filepath";
      "-closed", Set closed_network, " Closed network mode";

      "-max-packet-size", Set_int max_packet_size, "int Max size of packets";
      "-peer-answer-timeout", Set_float peer_answer_timeout, "float Number of seconds";
      "-expected-connections", Set_int expected_connections, "conns Expected connections";
      "-min-connections", Set_int min_connections, "conns Minimal number of connections";
      "-max-connections", Set_int max_connections, "conns num of connections";
      "-blacklist-time", Set_float blacklist_time, "float Number of seconds";
      "-v", Unit (fun () -> Lwt_log_core.(add_rule "*" Info)), " Log up to info msgs";
      "-vv", Unit (fun () -> Lwt_log_core.(add_rule "*" Debug)), " Log up to debug msgs";
    ]
  in
  let anon_fun peer = known_peers := peer_of_string peer :: !known_peers in
  let usage_msg = "Test P2p. Arguments are:" in
  Arg.parse spec anon_fun usage_msg;
    let config = {
      incoming_port = !incoming_port;
      discovery_port = !discovery_port;
      known_peers = !known_peers;
      peers_file = !peers_file;
      closed_network = !closed_network;
    }
    in
    let limits = {
      max_packet_size = !max_packet_size;
      peer_answer_timeout = !peer_answer_timeout;
      expected_connections = !expected_connections;
      min_connections = !min_connections;
      max_connections = !max_connections;
      blacklist_time = !blacklist_time;
    }
    in
    bootstrap ~config ~limits >>= fun net ->
    let rec loop () =
      ListLabels.iter (peers net) ~f:begin fun p ->
        let pi = peer_info net p in
        log_info "%s" (print_peer_info pi)
      end;
      Lwt_unix.sleep 3. >>=
      loop
    in
    loop ()

let () =
  Sys.catch_break true;
  try
    Lwt_main.run @@ main ()
  with _ ->
    Sys.remove !peers_file
