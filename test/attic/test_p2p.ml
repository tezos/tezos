(*
ocamlfind ocamlopt \
   -package 'lwt,lwt.unix,lwt.log,ezjsonm,ocplib-endian,config-file,cryptokit,cstruct' \
  ../core/utils.cmx ../core/logs.cmx ../core/mMBytes.cmx ../core/json.cmx \
  netbits.cmx p2p.cmx test_p2p.ml -linkpkg \
  -o test_p2p && ./test_p2p
*)

open Lwt
open P2p
open Netbits
open Printf

let interval min max cb =
  let rec loop acc n =
    if n > max then List.rev acc
    else loop ((n, cb n) :: acc) (n + 1) in
  loop [] min

let rec join_map acc = function
  | [] -> return (List.rev acc)
  | (i, t) :: ts -> t >>= fun v -> join_map ((i, v) :: acc) ts

let show_peers creds net =
  Printf.printf " - Network of %s\n%!" creds ;
  let peers = peers net in
  peers |> List.iter @@ fun peer ->
  let addr, port, _ = peer_info peer net in
  Printf.printf "    * %s @ %s:%i\n%!" creds addr port


(* launch 15 peers who originally know all the others, make everyone
   send toto, and wait for everyone to receive one toto *)
let toto () =
  let known_peers =
    interval 0 15 (fun i -> ("127.0.0.1", 4440 + i)) |> List.split |> snd
  in
  let net i =
    bootstrap
      { incoming_port = Some (4440 + i) ;
        discovery_port = None ;
        supported_versions = [ "TEST", 0, 0 ] ;
        known_peers;
        peers_file = sprintf "test_peers_toto_%d.json" i }
      { max_packet_size = 10_000 ;
        peer_answer_timeout = 2. ;
        expected_connections = 5 ;
        min_connections = 2 ;
        max_connections = 20 ;
        blacklist_time = 30. }
  in
  let nets = interval 0 15 (fun i -> net i) in
  printf "---- Networks created\n%!" ;
  join_map [] nets >>= fun nets ->
  printf "---- Networks bootstrapped\n%!" ;
  List.iter (fun (_, net) -> broadcast [ B (MBytes.of_string "TOTO") ] net) nets ;
  printf "---- Messages sent\n%!" ;
  let recv (i, net) = i, recv net >>= fun (_, m) ->
    Printf.printf "user_%d received %s\n%!" i (Netbits.to_string m) ;
    return m
  in
  let receptions = List.map recv nets in
  join_map [] receptions >>= fun msgs ->
  printf "---- Messages received\n%!" ;
  let shutdowns = List.map (fun (i, net) -> i, shutdown net) nets in
  join_map [] shutdowns >>= fun _ ->
  printf "---- Networks shutdown\n%!" ;
  return ()

(* launch 15 peers who originally know only one another, make everyone
   connect to at least 10 others *)
let boot () =
  let net i =
    bootstrap
      { incoming_port = Some (4440 + i) ;
        discovery_port = None ;
        supported_versions = [ "TEST", 0, 0 ] ;
        known_peers = [ "127.0.0.1", 4440 + ((i + 1) mod 16) ] ;
        peers_file = sprintf "test_peers_boot_%d.json" i }
      { max_packet_size = 10_000 ;
        peer_answer_timeout = 2. ;
        expected_connections = 10 ;
        min_connections = 2 ;
        max_connections = 20 ;
        blacklist_time = 30. }
  in
  let nets = interval 0 15 (fun i -> net i) in
  printf "---- Networks created\n%!" ;
  join_map [] nets >>= fun nets ->
  printf "---- Networks bootstrapped\n%!" ;
  List.iter (fun (i, net) -> show_peers (sprintf "user_%d" i) net) nets ;
  let shutdowns = List.map (fun (i, net) -> i, shutdown net) nets in
  join_map [] shutdowns >>= fun _ ->
  printf "---- Networks shutdown\n%!" ;
  return ()

(* the same as above, but five nodes have no input port, and some
   known addresses are bad ones *)
let boot_with_unreachable () =
  let net i =
    bootstrap
      { incoming_port = if i > 10 then None else Some (4440 + i) ;
        discovery_port = None ;
        supported_versions = [ "TEST", 0, 0 ] ;
        known_peers = [ "127.0.0.1", 4440 + ((i + 1) mod 10) ;
                        "127.0.0.1", 9999 ] ;
        peers_file = sprintf "test_peers_boot_%d.json" i }
      { max_packet_size = 10_000 ;
        peer_answer_timeout = 2. ;
        expected_connections = 10 ;
        min_connections = 5 ;
        max_connections = 15 ;
        blacklist_time = 30. }
  in
  let nets = interval 0 15 (fun i -> net i) in
  printf "---- Networks created\n%!" ;
  join_map [] nets >>= fun nets ->
  printf "---- Networks bootstrapped\n%!" ;
  List.iter (fun (i, net) -> show_peers (sprintf "user_%d" i) net) nets ;
  let shutdowns = List.map (fun (i, net) -> i, shutdown net) nets in
  join_map [] shutdowns >>= fun _ ->
  printf "---- Networks shutdown\n%!" ;
  return ()

(* connect to ten peers, only by using local discovery *)
let boot_by_discovery () =
  let net i =
    bootstrap
      { incoming_port = Some (4440 + i) ;
        discovery_port = Some 121212 ;
        supported_versions = [ "TEST", 0, 0 ] ;
        known_peers = [] ;
        peers_file = sprintf "test_peers_boot_%d.json" i }
      { max_packet_size = 10_000 ;
        peer_answer_timeout = 2. ;
        expected_connections = 10 ;
        min_connections = 5 ;
        max_connections = 15 ;
        blacklist_time = 30. }
  in
  let nets = interval 0 15 (fun i -> net i) in
  printf "---- Networks created\n%!" ;
  join_map [] nets >>= fun nets ->
  printf "---- Networks bootstrapped\n%!" ;
  List.iter (fun (i, net) -> show_peers (sprintf "user_%d" i) net) nets ;
  let shutdowns = List.map (fun (i, net) -> i, shutdown net) nets in
  join_map [] shutdowns >>= fun _ ->
  printf "---- Networks shutdown\n%!" ;
  return ()

let _ = Lwt_main.run (boot_by_discovery ())
