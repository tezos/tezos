open Internal_pervasives

type t =
  { id: string
  ; expected_connections: int
  ; rpc_port: int
  ; p2p_port: int
  ; (* Ports: *)
    peers: int list
  ; exec: [`Node] Tezos_executable.t
  ; protocol: Tezos_protocol.t }

let ef t =
  EF.(
    desc_list (af "Node:%S" t.id)
      [ desc (af "rpc") (af ":%d" t.rpc_port)
      ; desc (af "p2p") (af ":%d" t.p2p_port)
      ; desc_list (af "peers") (List.map t.peers ~f:(af ":%d")) ])

let pp fmt t = Easy_format.Pretty.to_formatter fmt (ef t)

let make ~exec ?(protocol = Tezos_protocol.default ()) id ~expected_connections
    ~rpc_port ~p2p_port peers =
  {id; expected_connections; rpc_port; p2p_port; peers; exec; protocol}

let make_path p ~config t = Paths.root config // sprintf "node-%s" t.id // p

(* Data-dir should not exist OR be fully functional. *)
let data_dir ~config t = make_path "data-dir" ~config t
let config_file ~config t = data_dir ~config t // "config.json"
let identity_file ~config t = data_dir ~config t // "identity.json"
let log_output ~config t = make_path "node-output.log" ~config t
let exec_path ~config t = make_path ~config "exec" t

open Tezos_executable.Make_cli

let node_command t ~config cmd options =
  Tezos_executable.call t.exec ~path:(exec_path t ~config)
    ( cmd
    @ opt "config-file" (config_file ~config t)
    @ opt "data-dir" (data_dir ~config t)
    @ options )

let config_options t ~config =
  opt "log-output" (log_output ~config t)
  @ optf "rpc-addr" "0.0.0.0:%d" t.rpc_port
  @ optf "net-addr" "0.0.0.0:%d" t.p2p_port
  @ optf "expected-pow" "%d" (Tezos_protocol.expected_pow t.protocol)

let run_command t ~config =
  let peers = List.concat_map t.peers ~f:(optf "peer" "127.0.0.1:%d") in
  node_command t ~config ["run"]
    ( flag "private-mode" @ flag "no-bootstrap-peers" @ peers
    @ optf "bootstrap-threshold" "0"
    @ optf "connections" "%d" t.expected_connections
    @ opt "sandbox" (Tezos_protocol.sandbox_path ~config t.protocol) )

let start_script t ~config =
  let open Genspio.EDSL in
  let gen_id =
    node_command t ~config
      [ "identity"; "generate"
      ; sprintf "%d" (Tezos_protocol.expected_pow t.protocol) ]
      []
  in
  let tmp_config = tmp_file (config_file t ~config) in
  check_sequence
    ~verbosity:(`Announce (sprintf "Node-%s-start" t.id))
    [ (let opts = config_options t ~config in
       ( "config-init"
       , if_seq
           (file_exists (str (config_file t ~config)))
           ~t:[node_command t ~config ["config"; "reset"] opts]
           ~e:[node_command t ~config ["config"; "init"] opts] ))
    ; ( "update-config"
      , seq
          [ write_stdout ~path:tmp_config#path
              (exec
                 [ "jq"
                 ; {jq|.p2p += { "limits" : { "connection-timeout" : 2, "swap-linger" : 2 } }|jq}
                 ; config_file t ~config ])
          ; call [str "mv"; tmp_config#path; str (config_file t ~config)] ] )
    ; ( "ensure-identity"
      , ensure "node-id"
          ~condition:(file_exists (str (identity_file t ~config)))
          ~how:[("gen-id", gen_id)] )
    ; ("start", run_command t ~config) ]

let process config t =
  Running_processes.Process.genspio t.id (start_script t ~config)

let protocol t = t.protocol

let connections node_list =
  let module Connection = struct
    type node = t

    type t =
      [ `Duplex of node * node
      | `From_to of node * node
      | `Missing of node * int ]

    let compare a b =
      match (a, b) with
      | `Duplex (a, b), `Duplex (c, d) when a = d && b = c -> 0
      | `Duplex _, _ -> -1
      | _, `Duplex _ -> 1
      | _, _ -> Caml.Pervasives.compare a b
  end in
  let module Connection_set = Set.Make (Connection) in
  let res = ref Connection_set.empty in
  List.iter node_list ~f:(fun node ->
      let peer_nodes =
        List.map node.peers ~f:(fun p2p ->
            match
              List.find node_list ~f:(fun {p2p_port; _} -> p2p_port = p2p)
            with
            | None -> `Unknown p2p
            | Some n -> `Peer n )
      in
      List.iter peer_nodes ~f:(fun peer_opt ->
          let conn =
            match peer_opt with
            | `Unknown p2p -> `Missing (node, p2p)
            | `Peer peer ->
                if List.mem peer.peers node.p2p_port ~equal:Int.equal then
                  `Duplex (node, peer)
                else `From_to (node, peer)
          in
          res := Connection_set.add conn !res ) ) ;
  Connection_set.elements !res
