open Internal_pervasives

type kind = [`Baker | `Endorser | `Accuser]

type args =
  | Baker : string -> args
  | Endorser : string -> args
  | Accuser : args

type t =
  { node: Tezos_node.t
  ; client: Tezos_client.t
  ; exec: kind Tezos_executable.t
  ; args: args }

let of_node node args ~exec ~client = {node; exec; client; args}
let baker_of_node nod ~key = of_node nod (Baker key)
let endorser_of_node nod ~key = of_node nod (Endorser key)
let accuser_of_node nod = of_node nod Accuser

let arg_to_string = function
  | Baker k -> sprintf "baker-%s" k
  | Endorser k -> sprintf "endorser-%s" k
  | Accuser -> "accuser"

let to_script (t : t) ~state =
  let base_dir = Tezos_client.base_dir ~state t.client in
  let call t args =
    Tezos_executable.call t.exec
      ~path:
        ( base_dir
        // sprintf "exec-%s-%d" (arg_to_string t.args)
             t.node.Tezos_node.rpc_port )
      args
  in
  match t.args with
  | Baker key ->
      let node_path = Tezos_node.data_dir ~config:state t.node in
      call t
        [ "--port"
        ; sprintf "%d" t.node.Tezos_node.rpc_port
        ; "--base-dir"; base_dir; "run"; "with"; "local"; "node"; node_path
        ; key ]
  | Endorser key ->
      call t
        [ "--port"
        ; sprintf "%d" t.node.Tezos_node.rpc_port
        ; "--base-dir"; base_dir; "run"; key ]
  | Accuser ->
      call t
        [ "--port"
        ; sprintf "%d" t.node.Tezos_node.rpc_port
        ; "--base-dir"; base_dir; "run"; "--preserved-levels"; "10" ]

let process (t : t) ~state =
  Running_processes.Process.genspio
    (sprintf "%s-for-%s" (arg_to_string t.args) t.node.Tezos_node.id)
    (to_script t ~state)
