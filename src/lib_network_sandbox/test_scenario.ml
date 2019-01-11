open Internal_pervasives

module Inconsistency_error = struct
  type t = [`Empty_protocol_list | `Too_many_protocols of Tezos_protocol.t list]

  let should_be_one_protocol = function
    | [one] -> return one
    | [] -> fail `Empty_protocol_list
    | more -> fail (`Too_many_protocols more)

  let pp fmt err =
    Format.fprintf fmt "Wrong number of protocols in network: %d"
      ( match err with
      | `Empty_protocol_list -> 0
      | `Too_many_protocols p -> List.length p )
end

module Topology = struct
  type node = Tezos_node.t

  type _ t =
    | Mesh : {size: int} -> node list t
    | Bottleneck :
        { name: string
        ; left: 'a network
        ; right: 'b network }
        -> ('a * node * 'b) t
    | Net_in_the_middle :
        { middle: 'm network
        ; left: 'a network
        ; right: 'b network }
        -> ('a * 'm * 'b) t

  and 'a network = {topology: 'a t; name: string}

  let make name topology = {name; topology}
  let mesh name size = Mesh {size} |> make name
  let sub = make
  let bottleneck name left right = Bottleneck {name; left; right} |> make name

  let net_in_the_middle name middle left right =
    Net_in_the_middle {middle; left; right} |> make name

  let rec node_count : type a. a t -> int = function
    | Mesh {size} -> size
    | Bottleneck {left; right; _} ->
        1 + node_count left.topology + node_count right.topology
    | Net_in_the_middle {left; right; middle} ->
        node_count middle.topology + node_count left.topology
        + node_count right.topology

  let rec node_ids : type a. a t -> a -> string list =
   fun topo res ->
    match (topo, res) with
    | Mesh _, l -> List.map l ~f:(fun nod -> nod.Tezos_node.id)
    | Bottleneck {left; right; _}, (l, i, r) ->
        (i.Tezos_node.id :: node_ids left.topology l)
        @ node_ids right.topology r
    | Net_in_the_middle {left; right; middle}, (l, i, r) ->
        node_ids middle.topology i @ node_ids left.topology l
        @ node_ids right.topology r

  let rec node_names : type a. ?prefix:string -> a network -> string list =
   fun ?(prefix = "") {name; topology} ->
    let make_ith i = sprintf "%s%03d" prefix i in
    let continue a = node_names ~prefix:(prefix ^ name) a in
    match topology with
    | Mesh {size} -> List.init size ~f:make_ith
    | Bottleneck {name; left; right} ->
        (sprintf "%s%s" prefix name :: continue left) @ continue right
    | Net_in_the_middle {left; right; middle} ->
        continue middle @ continue left @ continue right

  let build ?protocol ?(base_port = 15_001) ~exec network =
    let all_ports = ref [] in
    let next_port = ref (base_port + (base_port mod 2)) in
    let rpc name =
      match List.find !all_ports ~f:(fun (n, _) -> n = name) with
      | Some (_, p) -> p
      | None ->
          let p = !next_port in
          all_ports := (name, p) :: !all_ports ;
          next_port := !next_port + 2 ;
          p
    in
    let p2p n = rpc n + 1 in
    let node peers id =
      let rpc_port = rpc id in
      let p2p_port = p2p id in
      let expected_connections = List.length peers in
      let peers =
        List.filter_map peers ~f:(fun p ->
            if p <> id then Some (p2p p) else None )
      in
      Tezos_node.make ?protocol ~exec id ~expected_connections ~rpc_port
        ~p2p_port peers
    in
    let dbgp prefx names =
      Printf.eprintf "%s:\n  %s\n%!" prefx
        (String.concat ~sep:"\n  "
           (List.map names ~f:(fun n -> sprintf "%s:%d" n (p2p n))))
    in
    let rec make : type a.
        ?extra_peers:string list -> prefix:string -> a network -> a =
     fun ?(extra_peers = []) ~prefix network ->
      let prefix = prefix ^ network.name in
      let make ?extra_peers n = make ?extra_peers ~prefix n in
      match network.topology with
      | Bottleneck {name; left; right} ->
          let intermediate = name in
          let extra_peers = [intermediate] in
          let left_nodes = make ~extra_peers left in
          let right_nodes = make ~extra_peers right in
          let intermediate_node =
            let peers =
              node_ids left.topology left_nodes
              @ node_ids right.topology right_nodes
            in
            node peers intermediate
          in
          (left_nodes, intermediate_node, right_nodes)
      | Net_in_the_middle {middle; left; right} ->
          let middle_names =
            node_names ~prefix:(prefix ^ middle.name) middle
          in
          dbgp "Mid-name" middle_names ;
          let left_nodes =
            make ~extra_peers:(extra_peers @ middle_names) left
          in
          let right_nodes =
            make ~extra_peers:(extra_peers @ middle_names) right
          in
          let intermediate_nodes =
            let peers =
              node_ids left.topology left_nodes
              @ node_ids right.topology right_nodes
            in
            dbgp "peers" peers ;
            dbgp "extr-peers" extra_peers ;
            dbgp "left-names" (node_names ~prefix:(prefix ^ left.name) left) ;
            dbgp "right-names" (node_names ~prefix:(prefix ^ right.name) right) ;
            make ~extra_peers:(peers @ extra_peers) middle
          in
          (left_nodes, intermediate_nodes, right_nodes)
      | Mesh _ ->
          let all = node_names ~prefix network in
          dbgp "mesh-names" all ;
          let nodes = List.map all ~f:(fun n -> node (all @ extra_peers) n) in
          nodes
    in
    make ~prefix:"" network
end

module Network = struct
  type t = {nodes: Tezos_node.t list}

  let make nodes = {nodes}

  let netstat state =
    Running_processes.run_cmdf state "netstat -nut"
    >>= fun res ->
    Process_result.Error.fail_if_non_zero res "netstat -nut command"
    >>= fun () ->
    let rows =
      List.filter_mapi res#out ~f:(fun idx line ->
          match
            String.split line ~on:' '
            |> List.filter_map ~f:(fun s ->
                   match String.strip s with "" -> None | s -> Some s )
          with
          | ("tcp" | "tcp6") :: _ as row -> Some (`Tcp (idx, row))
          | _ -> Some (`Wrong (idx, line)) )
    in
    return rows

  let all_listening_ports rows =
    List.filter_map rows ~f:(function
      | `Tcp (_, _ :: _ :: _ :: addr :: _) as row -> (
        match String.split addr ~on:':' with
        | [_; port] -> ( try Some (Int.of_string port, row) with _ -> None )
        | _ -> None )
      | _ -> None )

  let netstat_listening_ports state =
    netstat state
    >>= fun rows ->
    let all_used = all_listening_ports rows in
    return all_used

  let start_up ?(check_ports = true) state ~client_exec {nodes} =
    ( if check_ports then
      netstat_listening_ports state
      >>= fun all_used ->
      let taken port =
        List.find all_used ~f:(fun (p, _) -> Int.equal p port)
      in
      List_sequential.iter nodes
        ~f:(fun {Tezos_node.id; rpc_port; p2p_port; _} ->
          let fail s (p, `Tcp (_, row)) =
            System_error.fail "Node: %S's %s port %d already in use {%s}" id s
              p
              (String.concat ~sep:"|" row)
          in
          let time_wait (_, `Tcp (_, row)) =
            List.last row = Some "TIME_WAIT"
          in
          match (taken rpc_port, taken p2p_port) with
          | None, None -> return ()
          | Some p, _ -> if time_wait p then return () else fail "RPC" p
          | _, Some p -> if time_wait p then return () else fail "P2P" p )
    else return () )
    >>= fun () ->
    let protocols =
      List.map ~f:Tezos_node.protocol nodes
      |> List.dedup_and_sort ~compare:Tezos_protocol.compare
    in
    Inconsistency_error.should_be_one_protocol protocols
    >>= fun protocol ->
    Tezos_protocol.ensure protocol ~config:state
    >>= fun () ->
    List.fold nodes ~init:(return ()) ~f:(fun prev_m node ->
        prev_m
        >>= fun () ->
        Running_processes.start state (Tezos_node.process state node)
        >>= fun _ -> return () )
    >>= fun () ->
    let node_0 = List.hd_exn nodes in
    let client = Tezos_client.of_node node_0 ~exec:client_exec in
    Dbg.e EF.(af "Trying to bootstrap client") ;
    Tezos_client.bootstrapped client ~state
    >>= fun () ->
    Tezos_client.activate_protocol client ~state protocol
    >>= fun () ->
    Dbg.e EF.(af "Waiting for all nodes to be bootstrapped") ;
    List_sequential.iter nodes ~f:(fun node ->
        let client = Tezos_client.of_node node ~exec:client_exec in
        Tezos_client.bootstrapped client ~state )
end

let network_with_protocol ?base_port ?(size = 5) ?protocol state ~node_exec
    ~client_exec =
  let nodes =
    Topology.build ?base_port ?protocol ~exec:node_exec
      (Topology.mesh "N" size)
  in
  let protocols =
    List.map ~f:Tezos_node.protocol nodes
    |> List.dedup_and_sort ~compare:Tezos_protocol.compare
  in
  Inconsistency_error.should_be_one_protocol protocols
  >>= fun protocol ->
  Network.start_up state ~client_exec (Network.make nodes)
  >>= fun () -> return (nodes, protocol)

module Queries = struct
  let all_levels state ~nodes =
    List.fold nodes ~init:(return [])
      ~f:(fun prevm {Tezos_node.id; rpc_port; _} ->
        prevm
        >>= fun prev ->
        Running_processes.run_cmdf state
          "curl http://localhost:%d/chains/main/blocks/head/metadata | jq \
           .level.level"
          rpc_port
        >>= fun lvl ->
        Console.display_errors_of_command state lvl ~should_output:true
        >>= function
        | true ->
            let res = String.concat ~sep:"\n" lvl#out in
            let parsed =
              match Int.of_string res with
              | i -> `Level i
              | exception _ -> (
                match res with "null" -> `Null | unknown -> `Unknown unknown )
            in
            return ((id, parsed) :: prev)
        | false -> return ((id, `Failed) :: prev) )
    >>= fun results ->
    let sorted =
      List.sort results ~compare:(fun (a, _) (b, _) -> String.compare a b)
    in
    return sorted

  let wait_for_all_levels_to_be state ~attempts ~seconds nodes level =
    let check_level =
      match level with
      | `Equal_to l -> ( = ) l
      | `At_least l -> fun x -> x >= l
    in
    let level_string =
      match level with
      | `Equal_to l -> sprintf "= %d" l
      | `At_least l -> sprintf "≥ %d" l
    in
    let msg ids =
      let show_node (id, res) =
        sprintf "%s (%s)" id
          ( match res with
          | `Failed -> "failed"
          | `Level l -> sprintf "%d" l
          | `Null -> "null"
          | `Unknown s -> sprintf "¿¿ %S ??" s )
      in
      sprintf "Waiting for %s to reach level %s"
        (String.concat (List.map ~f:show_node ids) ~sep:", ")
        level_string
    in
    Console.say state
      EF.(
        wf "Checking for all levels to be %s (nodes: %s)" level_string
          (String.concat ~sep:", "
             (List.map nodes ~f:(fun n -> n.Tezos_node.id))))
    >>= fun () ->
    Helpers.wait_for state ~attempts ~seconds (fun _nth ->
        all_levels state ~nodes
        >>= fun results ->
        let not_readys =
          List.filter_map results ~f:(function
            | _, `Level n when check_level n -> None
            | id, res -> Some (id, res) )
        in
        match not_readys with
        | [] -> return (`Done ())
        | ids -> return (`Not_done (msg ids)) )
end
