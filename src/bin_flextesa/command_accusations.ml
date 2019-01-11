open Tezos_network_sandbox
open Internal_pervasives
open Console

let default_attempts = 35

let little_mesh_with_bakers ?base_port ?kiln state ~starting_level ~node_exec
    ~client_exec ~bakers () =
  Helpers.clear_root state
  >>= fun () ->
  Interactive_test.Pauser.generic state
    EF.[af "Ready to start"; af "Root path deleted."]
  >>= fun () ->
  let block_interval = 1 in
  let protocol, baker_list =
    let open Tezos_protocol in
    let d = default () in
    let bakers = List.take d.bootstrap_accounts bakers in
    ( { d with
        time_between_blocks= [block_interval; 0]
      ; bootstrap_accounts=
          List.map d.bootstrap_accounts ~f:(fun (n, v) ->
              if List.exists bakers ~f:(fun baker -> n = fst baker) then (n, v)
              else (n, 1_000) ) }
    , bakers )
  in
  let net_size = 3 in
  let topology = Test_scenario.Topology.(mesh "Simple" net_size) in
  let all_nodes =
    Test_scenario.Topology.build ~protocol ~exec:node_exec topology ?base_port
  in
  Helpers.dump_connections state all_nodes
  >>= fun () ->
  Interactive_test.Pauser.add_commands state
    Interactive_test.Commands.(
      all_defaults state ~nodes:all_nodes
      @ [secret_keys state ~protocol; Log_recorder.Operations.show_all state]) ;
  Test_scenario.Network.(start_up state ~client_exec (make all_nodes))
  >>= fun () ->
  let baker nth_node =
    let nth_baker = nth_node mod List.length baker_list in
    let key_name = sprintf "b%d" nth_baker in
    let node = List.nth_exn all_nodes nth_node in
    let client = Tezos_client.of_node node ~exec:client_exec in
    let baker_account = List.nth_exn baker_list nth_baker in
    let bak =
      Tezos_client.Keyed.make client ~key_name
        ~secret_key:(Tezos_protocol.Account.private_key (fst baker_account))
    in
    Tezos_client.Keyed.initialize state bak >>= fun _ -> return (client, bak)
  in
  baker 0
  >>= fun (client_0, baker_0) ->
  baker 1
  >>= fun (client_1, baker_1) ->
  baker 2
  >>= fun (client_2, baker_2) ->
  Interactive_test.Pauser.add_commands state
    Interactive_test.Commands.
      [ arbitrary_command_on_clients state
          ~clients:[client_0; client_1; client_2] ] ;
  Asynchronous_result.map_option kiln ~f:(fun k ->
      Tezos_client.rpc state ~client:client_0 `Get
        ~path:"/chains/main/chain_id"
      >>= fun chain_id_json ->
      let network_id =
        match chain_id_json with `String s -> s | _ -> assert false
      in
      Kiln.start state ~network_id k
        ~bakers:
          ( List.map baker_list ~f:(fun (account, _) ->
                Tezos_protocol.Account.(name account, pubkey_hash account) )
            @ List.map [baker_0; baker_1; baker_2] ~f:(fun bak ->
                  ( bak.key_name
                  , Tezos_protocol.Key.Of_name.pubkey_hash bak.key_name ) )
          |> List.dedup_and_sort ~compare:(fun (_, a) (_, b) ->
                 String.compare a b ) )
        ~node_uris:
          (List.map all_nodes ~f:(fun {Tezos_node.rpc_port; _} ->
               sprintf "http://localhost:%d" rpc_port ))
      >>= fun (pg, kiln) ->
      Interactive_test.Pauser.generic state EF.[af "Started Kiln with its DB."]
  )
  >>= fun (_ : unit option) ->
  let bake msg baker = Tezos_client.Keyed.bake state baker msg in
  List.fold
    (List.init (starting_level - 1) ~f:(fun n -> n))
    ~init:(return ()) (* We are already at level 1, we bake 7 times: *)
    ~f:(fun pm n ->
      pm
      >>= fun () ->
      bake
        (sprintf "first bakes: [%d/%d]" (n + 1) (starting_level - 1))
        baker_0 )
  >>= fun () ->
  Test_scenario.Queries.wait_for_all_levels_to_be state
    ~attempts:default_attempts ~seconds:8. all_nodes (`Equal_to starting_level)
  >>= fun () ->
  Interactive_test.Pauser.generic state
    EF.
      [ af "Clients ready"
      ; af "Node 0 baked %d times." (starting_level - 1)
      ; af "All nodes should be at level %d." starting_level ]
  >>= fun () ->
  return (all_nodes, client_0, baker_0, client_1, baker_1, client_2, baker_2)

let wait_for_operation_in_mempools state ~nodes:all_nodes ~kind ~client_exec
    how =
  let init, combine =
    match how with `At_least_one -> (false, ( || )) | `All -> (true, ( && ))
  in
  Helpers.wait_for state ~attempts:default_attempts ~seconds:8. (fun _ ->
      List.fold ~init:(return init) all_nodes ~f:(fun prev_m node ->
          prev_m
          >>= fun prev ->
          let client = Tezos_client.of_node node ~exec:client_exec in
          Tezos_client.mempool_has_operation state ~client ~kind
          >>= fun client_result -> return (combine client_result prev) )
      >>= function
      | true -> return (`Done ())
      | false ->
          return
            (`Not_done
              (sprintf "Waiting for %S to show up in the mempool" kind)) )

let simple_double_baking ~starting_level ?kiln ~state ~base_port node_exec
    client_exec () =
  little_mesh_with_bakers ~bakers:1 state ~node_exec ~client_exec () ~base_port
    ~starting_level ?kiln
  >>= fun (all_nodes, client_0, baker_0, client_1, baker_1, client_2, baker_2) ->
  let kill_nth nth = List.nth_exn all_nodes nth |> Helpers.kill_node state in
  let restart_nth nth =
    List.nth_exn all_nodes nth |> Helpers.restart_node ~client_exec state
  in
  let number_of_lonely_bakes = 1 in
  kill_nth 1
  >>= fun () ->
  kill_nth 2
  >>= fun () ->
  Loop.n_times number_of_lonely_bakes (fun _ ->
      Tezos_client.Keyed.bake state baker_0 "Bake-on-0" )
  >>= fun () ->
  Tezos_client.get_block_header state ~client:client_0 `Head
  >>= fun baking_0_header ->
  (* This baking will have better fitness so other nodes will have to fetch it. *)
  Tezos_client.Keyed.endorse state baker_0 "endorsing lonely bake-on-0"
  >>= fun () ->
  System.sleep 1.
  >>= fun () ->
  kill_nth 0
  >>= fun () ->
  restart_nth 1
  >>= fun () ->
  restart_nth 2
  >>= fun () ->
  Loop.n_times number_of_lonely_bakes (fun _ ->
      Tezos_client.Keyed.bake state baker_1 "Bake-on-1" )
  >>= fun () ->
  Tezos_client.get_block_header state ~client:client_1 `Head
  >>= fun baking_1_header ->
  restart_nth 0
  >>= fun () ->
  Tezos_client.Keyed.bake state baker_0 "Bake-on-0"
  >>= fun () ->
  Test_scenario.Queries.wait_for_all_levels_to_be state
    ~attempts:default_attempts ~seconds:8. all_nodes
    (`At_least (starting_level + number_of_lonely_bakes + 1))
  >>= fun () ->
  Tezos_client.rpc state ~client:client_1 `Get
    ~path:"/chains/main/blocks/head/hash"
  >>= fun head_hash_json ->
  Interactive_test.Pauser.generic state
    EF.
      [ af "About to forge"
      ; ef_json "Baking 0" baking_0_header
      ; ef_json "Baking 1" baking_1_header
      ; ef_json "Head hash" head_hash_json ]
  >>= fun () ->
  Tezos_client.Keyed.forge_and_inject state baker_1
    ~json:
      (let clean header =
         let open Jqo in
         remove_field header ~name:"hash"
         |> remove_field ~name:"chain_id"
         |> remove_field ~name:"protocol"
       in
       `O
         [ ("branch", head_hash_json)
         ; ( "contents"
           , `A
               [ `O
                   [ ("kind", `String "double_baking_evidence")
                   ; ("bh1", clean baking_0_header)
                   ; ("bh2", clean baking_1_header) ] ] ) ])
  >>= fun result ->
  Interactive_test.Pauser.generic state
    EF.
      [ af "Waiting for accuser to notice double baking"
      ; ef_json "Result of injection" result
      ; af "All nodes reaching level %d"
          (starting_level + number_of_lonely_bakes + 1) ]
  >>= fun () ->
  wait_for_operation_in_mempools state ~nodes:all_nodes
    ~kind:"double_baking_evidence" ~client_exec `All
  >>= fun () ->
  Tezos_client.Keyed.bake state baker_2
    (sprintf "all at lvl %d" (starting_level + number_of_lonely_bakes + 1))
  >>= fun () ->
  let last_level = starting_level + number_of_lonely_bakes + 2 in
  Interactive_test.Pauser.generic state
    EF.[af "Just baked what's the level? Vs %d" last_level]
  >>= fun () ->
  Test_scenario.Queries.wait_for_all_levels_to_be state
    ~attempts:default_attempts ~seconds:8. all_nodes (`Equal_to last_level)
  >>= fun () ->
  Helpers.wait_for state ~attempts:10 ~seconds:4. (fun _ ->
      Tezos_client.block_has_operation state ~client:client_2 ~level:last_level
        ~kind:"double_baking_evidence"
      >>= function
      | true -> return (`Done ())
      | false ->
          return
            (`Not_done
              (sprintf "Waiting for accusation to show up in block %d"
                 last_level)) )
  >>= fun () -> say state EF.(af "Test done.")

let find_endorsement_in_mempool state ~client =
  Helpers.wait_for state ~attempts:4 ~seconds:2. (fun _ ->
      Tezos_client.find_applied_in_mempool state ~client ~f:(fun o ->
          Jqo.field o ~k:"contents"
          |> Jqo.list_exists ~f:(fun op ->
                 (* Dbg.e EF.(ef_json "op" op) ; *)
                 Jqo.field op ~k:"kind" = `String "endorsement" ) )
      >>= function
      | None -> return (`Not_done (sprintf "No endorsement so far"))
      | Some e -> return (`Done e) )

let simple_double_endorsement ~starting_level ?kiln ~state ~base_port node_exec
    client_exec () =
  little_mesh_with_bakers ~bakers:2 state ~node_exec ~client_exec ()
    ~starting_level ~base_port ?kiln
  >>= fun (all_nodes, client_0, baker_0, client_1, baker_1, client_2, baker_2) ->
  (* 2 bakers ⇒ baker_0 and baker_2 are for the same key on ≠ nodes *)
  assert (
    Tezos_client.Keyed.(
      baker_0.key_name = baker_2.key_name
      && baker_0.secret_key = baker_2.secret_key) ) ;
  let node_0 = List.nth_exn all_nodes 0 in
  let node_1 = List.nth_exn all_nodes 1 in
  let node_2 = List.nth_exn all_nodes 2 in
  let baker_1_n0 =
    let open Tezos_client.Keyed in
    let {key_name; secret_key; _} = baker_1 in
    make client_0 ~key_name ~secret_key
  in
  Tezos_client.Keyed.initialize state baker_1_n0
  >>= fun _ ->
  Helpers.kill_node state node_1
  >>= fun () ->
  Helpers.kill_node state node_2
  >>= fun () ->
  Tezos_client.Keyed.bake state baker_0 "baker-0 baking with node 0"
  >>= fun () ->
  Tezos_client.Keyed.endorse state baker_0 "baker-0 endorsing with node 0"
  >>= fun () ->
  Tezos_client.Keyed.endorse state baker_1_n0 "baker-1 endorsing with node 0"
  >>= fun () ->
  find_endorsement_in_mempool state ~client:client_0
  >>= fun endorsement_0 ->
  Helpers.kill_node state node_0
  >>= fun () ->
  Helpers.restart_node state node_2 ~client_exec
  >>= fun () ->
  Tezos_client.Keyed.bake state baker_2 "baker-0 baking with node 2"
  >>= fun () ->
  Tezos_client.Keyed.endorse state baker_2 "baker-0 endorsing with node 2"
  >>= fun () ->
  find_endorsement_in_mempool state ~client:client_2
  >>= fun endorsement_1 ->
  say state
    EF.(
      list
        [ ef_json "Endorsement 0:" endorsement_0
        ; ef_json "Endorsement 1:" endorsement_1 ])
  >>= fun () ->
  Helpers.restart_node state node_1 ~client_exec
  >>= fun () ->
  (* Tezos_client.Keyed.bake state baker_0 "baker-0 baking lonelily"
   * >>= fun () -> *)
  Test_scenario.Queries.wait_for_all_levels_to_be state
    ~attempts:default_attempts ~seconds:8. [node_1; node_2]
    (`Equal_to (starting_level + 1))
  >>= fun () ->
  Helpers.restart_node state node_0 ~client_exec
  >>= fun () ->
  (* TODO: understand why this kick in the butt is necessary for node
     2 (seems like the node was not getting to level starting+2 without
     this). *)
  Helpers.kill_node state node_2
  >>= fun () ->
  Helpers.restart_node state node_2 ~client_exec
  >>= fun () ->
  Test_scenario.Queries.wait_for_all_levels_to_be state
    ~attempts:default_attempts ~seconds:8. all_nodes
    (`Equal_to (starting_level + 1))
  >>= fun () ->
  Tezos_client.rpc state ~client:client_1 `Get
    ~path:"/chains/main/blocks/head/hash"
  >>= fun head_hash_json ->
  let double_endorsement =
    let transform_endorsement endorsement =
      let branch = Jqo.field ~k:"branch" endorsement in
      let signature = Jqo.field ~k:"signature" endorsement in
      let contents =
        match Jqo.field ~k:"contents" endorsement with
        | `A [one] -> one
        | _ -> assert false
      in
      `O
        [("branch", branch); ("operations", contents); ("signature", signature)]
    in
    let inlined_endorsement_1 = transform_endorsement endorsement_0 in
    let inlined_endorsement_2 = transform_endorsement endorsement_1 in
    `O
      [ ("branch", head_hash_json)
      ; ( "contents"
        , `A
            [ `O
                [ ("kind", `String "double_endorsement_evidence")
                ; ("op1", inlined_endorsement_1)
                ; ("op2", inlined_endorsement_2) ] ] ) ]
  in
  Interactive_test.Pauser.generic state
    EF.[ef_json "About to forge" double_endorsement]
  >>= fun () ->
  Tezos_client.Keyed.forge_and_inject state baker_1 ~json:double_endorsement
  >>= fun result ->
  Interactive_test.Pauser.generic state
    EF.[ef_json "Result of injection" result]
  >>= fun () ->
  wait_for_operation_in_mempools state ~nodes:[node_1]
    ~kind:"double_endorsement_evidence" ~client_exec `All
  >>= fun () ->
  let last_level = starting_level + 2 in
  Tezos_client.Keyed.bake state baker_1 (sprintf "level %d" last_level)
  >>= fun () ->
  Tezos_client.Keyed.endorse state baker_1
    (sprintf "endorse level %d" last_level)
  >>= fun () ->
  Test_scenario.Queries.wait_for_all_levels_to_be state
    ~attempts:default_attempts ~seconds:8. all_nodes (`Equal_to last_level)
  >>= fun () ->
  Helpers.wait_for state ~attempts:10 ~seconds:4. (fun _ ->
      (* We check that client-2 sees the evidence from baker-1 *)
      Tezos_client.block_has_operation state ~client:client_2 ~level:last_level
        ~kind:"double_endorsement_evidence"
      >>= function
      | true -> return (`Done ())
      | false ->
          return
            (`Not_done
              (sprintf "Waiting for accusation to show up in block %d"
                 last_level)) )
  >>= fun () -> say state EF.(af "Test done.")

let with_accusers ?kiln ~state ~base_port node_exec accuser_exec client_exec ()
    =
  Helpers.clear_root state
  >>= fun () ->
  let block_interval = 2 in
  let protocol, baker_0_account =
    let open Tezos_protocol in
    let d = default () in
    let baker = List.hd_exn d.bootstrap_accounts in
    ( { d with
        time_between_blocks= [block_interval; block_interval * 2]
      ; bootstrap_accounts=
          List.map d.bootstrap_accounts ~f:(fun (n, v) ->
              if n = fst baker then (n, v) else (n, 1_000) ) }
    , baker )
  in
  let topology =
    Test_scenario.Topology.(
      net_in_the_middle "AT-" (mesh "Mid" 3) (mesh "Main" 4) (mesh "Acc" 4))
  in
  let mesh_nodes, intermediary_nodes, accuser_nodes =
    Test_scenario.Topology.build ~protocol ~exec:node_exec topology ~base_port
  in
  let all_nodes = mesh_nodes @ intermediary_nodes @ accuser_nodes in
  Helpers.dump_connections state all_nodes
  >>= fun () ->
  Test_scenario.Network.(start_up state ~client_exec (make all_nodes))
  >>= fun () ->
  let start_accuser nod =
    let client = Tezos_client.of_node nod ~exec:client_exec in
    let acc = Tezos_daemon.accuser_of_node ~exec:accuser_exec ~client nod in
    Running_processes.start state (Tezos_daemon.process acc ~state)
    >>= fun _ -> return ()
  in
  List_sequential.iter accuser_nodes ~f:start_accuser
  >>= fun () ->
  let key_name = "b0" in
  let baker nth =
    let node = List.nth_exn all_nodes nth in
    let client = Tezos_client.of_node node ~exec:client_exec in
    let bak =
      Tezos_client.Keyed.make client ~key_name
        ~secret_key:(Tezos_protocol.Account.private_key (fst baker_0_account))
      (* ~secret_key:
         *   (Tezos_protocol.Key.Of_name.private_key
         *      (fst baker_0 |> Tezos_protocol.name_to_string)) *)
    in
    Tezos_client.Keyed.initialize state bak >>= fun _ -> return (client, bak)
  in
  baker 0
  >>= fun (client_0, baker_0) ->
  baker 1
  >>= fun (client_1, baker_1) ->
  baker 2
  >>= fun (client_2, baker_2) ->
  Asynchronous_result.map_option kiln ~f:(fun k ->
      Tezos_client.rpc state ~client:client_0 `Get
        ~path:"/chains/main/chain_id"
      >>= fun chain_id_json ->
      let network_id =
        match chain_id_json with `String s -> s | _ -> assert false
      in
      Kiln.start state ~network_id k
        ~bakers:
          [ Tezos_protocol.Account.(
              let acc = fst baker_0_account in
              (name acc, pubkey_hash acc)) ]
        ~node_uris:
          (List.map all_nodes ~f:(fun {Tezos_node.rpc_port; _} ->
               sprintf "http://localhost:%d" rpc_port ))
      >>= fun (pg, kiln) ->
      Interactive_test.Pauser.generic state EF.[af "Started Kiln with its DB."]
  )
  >>= fun (_ : unit option) ->
  Interactive_test.Pauser.add_commands state
    Interactive_test.Commands.(
      all_defaults state ~nodes:all_nodes
      @ [ secret_keys state ~protocol
        ; Log_recorder.Operations.show_all state
        ; arbitrary_command_on_clients state
            ~clients:[client_0; client_1; client_2] ]) ;
  let pause ?force msgs = Interactive_test.Pauser.generic state ?force msgs in
  let starting_level = 10 in
  List.fold
    (List.init (starting_level - 1) ~f:(fun n -> n))
    ~init:(return ()) (* We are already at level 1, we bake 7 times: *)
    ~f:(fun pm n ->
      pm
      >>= fun () ->
      Tezos_client.Keyed.bake state baker_0
        (sprintf "first bakes: [%d/%d]" (n + 1) (starting_level - 1)) )
  >>= fun () ->
  Test_scenario.Queries.wait_for_all_levels_to_be state
    ~attempts:default_attempts ~seconds:8. all_nodes (`Equal_to starting_level)
  >>= fun () ->
  pause
    EF.
      [ af "Two clients ready"
      ; af "Node 0 baked %d times." (starting_level - 1)
      ; af "All nodes should be at level %d." starting_level ]
  >>= fun () ->
  let transfer msg client =
    let dest =
      List.random_element_exn protocol.Tezos_protocol.bootstrap_accounts
      |> fst |> Tezos_protocol.Account.pubkey_hash
    in
    Tezos_client.successful_client_cmd state ~client
      [ "--wait"; "none"; "transfer"; "1"; "from"; key_name; "to"; dest; "--fee"
      ; "0.05" ]
    >>= fun res ->
    say state
      EF.(
        desc
          (af "Successful transfer (%s):" client.Tezos_client.id)
          (ocaml_string_list res#out))
  in
  List_sequential.iter intermediary_nodes ~f:(fun x ->
      Helpers.kill_node state x )
  >>= fun () ->
  let kill_all_but nodes iths =
    List_sequential.iteri nodes ~f:(fun ith n ->
        if List.mem iths ith ~equal:Int.equal then return ()
        else Helpers.kill_node state n )
  in
  let kill_nth_node nodes nth =
    Helpers.kill_node state
      (Option.value_exn ~message:"kill_nth_node" (List.nth nodes nth))
  in
  let restart_nth_node nodes nth =
    Helpers.restart_node state ~client_exec
      (Option.value_exn ~message:"restart_nth_node" (List.nth nodes nth))
  in
  let get_block_header ~client block =
    let path =
      sprintf "/chains/main/blocks/%s/header"
        (match block with `Head -> "head" | `Level i -> Int.to_string i)
    in
    Tezos_client.rpc state ~client `Get ~path
  in
  kill_all_but mesh_nodes [0]
  >>= fun () ->
  let number_of_lonely_bakes = 1 in
  pause EF.[af "Node 0 is the only one alive"]
  >>= fun () ->
  transfer "node0 only alive" client_0
  >>= fun () ->
  Loop.n_times number_of_lonely_bakes (fun n ->
      Tezos_client.Keyed.bake state baker_0 (sprintf "n0 only alive: %d" n) )
  >>= fun () ->
  get_block_header ~client:client_0 `Head
  >>= fun baking_0_header ->
  Tezos_client.Keyed.endorse state baker_0 "self-endorsing"
  >>= fun () ->
  Tezos_client.Keyed.bake state baker_0 "baking self-endorsement"
  >>= fun () ->
  kill_nth_node mesh_nodes 0
  >>= fun () ->
  restart_nth_node mesh_nodes 1
  >>= fun () ->
  transfer "node1 only one alive" client_1
  >>= fun () ->
  Loop.n_times number_of_lonely_bakes (fun _ ->
      Tezos_client.Keyed.bake state baker_1 "after transfer" )
  >>= fun () ->
  get_block_header ~client:client_1 `Head
  >>= fun baking_1_header ->
  kill_nth_node mesh_nodes 1
  >>= fun () ->
  pause
    EF.
      [ af "Node 0 was killed"; af "Node 1 was restarted"
      ; af "Node 1 transfered"; af "Node 1 baked"; af "Node 1 was killed" ]
  >>= fun () ->
  List.fold ~init:(return ()) intermediary_nodes ~f:(fun prev x ->
      prev >>= fun () -> Helpers.restart_node state ~client_exec x )
  >>= fun () ->
  let node_0 = List.nth_exn mesh_nodes 0 in
  let except_0 l = List.filter l ~f:Tezos_node.(fun n -> n.id <> node_0.id) in
  List_sequential.iter (except_0 mesh_nodes)
    ~f:(Helpers.restart_node state ~client_exec)
  >>= fun () ->
  pause EF.[af "All nodes restarted Except 0"]
  >>= fun () ->
  Test_scenario.Queries.wait_for_all_levels_to_be state
    ~attempts:default_attempts ~seconds:8. (except_0 all_nodes)
    (`At_least (starting_level + number_of_lonely_bakes))
  >>= fun () ->
  Helpers.restart_node state ~client_exec node_0
  >>= fun () ->
  pause EF.[af "Restarted 0"]
  >>= fun () ->
  Helpers.wait_for state ~attempts:default_attempts ~seconds:8. (fun _ ->
      List.fold ~init:(return false) accuser_nodes ~f:(fun prev_m node ->
          prev_m
          >>= fun prev ->
          let client = Tezos_client.of_node node ~exec:client_exec in
          Tezos_client.mempool_has_operation state ~client
            ~kind:"double_baking_evidence"
          >>= fun client_result -> return (client_result || prev) )
      >>= function
      | true -> return (`Done ())
      | false ->
          return
            (`Not_done
              (sprintf "Waiting for accusation to show up in the mempool")) )
  >>= fun () ->
  Tezos_client.Keyed.bake state baker_2
    (sprintf "all at lvl %d" (starting_level + number_of_lonely_bakes + 1))
  >>= fun () ->
  Helpers.wait_for state ~attempts:10 ~seconds:4. (fun _ ->
      let level = starting_level + number_of_lonely_bakes + 2 in
      Tezos_client.block_has_operation state ~client:client_2 ~level
        ~kind:"double_baking_evidence"
      >>= function
      | true -> return (`Done ())
      | false ->
          return
            (`Not_done
              (sprintf "Waiting for accusation to show up in block %d" level))
  )
  >>= fun () ->
  pause
    EF.
      [ af "One more baking (level should include accusation)"
      ; af "All nodes reaching level %d"
          (starting_level + number_of_lonely_bakes + 2) ]
  >>= fun () ->
  Tezos_client.Keyed.bake state baker_1 "a couple more"
  >>= fun () ->
  Test_scenario.Queries.wait_for_all_levels_to_be state
    ~attempts:default_attempts ~seconds:8. all_nodes
    (`At_least (starting_level + number_of_lonely_bakes + 1))

let cmd ~pp_error () =
  let open Cmdliner in
  let open Term in
  let pf fmt = ksprintf (fun s -> `P s) fmt in
  let tests =
    let test variant name title man = (variant, name, title, man) in
    [ test `With_accusers "with-accusers" "Network With Accusers"
        (pf
           "This test builds a network with 3 interconnected meshes: Main, \
            Intermediate, and Accuser.")
    ; test `Simple_double_baking "simple-double-baking"
        "Simple Network With Manual Double Baking Accusation"
        (pf
           "This test builds a very simple 3-piece network, makes a baker \
            double bake and $(i,manually) inserts a double-baking accusation.")
    ; test `Simple_double_endorsing "simple-double-endorsing"
        "Simple Network With Manual Double Endorsing Accusation"
        (pf
           "This test builds a very simple 3-piece network, makes a baker \
            double endorse and $(i,manually) inserts a double-baking \
            accusation.") ]
  in
  Test_command_line.Run_command.make ~pp_error
    ( pure
        (fun test
        base_port
        (`Starting_level starting_level)
        bnod
        bcli
        accex
        kiln
        state
        ->
          let actual_test =
            match test with
            | `With_accusers -> with_accusers ~state bnod accex bcli ~base_port
            | `Simple_double_baking ->
                simple_double_baking ~state bnod bcli ~base_port ?kiln
                  ~starting_level
            | `Simple_double_endorsing ->
                simple_double_endorsement ~state bnod bcli ~base_port ?kiln
                  ~starting_level
          in
          (state, Interactive_test.Pauser.run_test ~pp_error state actual_test)
      )
    $ Arg.(
        required
          (pos 0
             (some (enum (List.map tests ~f:(fun (v, n, _, _) -> (n, v)))))
             None
             (info [] ~docv:"TEST-NAME" ~doc:"Choose which test to run.")))
    $ Arg.(
        value & opt int 30_000
        & info ["base-port"] ~doc:"Base port number to build upon.")
    $ Arg.(
        pure (fun l -> `Starting_level l)
        $ value
            (opt int 5
               (info ["starting-level"]
                  ~doc:
                    "Initial block-level to reach before actually starting \
                     the test.")))
    $ Tezos_executable.cli_term `Node "tezos"
    $ Tezos_executable.cli_term `Client "tezos"
    $ Tezos_executable.cli_term `Accuser "tezos"
    $ Kiln.cli_term ()
    $ Test_command_line.cli_state ~name:"accusing" () )
    (let doc = "Sandbox networks which record double-bakings." in
     let man : Manpage.block list =
       [ `S "ACCUSATION TESTS"
       ; pf
           "This command provides %d tests which use network sandboxes to \
            make double-bakings and double-endorsements happen."
           (List.length tests)
       ; `Blocks
           (List.map tests ~f:(fun (_, n, tit, m) ->
                `Blocks [pf "* $(b,`%s`): $(i,%s)." n tit; `Noblank; m] )) ]
     in
     info ~man ~doc "accusations")
