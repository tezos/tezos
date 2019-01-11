open Tezos_network_sandbox
open Internal_pervasives
open Console

let run state ~protocol ~size ~base_port ?kiln node_exec client_exec baker_exec
    endorser_exec accuser_exec () =
  Test_scenario.network_with_protocol ~protocol ~size ~base_port state
    ~node_exec ~client_exec
  >>= fun (nodes, protocol) ->
  Tezos_client.rpc state
    ~client:(Tezos_client.of_node (List.hd_exn nodes) ~exec:client_exec)
    `Get ~path:"/chains/main/chain_id"
  >>= fun chain_id_json ->
  let network_id =
    match chain_id_json with `String s -> s | _ -> assert false
  in
  Asynchronous_result.map_option kiln ~f:(fun k ->
      Kiln.start state ~network_id k
        ~bakers:
          (List.map protocol.Tezos_protocol.bootstrap_accounts
             ~f:(fun (account, _) ->
               Tezos_protocol.Account.(name account, pubkey_hash account) ))
        ~node_uris:
          (List.map nodes ~f:(fun {Tezos_node.rpc_port; _} ->
               sprintf "http://localhost:%d" rpc_port ))
      >>= fun (pg, kiln) -> return () )
  >>= fun (_ : unit option) ->
  let accusers =
    List.map nodes ~f:(fun node ->
        let client = Tezos_client.of_node node ~exec:client_exec in
        Tezos_daemon.accuser_of_node ~exec:accuser_exec ~client node )
  in
  List_sequential.iter accusers ~f:(fun acc ->
      Running_processes.start state (Tezos_daemon.process acc ~state)
      >>= fun {process; lwt} -> return () )
  >>= fun () ->
  let keys_and_daemons =
    let pick_a_node_and_client idx =
      match List.nth nodes ((1 + idx) mod List.length nodes) with
      | Some node -> (node, Tezos_client.of_node node ~exec:client_exec)
      | None -> assert false
    in
    Tezos_protocol.bootstrap_accounts protocol
    |> List.mapi ~f:(fun idx acc ->
           let node, client = pick_a_node_and_client idx in
           let key = Tezos_protocol.Account.name acc in
           ( acc
           , client
           , [ Tezos_daemon.baker_of_node ~exec:baker_exec ~client node ~key
             ; Tezos_daemon.endorser_of_node ~exec:endorser_exec ~client node
                 ~key ] ) )
  in
  List_sequential.iter keys_and_daemons ~f:(fun (acc, client, daemons) ->
      Tezos_client.bootstrapped ~state client
      >>= fun () ->
      let key, priv = Tezos_protocol.Account.(name acc, private_key acc) in
      Tezos_client.import_secret_key ~state client key priv
      >>= fun () ->
      say state
        EF.(
          desc_list
            (haf "Registration-as-delegate:")
            [ desc (af "Client:") (af "%S" client.Tezos_client.id)
            ; desc (af "Key:") (af "%S" key) ])
      >>= fun () ->
      Tezos_client.register_as_delegate ~state client key
      >>= fun () ->
      say state
        EF.(
          desc_list (haf "Starting daemons:")
            [ desc (af "Client:") (af "%S" client.Tezos_client.id)
            ; desc (af "Key:") (af "%S" key) ])
      >>= fun () ->
      List_sequential.iter daemons ~f:(fun daemon ->
          Running_processes.start state (Tezos_daemon.process daemon ~state)
          >>= fun {process; lwt} -> return () ) )
  >>= fun () ->
  Prompt.(
    command state
      ~commands:
        Interactive_test.Commands.(
          all_defaults state ~nodes
          @ [ secret_keys state ~protocol
            ; arbitrary_command_on_clients state
                ~clients:
                  (List.map nodes ~f:(Tezos_client.of_node ~exec:client_exec))
            ]))

let cmd ~pp_error () =
  let open Cmdliner in
  let open Term in
  Test_command_line.Run_command.make ~pp_error
    ( pure (fun size base_port protocol bnod bcli bak endo accu kiln state ->
          let actual_test =
            run state ~size ~base_port ~protocol bnod bcli bak endo accu ?kiln
          in
          (state, Interactive_test.Pauser.run_test ~pp_error state actual_test)
      )
    $ Arg.(
        value & opt int 5
        & info ["size"; "S"] ~doc:"Set the size of the network.")
    $ Arg.(
        value & opt int 20_000
        & info ["base-port"; "P"] ~doc:"Base port number to build upon.")
    $ Tezos_protocol.cli_term ()
    $ Tezos_executable.cli_term `Node "tezos"
    $ Tezos_executable.cli_term `Client "tezos"
    $ Tezos_executable.cli_term `Baker "tezos"
    $ Tezos_executable.cli_term `Endorser "tezos"
    $ Tezos_executable.cli_term `Accuser "tezos"
    $ Kiln.cli_term ()
    $ Test_command_line.cli_state ~name:"mininet" () )
    (let doc = "Small network sandbox with bakers, endorsers, and accusers." in
     let man : Manpage.block list =
       [ `P
           "This test builds a small sandbox network, start various daemons, \
            and then gives the user an interactive command prompt to inspect \
            the network." ]
     in
     info "mini-network" ~man ~doc)
