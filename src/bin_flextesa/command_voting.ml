(* Semi-interactive test for voting *)
open Tezos_network_sandbox
open Internal_pervasives
module Counter_log = Helpers.Counter_log

let ledger_prompt_notice state ef =
  Console.say state
    EF.(
      desc (shout "Ledger-prompt")
        (list [ef; wf "Please hit “✔” on the ledger."]))

let setup_baking_ledger state uri ~client =
  Interactive_test.Pauser.generic state
    EF.
      [ wf "Setting up the ledger device %S" uri
      ; haf
          "Please make sure the ledger is on the Baking app and quit (`q`) \
           this prompt to continue." ]
    ~force:true
  >>= fun () ->
  let key_name = "ledgered" in
  let baker = Tezos_client.Keyed.make client ~key_name ~secret_key:uri in
  ledger_prompt_notice state
    EF.(
      wf
        "Importing %S in client `%s`. The ledger should be prompting for \
         acknowledgment to provide the public key."
        uri client.Tezos_client.id)
  >>= fun () ->
  Tezos_client.Keyed.initialize state baker
  >>= fun _ ->
  ledger_prompt_notice state
    EF.(
      wf
        "Setting up %S for baking. The ledger should be showing the setup \
         parameters (Address, Main chain, HWMs)."
        uri)
  >>= fun () ->
  Tezos_client.successful_client_cmd state ~client
    [ "setup"; "ledger"; "to"; "bake"; "for"; key_name; "--main-hwm"; "0"
    ; "--test-hwm"; "0" ]
  >>= fun _ -> return baker

let failf fmt = ksprintf (fun s -> fail (`Scenario_error s)) fmt

type voting_period =
                    Tezos_client_alpha.Proto_alpha.Alpha_context.Voting_period
                    .kind =
  | Proposal
  | Testing_vote
  | Testing
  | Promotion_vote

let voting_period_to_string (p : voting_period) =
  match
    Tezos_data_encoding.Data_encoding.Json.construct
      Tezos_client_alpha.Proto_alpha.Alpha_context.Voting_period.kind_encoding
      p
  with
  | `String s -> s
  | other -> assert false

let transfer state ~client ~src ~dst ~amount =
  Tezos_client.successful_client_cmd state ~client
    [ "--wait"; "none"; "transfer"; sprintf "%d" amount; "from"; src; "to"; dst
    ; "--fee"; "0.05"; "--burn-cap"; "0.3" ]

let bake_until_voting_period ?keep_alive_delegate state ~baker ~attempts period
    =
  let client = baker.Tezos_client.Keyed.client in
  let period_name = voting_period_to_string period in
  Helpers.wait_for state ~attempts ~seconds:0.5 (fun nth ->
      Tezos_client.rpc state ~client `Get
        ~path:"/chains/main/blocks/head/votes/current_period_kind"
      >>= function
      | `String p when p = period_name -> return (`Done (nth - 1))
      | other ->
          Asynchronous_result.map_option keep_alive_delegate ~f:(fun dst ->
              transfer state ~client ~amount:1
                ~src:baker.Tezos_client.Keyed.key_name ~dst
              >>= fun res -> return () )
          >>= fun _ ->
          ksprintf
            (Tezos_client.Keyed.bake state baker)
            "Baker %s bakes %d/%d waiting for %S voting period" client.id nth
            attempts period_name
          >>= fun () ->
          return (`Not_done (sprintf "Waiting for %S period" period_name)) )

let run state ~demo_path ~node_exec ~client_exec ~admin_exec ~size ~base_port
    ~serialize_proposals ?with_ledger () =
  let default_attempts = 35 in
  Helpers.clear_root state
  >>= fun () ->
  Interactive_test.Pauser.generic state
    EF.[af "Ready to start"; af "Root path deleted."]
  >>= fun () ->
  let protocol, baker_0_account, baker_0_balance =
    let open Tezos_protocol in
    let d = default () in
    let baker = List.nth_exn d.bootstrap_accounts 0 in
    ( { d with
        time_between_blocks= [1; 0]
      ; bootstrap_accounts=
          List.map d.bootstrap_accounts ~f:(fun (n, v) ->
              if fst baker = n then (n, v) else (n, 1_000) ) }
    , fst baker
    , snd baker )
  in
  Test_scenario.network_with_protocol ~protocol ~size ~base_port state
    ~node_exec ~client_exec
  >>= fun (nodes, protocol) ->
  let make_admin = Tezos_admin_client.of_client ~exec:admin_exec in
  Interactive_test.Pauser.add_commands state
    Interactive_test.Commands.(
      all_defaults state ~nodes
      @ [ secret_keys state ~protocol
        ; Log_recorder.Operations.show_all state
        ; arbitrary_command_on_clients state ~command_names:["all-clients"]
            ~make_admin
            ~clients:
              (List.map nodes ~f:(Tezos_client.of_node ~exec:client_exec)) ]) ;
  Interactive_test.Pauser.generic state EF.[af "About to really start playing"]
  >>= fun () ->
  let client n =
    Tezos_client.of_node ~exec:client_exec (List.nth_exn nodes n)
  in
  let baker_0 =
    Tezos_client.Keyed.make (client 0) ~key_name:"baker-0"
      ~secret_key:(Tezos_protocol.Account.private_key baker_0_account)
  in
  Tezos_client.Keyed.initialize state baker_0
  >>= fun _ ->
  let level_counter = Counter_log.create () in
  let first_bakes = 5 in
  Loop.n_times first_bakes (fun nth ->
      ksprintf (Tezos_client.Keyed.bake state baker_0) "initial-bake %d" nth )
  >>= fun () ->
  let initial_level = first_bakes + 1 in
  Counter_log.add level_counter "initial_level" initial_level ;
  ( match with_ledger with
  | None ->
      Console.say state EF.(wf "No ledger.")
      >>= fun () ->
      let account = Tezos_protocol.Account.of_name "special-baker" in
      let baker =
        Tezos_client.Keyed.make (client 0)
          ~key_name:(Tezos_protocol.Account.name account)
          ~secret_key:(Tezos_protocol.Account.private_key account)
      in
      Tezos_client.Keyed.initialize state baker >>= fun _ -> return baker
  | Some uri -> setup_baking_ledger state ~client:(client 0) uri )
  >>= fun special_baker ->
  Interactive_test.Pauser.add_commands state
    Interactive_test.Commands.
      [ arbitrary_command_on_clients state ~command_names:["baker"] ~make_admin
          ~clients:[special_baker.Tezos_client.Keyed.client] ] ;
  transfer state (* Tezos_client.successful_client_cmd state *)
    ~client:(client 0)
    ~amount:(baker_0_balance / 2_000_000)
    ~src:"baker-0" ~dst:special_baker.Tezos_client.Keyed.key_name
  >>= fun res ->
  Console.say state
    EF.(
      desc
        (wf "Successful transfer baker-0 -> special:")
        (ocaml_string_list res#out))
  >>= fun () ->
  let after_transfer_bakes = 2 in
  Loop.n_times after_transfer_bakes (fun nth ->
      ksprintf
        (Tezos_client.Keyed.bake state baker_0)
        "after-transfer-bake %d" nth )
  >>= fun () ->
  Counter_log.add level_counter "after-transfer-bakes" after_transfer_bakes ;
  Test_scenario.Queries.wait_for_all_levels_to_be state
    ~attempts:default_attempts ~seconds:8. nodes
    (`At_least (Counter_log.sum level_counter))
  >>= fun () ->
  Asynchronous_result.map_option with_ledger ~f:(fun _ ->
      ledger_prompt_notice state EF.(wf "Registering as delegate.") )
  >>= fun (_ : unit option) ->
  Tezos_client.successful_client_cmd state ~client:(client 0)
    [ "--wait"; "none"; "register"; "key"
    ; special_baker.Tezos_client.Keyed.key_name; "as"; "delegate"; "--fee"
    ; "0.5" ]
  >>= fun _ ->
  let activation_bakes =
    let open Tezos_protocol in
    protocol.blocks_per_cycle * (protocol.preserved_cycles + 2)
  in
  Loop.n_times activation_bakes (fun nth ->
      ksprintf
        (Tezos_client.Keyed.bake state baker_0)
        "Baking after new delegate registered: %d/%d" nth activation_bakes
      >>= fun () ->
      Tezos_client.successful_client_cmd state ~client:(client 0)
        ["rpc"; "get"; "/chains/main/blocks/head/helpers/baking_rights"]
      >>= fun res ->
      Console.say state
        EF.(
          desc (haf "Baking rights")
            (markdown_verbatim (String.concat ~sep:"\n" res#out))) )
  >>= fun () ->
  Counter_log.add level_counter "activation-bakes" activation_bakes ;
  Tezos_client.Keyed.bake state special_baker "Baked by Special Baker™"
  >>= fun () ->
  Counter_log.incr level_counter "special-baker-first-bake" ;
  let attempts =
    Tezos_protocol.(
      (* If we are right after the proposal period, we need to get to
         the next one *)
      3 * protocol.blocks_per_voting_period)
  in
  bake_until_voting_period state ~baker:special_baker ~attempts Proposal
    ~keep_alive_delegate:baker_0.key_name
  >>= fun extra_bakes_waiting_for_proposal_period ->
  Counter_log.add level_counter "wait-for-proposal-period"
    extra_bakes_waiting_for_proposal_period ;
  Test_scenario.Queries.wait_for_all_levels_to_be state
    ~attempts:default_attempts ~seconds:8. nodes
    (`At_least (Counter_log.sum level_counter))
  >>= fun () ->
  let admin_0 = Tezos_admin_client.of_client ~exec:admin_exec (client 0) in
  Tezos_admin_client.successful_command admin_0 state ["list"; "protocols"]
  >>= fun res ->
  let default_protocols = res#out in
  let make_and_inject_protocol name =
    let tmpdir = Paths.root state // sprintf "protocol-%s" name in
    Console.say state EF.(wf "Injecting protocol from %s" tmpdir)
    >>= fun () ->
    Running_processes.run_successful_cmdf state
      "cp -r %s %s && echo '(* Protocol %s *)' >> %s/main.mli"
      (Filename.quote demo_path) (Filename.quote tmpdir) name
      (Filename.quote tmpdir)
    >>= fun _ ->
    Tezos_admin_client.successful_command admin_0 state
      ["inject"; "protocol"; tmpdir]
    >>= fun res -> return ()
  in
  Loop.n_times 3 (fun nth -> make_and_inject_protocol (sprintf "The%dth" nth))
  >>= fun () ->
  Tezos_admin_client.successful_command admin_0 state ["list"; "protocols"]
  >>= fun res ->
  let after_injections_protocols = res#out in
  Interactive_test.Pauser.generic state
    EF.
      [ af "Network up"
      ; desc (haf "Protcols")
        @@ list
             (List.map after_injections_protocols ~f:(fun p ->
                  af "`%s` (%s)" p
                    ( if List.mem default_protocols p ~equal:String.equal then
                      "previously known"
                    else "injected" ) )) ]
  >>= fun () ->
  let new_protocols =
    List.filter after_injections_protocols ~f:(fun ph ->
        not (List.mem default_protocols ph ~equal:String.equal) )
  in
  Asynchronous_result.map_option with_ledger ~f:(fun _ ->
      Interactive_test.Pauser.generic state
        EF.
          [ af "About to VOTE"
          ; haf "Please switch to the Wallet app and quit (`q`) this prompt."
          ]
        ~force:true )
  >>= fun (_ : unit option) ->
  let submit_proposals baker props =
    Asynchronous_result.map_option with_ledger ~f:(fun _ ->
        ledger_prompt_notice state
          EF.(
            wf "Submitting proposal%s: %s"
              (if List.length props = 1 then "" else "s")
              (String.concat ~sep:", " props)) )
    >>= fun _ ->
    Tezos_client.successful_client_cmd state
      ~client:baker.Tezos_client.Keyed.client
      (["submit"; "proposals"; "for"; baker.key_name] @ props)
    >>= fun _ -> return ()
  in
  ( match serialize_proposals with
  | false -> submit_proposals special_baker new_protocols
  | true ->
      List_sequential.iter new_protocols ~f:(fun one ->
          submit_proposals special_baker [one] ) )
  >>= fun () ->
  let winner = List.hd_exn new_protocols in
  Tezos_client.successful_client_cmd state ~client:baker_0.client
    ["submit"; "proposals"; "for"; baker_0.key_name; winner]
  >>= fun res ->
  bake_until_voting_period state ~baker:baker_0
    ~attempts:protocol.blocks_per_voting_period Testing_vote
    ~keep_alive_delegate:special_baker.key_name
  >>= fun extra_bakes_waiting_for_testing_vote_period ->
  Counter_log.add level_counter "wait-for-testing-vote-period"
    extra_bakes_waiting_for_testing_vote_period ;
  Test_scenario.Queries.wait_for_all_levels_to_be state
    ~attempts:default_attempts ~seconds:8. nodes
    (`At_least (Counter_log.sum level_counter))
  >>= fun () ->
  Helpers.wait_for state ~attempts:default_attempts ~seconds:2. (fun nth ->
      Tezos_client.rpc state ~client:(client 1) `Get
        ~path:"/chains/main/blocks/head/votes/current_proposal"
      >>= fun current_proposal_json ->
      if current_proposal_json <> `String winner then
        return
          (`Not_done
            (sprintf "Waiting for current_proposal_json to be %s (%s)" winner
               Ezjsonm.(to_string (wrap current_proposal_json))))
      else return (`Done ()) )
  >>= fun () ->
  Tezos_client.successful_client_cmd state ~client:baker_0.client
    ["submit"; "ballot"; "for"; baker_0.key_name; winner; "yay"]
  >>= fun _ ->
  Asynchronous_result.map_option with_ledger ~f:(fun _ ->
      ledger_prompt_notice state
        EF.(wf "Submitting “Yes” ballot for %S" winner) )
  >>= fun (_ : unit option) ->
  Tezos_client.successful_client_cmd state ~client:special_baker.client
    ["submit"; "ballot"; "for"; special_baker.key_name; winner; "yay"]
  >>= fun _ ->
  Interactive_test.Pauser.generic state
    EF.[af "Ballots are in (not baked though)"]
  >>= fun () ->
  bake_until_voting_period state ~baker:baker_0
    ~attempts:(1 + protocol.blocks_per_voting_period)
    ~keep_alive_delegate:special_baker.key_name Testing
  >>= fun extra_bakes_waiting_for_testing_period ->
  Counter_log.add level_counter "wait-for-testing-period"
    extra_bakes_waiting_for_testing_period ;
  Test_scenario.Queries.wait_for_all_levels_to_be state
    ~attempts:default_attempts ~seconds:8. nodes
    (`At_least (Counter_log.sum level_counter))
  >>= fun () ->
  Helpers.wait_for state ~attempts:default_attempts ~seconds:0.3 (fun nth ->
      Tezos_client.rpc state ~client:(client 1) `Get
        ~path:"/chains/main/blocks/head/metadata"
      >>= fun metadata_json ->
      try
        match
          Jqo.field metadata_json ~k:"test_chain_status"
          |> Jqo.field ~k:"protocol"
        with
        | `String s when s = winner -> return (`Done ())
        | other ->
            return
              (`Not_done
                (sprintf "Wrong protocol: %s" Ezjsonm.(to_string (wrap other))))
      with e ->
        return
          (`Not_done
            (sprintf "Cannot get test-chain protocol: %s → %s"
               (Exn.to_string e)
               Ezjsonm.(to_string (wrap metadata_json)))) )
  >>= fun () ->
  bake_until_voting_period state ~baker:baker_0
    ~attempts:(1 + protocol.blocks_per_voting_period)
    ~keep_alive_delegate:special_baker.key_name Promotion_vote
  >>= fun extra_bakes_waiting_for_promotion_period ->
  Counter_log.add level_counter "wait-for-promotion-period"
    extra_bakes_waiting_for_promotion_period ;
  Test_scenario.Queries.wait_for_all_levels_to_be state
    ~attempts:default_attempts ~seconds:8. nodes
    (`At_least (Counter_log.sum level_counter))
  >>= fun () ->
  Interactive_test.Pauser.generic state EF.[haf "Before ballots"]
  >>= fun () ->
  Tezos_client.successful_client_cmd state ~client:baker_0.client
    ["submit"; "ballot"; "for"; baker_0.key_name; winner; "yay"]
  >>= fun _ ->
  Asynchronous_result.map_option with_ledger ~f:(fun _ ->
      ledger_prompt_notice state
        EF.(wf "Submitting “Yes” ballot for %S" winner) )
  >>= fun (_ : unit option) ->
  Tezos_client.successful_client_cmd state ~client:special_baker.client
    ["submit"; "ballot"; "for"; special_baker.key_name; winner; "yay"]
  >>= fun _ ->
  Interactive_test.Pauser.generic state
    EF.[af "Final ballot(s) are in (not baked though)"]
  >>= fun () ->
  let ballot_bakes = 1 in
  Loop.n_times ballot_bakes (fun _ ->
      Tezos_client.Keyed.bake state baker_0 "Baking the promotion vote ballots"
  )
  >>= fun () ->
  Counter_log.add level_counter "bake-the-ballots" ballot_bakes ;
  Tezos_client.successful_client_cmd state ~client:(client 0)
    ["list"; "understood"; "protocols"]
  >>= fun client_protocols_result ->
  Interactive_test.Pauser.generic state
    EF.
      [ af "Final ballot(s) are baked in."
      ; af "The client `%s` understands the following protocols: %s"
          Tezos_executable.(
            Option.value
              ~default:(default_binary client_exec)
              client_exec.binary)
          (String.concat ~sep:", " client_protocols_result#out) ]
  >>= fun () ->
  Helpers.wait_for state ~seconds:0.5
    ~attempts:(1 + protocol.blocks_per_voting_period) (fun nth ->
      let client = baker_0.client in
      Running_processes.run_successful_cmdf state
        "curl http://localhost:%d/chains/main/blocks/head/metadata" client.port
      >>= fun curl_res ->
      let json_string = curl_res#out |> String.concat ~sep:"\n" in
      let json_metadata = Ezjsonm.from_string json_string in
      match Jqo.field json_metadata ~k:"next_protocol" with
      | `String p when p = winner -> return (`Done (nth - 1))
      | other ->
          transfer state ~client ~amount:1
            ~src:baker_0.Tezos_client.Keyed.key_name
            ~dst:special_baker.Tezos_client.Keyed.key_name
          >>= fun _ ->
          ksprintf
            (Tezos_client.Keyed.bake state baker_0)
            "Baker %s bakes %d/%d waiting for next protocol: %S" client.id nth
            attempts winner
          >>= fun () ->
          return
            (`Not_done
              (sprintf "Waiting for next_protocol: %S (≠ %s)" winner
                 Ezjsonm.(to_string (wrap other)))) )
  >>= fun extra_bakes_waiting_for_next_protocol ->
  Counter_log.add level_counter "wait-for-next-protocol"
    extra_bakes_waiting_for_next_protocol ;
  ( match
      List.find client_protocols_result#out ~f:(fun prefix ->
          String.is_prefix winner ~prefix )
    with
  | Some p -> Console.say state EF.(wf "The client knows about %s" winner)
  (* 
     TODO:
     - make winner a protocol that the client knows
     - bake on test chain
     - test protocol switch
     - test ≠ not-enough-votes “failures”
 *)
  | None ->
      Console.say state EF.(wf "The client does not know about %s" winner) )
  >>= fun () ->
  Interactive_test.Pauser.generic state
    EF.
      [ haf "End of Current WIP of the Voting test: SUCCESS \\o/"
      ; desc
          (af "Estimated level: %d" (Counter_log.sum level_counter))
          (markdown_verbatim (Counter_log.to_table_string level_counter)) ]
  >>= fun () -> return ()

let cmd ~pp_error () =
  let open Cmdliner in
  let open Term in
  Test_command_line.Run_command.make ~pp_error
    ( pure
        (fun demo_path
        node_exec
        client_exec
        admin_exec
        size
        (`Base_port base_port)
        (`With_ledger with_ledger)
        (`Serialize_proposals serialize_proposals)
        state
        ->
          ( state
          , Interactive_test.Pauser.run_test state ~pp_error
              (run state ~serialize_proposals ~demo_path ~node_exec ~size
                 ~admin_exec ~base_port ~client_exec ?with_ledger) ) )
    $ Arg.(
        required
          (pos 0 (some string) None
             (info [] ~docv:"PROTOCOL-PATH"
                ~doc:
                  "The protocol to inject, e.g. `./src/bin_client/test/demo/`.")))
    $ Tezos_executable.cli_term `Node "tezos"
    $ Tezos_executable.cli_term `Client "tezos"
    $ Tezos_executable.cli_term `Admin "tezos"
    $ Arg.(value (opt int 5 (info ["size"; "S"] ~doc:"Size of the Network.")))
    $ Arg.(
        pure (fun p -> `Base_port p)
        $ value
            (opt int 46_000
               (info ["base-port"] ~doc:"Base port number to build upon.")))
    $ Arg.(
        pure (fun x -> `With_ledger x)
        $ value
            (opt (some string) None
               (info ["with-ledger"] ~docv:"ledger://..."
                  ~doc:
                    "Do the test with a Ledger Nano S as one of the \
                     bakers/voters.")))
    $ Arg.(
        pure (fun x -> `Serialize_proposals x)
        $ value
            (flag
               (info ["serialize-proposals"]
                  ~doc:
                    "Run the proposals one-by-one instead of all together \
                     (preferred by the Ledger).")))
    $ Test_command_line.cli_state ~name:"voting" () )
    (let doc = "Sandbox network with a full round of voting." in
     let man : Manpage.block list =
       let pf fmt = ksprintf (fun s -> `P s) fmt in
       [ `S "VOTING TEST"
       ; pf
           "This command provides a test which uses a network sandbox to \
            perform a full round of protocol vote and upgrade. For now, it \
            goes up to the last block before the protocol switch, baking on \
            the test chain, and with the new protocol is future work."
       ; pf
           "The test can run fully automated unless one uses the \
            `\"--with-ledger=ledger://...\"` option in which case some steps \
            have to be interactive." ]
     in
     info ~doc ~man "voting")
