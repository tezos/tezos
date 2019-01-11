open Tezos_network_sandbox
open Internal_pervasives
open Console

let run state node_exec client_exec () =
  Test_scenario.network_with_protocol ~size:2 state ~node_exec ~client_exec
  >>= fun (nodes, protocol) ->
  match nodes with
  | [] | [_] | _ :: _ :: _ :: _ -> assert false
  | [n1; n2] ->
      let c1 = Tezos_client.of_node ~exec:client_exec n1 in
      let c2 = Tezos_client.of_node ~exec:client_exec n2 in
      (* TODO: helpers for
         - injecting an op
         - displaying the mempool
         - setting filter plugin config

         TODO: non-interactive test for propagation
         TODO: commands for interactivea use *)
      Pervasives.ignore c1 ;
      Pervasives.ignore c2 ;
      return ()
      >>= fun () ->
      let commands = Interactive_test.Commands.all_defaults state ~nodes in
      Prompt.command state ~commands
      >>= fun () -> Running_processes.wait_all state

let cmd ~pp_error () =
  let open Cmdliner in
  let open Term in
  Test_command_line.Run_command.make ~pp_error
    ( pure (fun bnod bcli state -> (state, run state bnod bcli))
    $ Tezos_executable.cli_term `Node "tezos"
    $ Tezos_executable.cli_term `Client "tezos"
    $ Test_command_line.cli_state ~name:"prevalidation" () )
    (info ~doc:"Work-in-progress." "prevalidation")
