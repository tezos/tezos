open Internal_pervasives

module Run_command = struct
  let or_hard_fail state main ~pp_error : unit =
    let open Asynchronous_result in
    run_application (fun () ->
        bind_on_error (main ()) ~f:(fun e ->
            transform_error
              ~f:(fun (`Lwt_exn _) _ -> die 3)
              (Console.say state
                 EF.(custom (fun fmt -> (Error.pp ~error:pp_error) fmt e)))
            >>= fun () -> die 2 ) )

  let term ~pp_error () =
    Cmdliner.Term.pure (fun (state, run) -> or_hard_fail state run ~pp_error)

  let make ~pp_error t i = Cmdliner.Term.(term ~pp_error () $ t, i)
end

let cli_state ?default_interactivity ?(disable_interactivity = false) ~name ()
    =
  let runner = Running_processes.State.make () in
  let default_root = sprintf "/tmp/%s-test" name in
  let app = sprintf "Flextesa.%s" name in
  let pauser = Interactive_test.Pauser.make [] in
  let ops = Log_recorder.Operations.make () in
  let state console paths interactivity =
    object
      method paths = paths

      method runner = runner

      method console = console

      method application_name = app

      method test_interactivity = interactivity

      method pauser = pauser

      method operations_log = ops
    end
  in
  let open Cmdliner in
  Term.(
    pure state $ Console.cli_term ()
    $ Paths.cli_term ~default_root ()
    $
    if disable_interactivity then pure `None
    else
      Interactive_test.Interactivity.cli_term ?default:default_interactivity ())
