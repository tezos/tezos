open Internal_pervasives

type t = {run: [`Docker of string]; port: int; postgres_port: int}

let make ~run ~port ~postgres_port = {run; port; postgres_port}
let default_docker_image = "obsidiansystems/tezos-bake-monitor:0.4.0"

let default =
  make ~run:(`Docker default_docker_image) ~port:8086 ~postgres_port:4_532

let start ?(network_id = "zeronet") state
    {run= `Docker image; port; postgres_port} ~node_uris ~bakers =
  let name nonbase = sprintf "flxts-%s" nonbase in
  let pg_password = Tezos_protocol.Key.Of_name.pubkey "pg-password" in
  let pg_port = postgres_port in
  let kiln_port = port in
  let pg =
    Running_processes.Process.docker_run (name "kiln-postgres-db")
      ~image:"postgres"
      ~options:
        [ "-p"; sprintf "%d:5432" pg_port; "-e"
        ; sprintf "POSTGRES_PASSWORD=%s" pg_password ]
      ~args:[]
  in
  Running_processes.start state pg
  >>= fun pg_process ->
  Helpers.wait_for state ~attempts:20 ~seconds:8. (fun attempt ->
      Running_processes.run_cmdf state
        "docker run --rm -e PGPASSWORD=%s --network host -it postgres psql -h \
         localhost -p %d -U postgres -w -c '\\l'"
        pg_password pg_port
      >>= fun res ->
      Console.display_errors_of_command state res
      >>= function
      | true -> return (`Done ())
      | false ->
          return
            (`Not_done
              (sprintf "Waiting for postgres to be ready (%d)" attempt)) )
  >>= fun () ->
  (* We need to use /tmp and not the root-path because of Docker access rights. *)
  let tmp = "/tmp" // sprintf "kiln-config-%d" port in
  Running_processes.run_cmdf state
    "rm -fr %s ; mkdir -p %s/config ; chmod -R 777 %s" tmp tmp tmp
  >>= fun _ ->
  Lwt_exception.catch
    (fun () ->
      Lwt_io.with_file ~perm:0o777 ~mode:Lwt_io.output (tmp // "config/loggers")
        (fun out ->
          Lwt_io.write out
            {json|[
{ "logger":{"Stderr":{}} , "filters": { "SQL":"Error" , "":"Info"}},
{ "logger":{"File":{"file":"/var/run/bake-monitor/kiln.log"}}, "filters": { "": "Debug" } }
]|json}
      ) )
    ()
  >>= fun () ->
  Running_processes.run_cmdf state " chmod -R 777 %s" tmp
  >>= fun _ ->
  let kiln =
    Running_processes.Process.docker_run (name "kiln-backend") ~image
      ~options:
        ["--network"; "host"; "-v"; sprintf "%s:/var/run/bake-monitor" tmp]
      ~args:
        [ sprintf
            "--pg-connection=host=localhost port=%d dbname=postgres \
             user=postgres password=%s"
            pg_port pg_password
        ; "--nodes"
        ; String.concat ~sep:"," node_uris
        ; "--network"; network_id; "--"; "--port"; Int.to_string kiln_port ]
  in
  Running_processes.start state kiln
  >>= fun kiln_process ->
  Console.say state
    EF.(
      wf "Kiln was started with nodes: %s, and network-id: %s"
        (List.map node_uris ~f:(sprintf "`%s`") |> String.concat ~sep:", ")
        network_id)
  >>= fun () ->
  ( match bakers with
  | [] -> return ()
  | _ ->
      Interactive_test.Pauser.generic state ~force:true
        EF.
          [ wf "Importing bakers in Kiln."
          ; wf
              "You should open <http://localhost:%d> and import the following \
               bakers:"
              kiln_port
          ; list
              (List.map bakers ~f:(fun (n, pkh) -> af "Baker: `%s` -> %s" n pkh))
          ] )
  >>= fun () -> return (pg_process, kiln_process)

let cli_term () =
  let open Cmdliner in
  Term.(
    pure (fun run port postgres_port -> function
      | true -> Some (make ~run ~postgres_port ~port) | false -> None )
    $ Arg.(
        let doc = "Set the Kiln docker image." in
        pure (fun docker_image -> `Docker docker_image)
        $ value
            (opt string default_docker_image (info ["kiln-docker-image"] ~doc)))
    $ Arg.(
        value
          (opt int default.port (info ["kiln-port"] ~doc:"Set the kiln port.")))
    $ Arg.(
        value
          (opt int default.postgres_port
             (info ["kiln-pg-port"] ~doc:"Set the Postgres port for Kiln.")))
    $ Arg.(
        value
          (flag
             (info ["with-kiln"]
                ~doc:
                  "Add Kiln to the network (may make the test partially \
                   interactive)."))))
