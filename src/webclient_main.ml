(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

(* Tezos Command line interface - Main Program *)

open Lwt.Infix
open Logging.Webclient

let with_cli_entries_logging =
  let startup =
    CalendarLib.Printer.Precise_Calendar.sprint
      "%Y-%m-%dT%H:%M:%SZ"
      (CalendarLib.Calendar.Precise.now ()) in
  let stdout = Buffer.create 1000 in
  let stderr = Buffer.create 1000 in
  let log channel msg = match channel with
    | "stdout" ->
        Buffer.add_string stdout msg ;
        Lwt.return ()
    | "stderr" ->
        Buffer.add_string stderr msg ;
        Lwt.return ()
    | log ->
        Lwt_utils.create_dir Client_config.(base_dir#get // "webclient_logs" // log) >>= fun () ->
        Lwt_io.with_file
          ~flags: Unix.[ O_APPEND ; O_CREAT ; O_WRONLY ]
          ~mode: Lwt_io.Output
          Client_config.(base_dir#get // "webclient_logs" // log // startup)
          (fun chan -> Lwt_io.write chan msg) in
  Cli_entries.log_hook := Some log ;
  let global_cli_entries_mutex = Lwt_mutex.create () in
  (fun callback ->
     Lwt_mutex.with_lock
       global_cli_entries_mutex
       (fun () ->
          Buffer.clear stdout ;
          Buffer.clear stderr ;
          Lwt.catch
            (fun () ->
               callback () >>= fun result ->
               Lwt.return
                 (Ok (result,
                      Buffer.contents stdout,
                      Buffer.contents stderr)))
            (fun exn ->
               Lwt.return
                 (Error (exn,
                         Buffer.contents stdout,
                         Buffer.contents stderr)))))

let block_protocol block =
  Lwt.catch
    (fun () ->
       Client_node_rpcs.Blocks.protocol block)
    (fun _ ->
       Cli_entries.message "\n\
                            The connection to the RPC server failed, \
                            using the default protocol version.\n" >>= fun () ->
       Lwt.return Client_bootstrap.Client_proto_main.protocol)

let eval_command argv =
  with_cli_entries_logging
    (fun () ->
       Client_config.preparse_args argv >>= fun block ->
       block_protocol block >>= fun version ->
       let commands =
         Client_generic_rpcs.commands @
         Client_keys.commands () @
         Client_protocols.commands () @
         Client_helpers.commands () @
         Client_version.commands_for_version version in
       Client_config.parse_args ~version
         (Cli_entries.usage commands)
         (Cli_entries.inline_dispatch commands)
         argv >>= fun command ->
       command ()) >>= function
  | Ok ((), stdout, _stderr) ->
      Lwt.return (Ok stdout)
  | Error (exn, stdout, stderr) ->
      let msg = match exn with
        | Arg.Help help ->
            Format.asprintf "%s%!" help
        | Arg.Bad help ->
            Format.asprintf "%s%!" help
        | Cli_entries.Command_not_found ->
            Format.asprintf "Unkonwn command, try `-help`.\n%!"
        | Client_version.Version_not_found ->
            Format.asprintf "Unkonwn protocol version, try `list versions`.\n%!"
        | Cli_entries.Bad_argument (idx, _n, v) ->
            Format.asprintf "There's a problem with argument %d, %s.\n%!" idx v
        | Cli_entries.Command_failed message ->
            Format.asprintf "Command failed, %s.\n%!" message
        | Failure msg ->
            Format.asprintf "Fatal error: %s\n%!" msg
        | exn ->
            Format.asprintf "Fatal internal error: %s\n%!" (Printexc.to_string exn) in
      let stderr =
        if stdout = ""
        || String.get stdout (String.length stderr - 1) = '\n' then
          stdout ^ stderr
        else
          stdout ^ "\n" ^ stderr in
      let stderr =
        if stderr = ""
        || String.get stderr (String.length stderr - 1) = '\n' then
          msg
        else
          stderr ^ "\n" ^ msg in
      Lwt.return (Error stderr)

module ConnectionMap = Map.Make(Cohttp.Connection)

exception Invalid_method
exception Cannot_parse_body of string

let root =
  let input, output =
    let open Data_encoding in
    (obj1 (req "command" string)),
    (obj1 (req "output" string)) in
  let root =
    RPC.empty in
  let root =
    RPC.register0 root
      (RPC.service ~input ~output RPC.Path.(root / "command"))
      (fun command ->
         let argv = Array.of_list (Utils.split ' ' command) in
         eval_command argv >>= function
         | Ok output | Error output ->
             RPC.Answer.return output) in
  let root =
    RPC.register_dynamic_directory1 root
      RPC.Path.(root / "block" /: Node_rpc_services.Blocks.blocks_arg)
      (fun block ->
         Client_node_rpcs.Blocks.protocol block >>= fun version ->
         let directory = Webclient_version.find_contextual_services version in
         let directory = RPC.map (fun ((), block) -> block) directory in
         Lwt.return directory) in
  root

let find_static_file path =
  let path = OCamlRes.Path.of_string path in
  let index path = match path with
    | ([], None) -> ([], Some ("index", Some "html"))
    | oth -> oth in
  match path with
  | ("block" :: block :: path, file) ->
      let path = index (path, file) in
      (match Node_rpc_services.Blocks.parse_block block with
       | Ok block ->
           block_protocol block >>= fun version ->
           Lwt.return
             (try
                let root =
                  Webclient_version.find_contextual_static_files version in
                Some (OCamlRes.Res.find path root)
              with Not_found -> None)
       | Error _ -> Lwt.return None)
  | _ ->
      Lwt.return
        (try
           Some (OCamlRes.Res.find (index path) Webclient_static.root)
         with Not_found -> None)

let http_proxy port =
  let pre_hook path =
    find_static_file path >>= function
    | Some body ->
        Lwt.return { RPC.Answer.code = 200 ; body = RPC.Answer.Single body }
    | None ->
        Lwt.return { RPC.Answer.code = 404 ; body = RPC.Answer.Empty } in
  let post_hook _ =
    (find_static_file "not_found.html" >>= function
      | Some body ->
          Lwt.return (RPC.Answer.Single body)
      | None ->
          Lwt.return (RPC.Answer.Empty)) >>= fun body ->
    Lwt.return { RPC.Answer.code = 404 ; body } in
  RPC_server.launch ~pre_hook ~post_hook port root

let web_port = Client_config.in_both_groups @@
  new Config_file.int_cp [ "web" ; "port" ] 8080
    "The TCP port to point the web browser to."

(* Where all the user friendliness starts *)
let () =
  Pervasives.exit @@ Lwt_main.run
    (Lwt.catch
       (fun () ->
          Client_config.parse_args
            (Cli_entries.usage [])
            (fun () -> function
               | `Arg arg -> raise (Arg.Bad ("unexpected argument " ^ arg))
               | `End -> `Res (fun () -> Lwt.return ()))
            Sys.argv >>= fun _no_command ->
          Random.self_init () ;
          Sodium.Random.stir () ;
          http_proxy web_port#get >>= fun _server ->
          fst (Lwt.wait ()))
       (function
         | Arg.Help help ->
             Format.eprintf "%s%!" help ;
             Lwt.return 0
         | Arg.Bad help ->
             Format.eprintf "%s%!" help ;
             Lwt.return 1
         | Failure msg ->
             Format.eprintf "Fatal error: %s\n%!" msg ;
             Lwt.return 1
         | exn ->
             Format.eprintf "Fatal internal error: %s\n%!" (Printexc.to_string exn) ;
             Lwt.return 1))
