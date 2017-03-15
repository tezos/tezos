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

let make_context () =
  let buffers = Hashtbl.create 50 in
  Hashtbl.add buffers "stdout" (Buffer.create 1000) ;
  Hashtbl.add buffers "stderr" (Buffer.create 1000) ;
  let log channel msg =
    let buffer =
      try Hashtbl.find buffers channel with
        Not_found ->
          let buffer = Buffer.create 1000 in
          Hashtbl.add buffers channel buffer ;
          buffer in
    Buffer.add_string buffer msg ;
    Buffer.add_char buffer '\n' ;
    Lwt.return () in
  Client_commands.make_context log,
  (fun () ->
     Hashtbl.fold
       (fun channel buffer acc ->
          (channel, Buffer.contents buffer) :: acc)
       buffers [])

let block_protocol cctxt block =
  Lwt.catch
    (fun () ->
       Client_node_rpcs.Blocks.protocol cctxt block)
    (fun _ ->
       cctxt.Client_commands.message
         "\n\
          The connection to the RPC server failed, \
          using the default protocol version.\n" >>= fun () ->
       Lwt.return Client_alpha.Client_proto_main.protocol)

let eval_command argv =
  let cctxt, result = make_context () in
  Lwt.catch
    (fun () ->
       Client_config.preparse_args argv cctxt >>= fun config ->
       let cctxt = { cctxt with config } in
       block_protocol cctxt Client_commands.(cctxt.config.block)
       >>= fun version ->
       let commands =
         Client_generic_rpcs.commands @
         Client_keys.commands () @
         Client_protocols.commands () @
         Client_helpers.commands () @
         Client_commands.commands_for_version version in
       Client_config.parse_args ~version
         (Cli_entries.usage ~commands)
         (Cli_entries.inline_dispatch commands)
         argv cctxt >>= fun (command, config) ->
       command Client_commands.({ cctxt with config }) >>= fun () ->
       Lwt.return (Ok (result ())))
    (fun exn ->
       let msg = match exn with
         | Arg.Help help ->
             Format.asprintf "%s%!" help
         | Arg.Bad help ->
             Format.asprintf "%s%!" help
         | Cli_entries.Command_not_found ->
             Format.asprintf "Unknown command, try `-help`.\n%!"
         | Client_commands.Version_not_found ->
             Format.asprintf "Unknown protocol version, try `list versions`.\n%!"
         | Cli_entries.Bad_argument (idx, _n, v) ->
             Format.asprintf "There's a problem with argument %d, %s.\n%!" idx v
         | Cli_entries.Command_failed message ->
             Format.asprintf "Command failed, %s.\n%!" message
         | Failure msg ->
             Format.asprintf "Fatal error: %s\n%!" msg
         | exn ->
             Format.asprintf "Fatal internal error: %s\n%!" (Printexc.to_string exn) in
       let result =
         result () in
       let stderr =
         List.assoc "stderr" result in
       let stderr =
         if stderr = ""
         || String.get stderr (String.length stderr - 1) = '\n' then
           msg
         else
           stderr ^ "\n" ^ msg in
       let result =
         ("stderr", stderr)::
         List.filter (fun (n, _) -> n <> "stderr") result in
       Lwt.return (Error result))

module ConnectionMap = Map.Make(Cohttp.Connection)

exception Invalid_method
exception Cannot_parse_body of string

let root =
  let input, output =
    let open Data_encoding in
    (obj1 (req "command" string)),
    (obj1 (req "outputs" (assoc string))) in
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
         Client_node_rpcs.Blocks.protocol Client_commands.ignore_context block >>= fun version ->
         let directory = Webclient_version.find_contextual_services version in
         let directory = RPC.map (fun ((), block) -> block) directory in
         Lwt.return directory) in
  root

let find_static_file path_str =
  let path = OCamlRes.Path.of_string path_str in
  let index path = match path with
    | [], None -> "text/html", ([], Some ("index", Some "html"))
    | oth -> Magic_mime.lookup path_str, oth in
  match path with
  | ("block" :: block :: path, file) ->
      let content_type, path = index (path, file) in
      (match Node_rpc_services.Blocks.parse_block block with
       | Ok block ->
           block_protocol Client_commands.ignore_context block >>= fun version ->
           Lwt.return
             (try
                let root =
                  Webclient_version.find_contextual_static_files version in
                Some (content_type, OCamlRes.Res.find path root)
              with Not_found -> None)
       | Error _ -> Lwt.return None)
  | _ ->
      Lwt.return
        (try
           let content_type, path = index path in
           Some (content_type, OCamlRes.Res.find path Webclient_static.root)
         with Not_found -> None)

let http_proxy mode =
  let pre_hook path =
    find_static_file path >>= function
    | Some (content_type, body) ->
        Lwt.return
          (Some content_type,
            { RPC.Answer.code = 200 ; body = RPC.Answer.Single body })
    | None ->
        Lwt.return
          (None, { RPC.Answer.code = 404 ; body = RPC.Answer.Empty }) in
  let post_hook _ =
    (find_static_file "not_found.html" >>= function
      | Some (content_type, body) ->
          Lwt.return (Some content_type, RPC.Answer.Single body)
      | None ->
          Lwt.return (None, RPC.Answer.Empty)) >>= fun (content_type, body) ->
    Lwt.return (content_type, { RPC.Answer.code = 404 ; body }) in
  RPC_server.launch ~pre_hook ~post_hook mode root [] []

let webclient_args cfg =
  let open Client_commands in
  [
    "-web-port", Arg.Int (fun x -> cfg := { !cfg with web_port = x }),
      "The TCP port to point the web browser to.\n\
       default: " ^ string_of_int Client_commands.(default_cfg.web_port);
  ]

(* Where all the user friendliness starts *)
let () =
  Pervasives.exit @@ Lwt_main.run
    (Lwt.catch
       (fun () ->
          Client_config.parse_args
            ~extra:webclient_args
            (Cli_entries.usage ~commands: [])
            (fun () -> function
               | `Arg arg -> raise (Arg.Bad ("unexpected argument " ^ arg))
               | `End -> `Res (fun () -> Lwt.return ()))
            Sys.argv Client_commands.ignore_context
          >>= fun (_no_command, config) ->
          Random.self_init () ;
          Sodium.Random.stir () ;
          (* TODO: add TLS? *)
          http_proxy (`TCP (`Port Client_commands.(config.web_port)))
          >>= fun _server ->
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
