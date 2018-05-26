(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

let default_tcp_host =
  match Sys.getenv_opt "TEZOS_SIGNER_TCP_HOST" with
  | None -> "localhost"
  | Some host -> host

let default_tcp_port =
  match Sys.getenv_opt "TEZOS_SIGNER_TCP_PORT" with
  | None -> "7732"
  | Some port -> port

let default_unix_path =
  match Sys.getenv_opt "TEZOS_SIGNER_UNIX_PATH" with
  | None -> Filename.concat (Sys.getenv "HOME") (".tezos-signer.sock")
  | Some path -> path

let default_https_host =
  match Sys.getenv_opt "TEZOS_SIGNER_HTTPS_HOST" with
  | None -> "localhost"
  | Some host -> host

let default_https_port =
  match Sys.getenv_opt "TEZOS_SIGNER_HTTPS_PORT" with
  | None -> "443"
  | Some port -> port

open Clic

let group =
  { Clic.name = "signer" ;
    title = "Commands specific to the signing daemon" }

let commands =
  Client_keys_commands.commands () @
  [ command ~group
      ~desc: "Launch a signer daemon over a TCP socket."
      (args2
         (default_arg
            ~doc: "listening address or host name"
            ~short: 'a'
            ~long: "address"
            ~placeholder: "host|address"
            ~default: default_tcp_host
            (parameter (fun _ s -> return s)))
         (default_arg
            ~doc: "listening TCP port"
            ~short: 'p'
            ~long: "port"
            ~placeholder: "port number"
            ~default: default_tcp_port
            (parameter
               (fun _ x ->
                  try return (int_of_string x)
                  with Failure _ -> failwith "Invalid port %s" x))))
      (prefixes [ "launch" ; "socket" ; "signer" ] @@ stop)
      (fun (host, port) cctxt ->
         Socket_daemon.run cctxt (Tcp (host, port))) ;
    command ~group
      ~desc: "Launch a signer daemon over a local Unix socket."
      (args1
         (default_arg
            ~doc: "path to the local socket file"
            ~short: 's'
            ~long: "socket"
            ~placeholder: "path"
            ~default: default_unix_path
            (parameter (fun _ s -> return s))))
      (prefixes [ "launch" ; "local" ; "signer" ] @@ stop)
      (fun path cctxt ->
         Socket_daemon.run cctxt (Unix path)) ;
    command ~group
      ~desc: "Launch a signer daemon over HTTPS."
      (args2
         (default_arg
            ~doc: "listening address or host name"
            ~short: 'a'
            ~long: "address"
            ~placeholder: "host|address"
            ~default: default_https_host
            (parameter (fun _ s -> return s)))
         (default_arg
            ~doc: "listening HTTPS port"
            ~short: 'p'
            ~long: "port"
            ~placeholder: "port number"
            ~default: default_https_port
            (parameter
               (fun _ x ->
                  try return (int_of_string x)
                  with Failure _ -> failwith "Invalid port %s" x))))
      (prefixes [ "launch" ; "https" ; "signer" ] @@
       param
         ~name:"cert"
         ~desc: "path to th TLS certificate"
         (parameter (fun _ s -> return s)) @@
       param
         ~name:"key"
         ~desc: "path to th TLS key"
         (parameter (fun _ s -> return s)) @@ stop)
      (fun (host, port) cert key cctxt ->
         Https_daemon.run cctxt ~host ~port ~cert ~key) ;
  ]

let home = try Sys.getenv "HOME" with Not_found -> "/root"

let default_base_dir =
  Filename.concat home ".tezos-signer"

let (//) = Filename.concat

let string_parameter () : (string, _) parameter =
  parameter (fun _ x -> return x)

let base_dir_arg () =
  arg
    ~long:"base-dir"
    ~short:'d'
    ~placeholder:"path"
    ~doc:("signer data directory\n\
           The directory where the Tezos client will store all its data.\n\
           By default: '" ^ default_base_dir ^"'.")
    (string_parameter ())

let global_options () =
  args1
    (base_dir_arg ())

(* Main (lwt) entry *)
let main () =
  let executable_name = Filename.basename Sys.executable_name in
  let original_args, autocomplete =
    (* for shell aliases *)
    let rec move_autocomplete_token_upfront acc = function
      | "bash_autocomplete" :: prev_arg :: cur_arg :: script :: args ->
          let args = List.rev acc @ args in
          args, Some (prev_arg, cur_arg, script)
      | x :: rest -> move_autocomplete_token_upfront (x :: acc) rest
      | [] -> List.rev acc, None in
    match Array.to_list Sys.argv with
    | _ :: args -> move_autocomplete_token_upfront [] args
    | [] -> [], None in
  Random.self_init () ;
  ignore Clic.(setup_formatter Format.std_formatter
                 (if Unix.isatty Unix.stdout then Ansi else Plain) Short) ;
  ignore Clic.(setup_formatter Format.err_formatter
                 (if Unix.isatty Unix.stderr then Ansi else Plain) Short) ;
  begin
    begin
      parse_global_options
        (global_options ()) () original_args >>=? fun (base_dir, remaining) ->
      let base_dir = Option.unopt ~default:default_base_dir base_dir in
      let cctxt = object
        inherit Client_context_unix.unix_logger ~base_dir
        inherit Client_context_unix.unix_prompter
        inherit Client_context_unix.unix_wallet ~base_dir
      end in
      Client_keys.register_signer
        (module Tezos_signer_backends.Encrypted) ;
      Client_keys.register_signer
        (module Tezos_signer_backends.Unencrypted) ;
      let commands =
        Clic.add_manual
          ~executable_name
          ~global_options:(global_options ())
          (if Unix.isatty Unix.stdout then Clic.Ansi else Clic.Plain)
          Format.std_formatter
          commands in
      begin match autocomplete with
        | Some (prev_arg, cur_arg, script) ->
            Clic.autocompletion
              ~script ~cur_arg ~prev_arg ~args:original_args
              ~global_options:(global_options ())
              commands cctxt >>=? fun completions ->
            List.iter print_endline completions ;
            return ()
        | None ->
            Clic.dispatch commands cctxt remaining
      end
    end >>= function
    | Ok () ->
        Lwt.return 0
    | Error [ Clic.Help command ] ->
        Clic.usage
          Format.std_formatter
          ~executable_name
          ~global_options:(global_options ())
          (match command with None -> [] | Some c -> [ c ]) ;
        Lwt.return 0
    | Error errs ->
        Clic.pp_cli_errors
          Format.err_formatter
          ~executable_name
          ~global_options:(global_options ())
          ~default:Error_monad.pp
          errs ;
        Lwt.return 1
  end >>= fun retcode ->
  Format.pp_print_flush Format.err_formatter () ;
  Format.pp_print_flush Format.std_formatter () ;
  Logging_unix.close () >>= fun () ->
  Lwt.return retcode

let () =
  Pervasives.exit (Lwt_main.run (main ()))
