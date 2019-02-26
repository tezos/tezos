(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

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

let default_http_host =
  match Sys.getenv_opt "TEZOS_SIGNER_HTTP_HOST" with
  | None -> "localhost"
  | Some host -> host

let default_http_port =
  match Sys.getenv_opt "TEZOS_SIGNER_HTTP_PORT" with
  | None -> "6732"
  | Some port -> port

open Clic

let group =
  { Clic.name = "signer" ;
    title = "Commands specific to the signing daemon" }

let magic_bytes_arg =
  Clic.arg
    ~doc: "values allowed for the magic bytes, defaults to any"
    ~short: 'M'
    ~long: "magic-bytes"
    ~placeholder: "0xHH,0xHH,..."
    (Clic.parameter (fun _ s ->
         try
           return
             (List.map
                (fun s ->
                   let b = int_of_string s in
                   if b < 0 || b > 255 then raise Exit else b)
                (String.split ',' s))
         with _ ->
           failwith "Bad format for magic bytes, a series of numbers \
                     is expected, separated by commas."))

let high_watermark_switch =
  Clic.switch
    ~doc: "high watermark restriction\n\
           Stores the highest level signed for blocks and endorsements \
           for each address, and forbids to sign a level that is \
           inferior or equal afterwards, except for the exact same \
           input data."
    ~short: 'W'
    ~long: "check-high-watermark"
    ()

let pidfile_arg =
  arg
    ~doc: "write process id in file"
    ~short: 'P'
    ~long: "pidfile"
    ~placeholder: "filename"
    (parameter (fun _ s -> return s))

let init_signal () =
  let handler name id = try
      Format.eprintf "Received the %s signal, triggering shutdown.@." name ;
      exit id
    with _ -> () in
  ignore (Lwt_unix.on_signal Sys.sigint (handler "INT") : Lwt_unix.signal_handler_id) ;
  ignore (Lwt_unix.on_signal Sys.sigterm (handler "TERM") : Lwt_unix.signal_handler_id)

let may_setup_pidfile = function
  | None -> return_unit
  | Some pidfile ->
      trace (failure "Failed to create the pidfile: %s" pidfile) @@
      Lwt_lock_file.create ~unlink_on_exit:true pidfile

let commands base_dir require_auth : Client_context.full command list =
  Tezos_signer_backends.Ledger.commands () @
  List.map
    (Clic.map_command
       (fun (o : Client_context.full) -> (o :> Client_context.io_wallet)))
    (Client_keys_commands.commands None @
     [ command ~group
         ~desc: "Launch a signer daemon over a TCP socket."
         (args5
            pidfile_arg
            magic_bytes_arg
            high_watermark_switch
            (default_arg
               ~doc: "listening address or host name"
               ~short: 'a'
               ~long: "address"
               ~placeholder: "host|address"
               ~default: default_tcp_host
               (parameter (fun _ s -> return s)))
            (default_arg
               ~doc: "listening TCP port or service name"
               ~short: 'p'
               ~long: "port"
               ~placeholder: "port number"
               ~default: default_tcp_port
               (parameter (fun _ s -> return s))))
         (prefixes [ "launch" ; "socket" ; "signer" ] @@ stop)
         (fun (pidfile, magic_bytes, check_high_watermark, host, port) cctxt ->
            init_signal () ;
            may_setup_pidfile pidfile >>=? fun () ->
            Tezos_signer_backends.Encrypted.decrypt_all cctxt >>=? fun () ->
            Socket_daemon.run
              cctxt (Tcp (host, port, [AI_SOCKTYPE SOCK_STREAM]))
              ?magic_bytes ~check_high_watermark ~require_auth >>=? fun _ ->
            return_unit) ;
       command ~group
         ~desc: "Launch a signer daemon over a local Unix socket."
         (args4
            pidfile_arg
            magic_bytes_arg
            high_watermark_switch
            (default_arg
               ~doc: "path to the local socket file"
               ~short: 's'
               ~long: "socket"
               ~placeholder: "path"
               ~default: (Filename.concat base_dir "socket")
               (parameter (fun _ s -> return s))))
         (prefixes [ "launch" ; "local" ; "signer" ] @@ stop)
         (fun (pidfile, magic_bytes, check_high_watermark, path) cctxt ->
            init_signal () ;
            may_setup_pidfile pidfile >>=? fun () ->
            Tezos_signer_backends.Encrypted.decrypt_all cctxt >>=? fun () ->
            Socket_daemon.run
              cctxt (Unix path) ?magic_bytes ~check_high_watermark ~require_auth >>=? fun _ ->
            return_unit) ;
       command ~group
         ~desc: "Launch a signer daemon over HTTP."
         (args5
            pidfile_arg
            magic_bytes_arg
            high_watermark_switch
            (default_arg
               ~doc: "listening address or host name"
               ~short: 'a'
               ~long: "address"
               ~placeholder: "host|address"
               ~default: default_http_host
               (parameter (fun _ s -> return s)))
            (default_arg
               ~doc: "listening HTTP port"
               ~short: 'p'
               ~long: "port"
               ~placeholder: "port number"
               ~default: default_http_port
               (parameter
                  (fun _ x ->
                     try return (int_of_string x)
                     with Failure _ -> failwith "Invalid port %s" x))))
         (prefixes [ "launch" ; "http" ; "signer" ] @@ stop)
         (fun (pidfile, magic_bytes, check_high_watermark, host, port) cctxt ->
            init_signal () ;
            may_setup_pidfile pidfile >>=? fun () ->
            Tezos_signer_backends.Encrypted.decrypt_all cctxt >>=? fun () ->
            Http_daemon.run_http cctxt ~host ~port ?magic_bytes ~check_high_watermark ~require_auth) ;
       command ~group
         ~desc: "Launch a signer daemon over HTTPS."
         (args5
            pidfile_arg
            magic_bytes_arg
            high_watermark_switch
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
            ~desc: "path to the TLS certificate"
            (parameter (fun _ s ->
                 if not (Sys.file_exists s) then
                   failwith "No such TLS certificate file %s" s
                 else
                   return s)) @@
          param
            ~name:"key"
            ~desc: "path to the TLS key"
            (parameter (fun _ s ->
                 if not (Sys.file_exists s) then
                   failwith "No such TLS key file %s" s
                 else
                   return s)) @@ stop)
         (fun (pidfile, magic_bytes, check_high_watermark, host, port) cert key cctxt ->
            init_signal () ;
            may_setup_pidfile pidfile >>=? fun () ->
            Tezos_signer_backends.Encrypted.decrypt_all cctxt >>=? fun () ->
            Http_daemon.run_https cctxt ~host ~port ~cert ~key ?magic_bytes ~check_high_watermark ~require_auth) ;
       command ~group
         ~desc: "Authorize a given public key to perform signing requests."
         (args1
            (arg
               ~doc: "an optional name for the key (defaults to the hash)"
               ~short: 'N'
               ~long: "name"
               ~placeholder: "name"
               (parameter (fun _ s -> return s))))
         (prefixes [ "add" ; "authorized" ; "key" ] @@
          param
            ~name:"pk"
            ~desc: "full public key (Base58 encoded)"
            (parameter (fun _ s -> Lwt.return (Signature.Public_key.of_b58check s))) @@
          stop)
         (fun name key cctxt ->
            let pkh = Signature.Public_key.hash key in
            let name = match name with
              | Some name -> name
              | None -> Signature.Public_key_hash.to_b58check pkh in
            Handler.Authorized_key.add ~force:false cctxt name key)
     ])


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

let require_auth_arg () =
  switch
    ~long:"require-authentication"
    ~short:'A'
    ~doc:"Require a signature from the caller to sign."
    ()

let password_filename_arg () =
  arg
    ~long:"password-file"
    ~short:'f'
    ~placeholder:"filename"
    ~doc:"Absolute path of the password file"
    (string_parameter ())

let global_options () =
  args3
    (base_dir_arg ())
    (require_auth_arg ())
    (password_filename_arg ())

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
        (global_options ()) () original_args >>=?
      fun ((base_dir, require_auth, password_filename), remaining) ->
      let base_dir = Option.unopt ~default:default_base_dir base_dir in
      let cctxt =
        new Client_context_unix.unix_full
          ~block:Client_config.default_block
          ~confirmations:None
          ~password_filename
          ~base_dir
          ~rpc_config:RPC_client.default_config in
      Client_keys.register_signer
        (module Tezos_signer_backends.Encrypted.Make(struct
             let cctxt = new Client_context_unix.unix_prompter
           end)) ;
      Client_keys.register_signer
        (module Tezos_signer_backends.Unencrypted) ;
      Client_keys.register_signer
        (module Tezos_signer_backends.Ledger) ;
      Logging_unix.init () >>= fun () ->
      let module Remote_params = struct
        let authenticate pkhs payload =
          Client_keys.list_keys cctxt >>=? fun keys ->
          match List.filter_map begin function
              | (_, known_pkh, _, Some known_sk_uri)
                when List.exists (fun pkh -> Signature.Public_key_hash.equal pkh known_pkh) pkhs ->
                  Some known_sk_uri
              | _ -> None
            end keys with
          | sk_uri :: _ ->
              Client_keys.sign cctxt sk_uri payload
          | [] -> failwith
                    "remote signer expects authentication signature, \
                     but no authorized key was found in the wallet"
        let logger = RPC_client.full_logger Format.err_formatter
      end in
      let module Socket = Tezos_signer_backends.Socket.Make(Remote_params) in
      let module Http = Tezos_signer_backends.Http.Make(Remote_params) in
      let module Https = Tezos_signer_backends.Https.Make(Remote_params) in
      Client_keys.register_signer (module Socket.Unix) ;
      Client_keys.register_signer (module Socket.Tcp) ;
      Client_keys.register_signer (module Http) ;
      Client_keys.register_signer (module Https) ;
      let commands =
        Clic.add_manual
          ~executable_name
          ~global_options:(global_options ())
          (if Unix.isatty Unix.stdout then Clic.Ansi else Clic.Plain)
          Format.std_formatter
          (commands base_dir require_auth) in
      begin match autocomplete with
        | Some (prev_arg, cur_arg, script) ->
            Clic.autocompletion
              ~script ~cur_arg ~prev_arg ~args:original_args
              ~global_options:(global_options ())
              commands cctxt >>=? fun completions ->
            List.iter print_endline completions ;
            return_unit
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
