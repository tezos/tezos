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


let () =
  Client_main_run.run
    (fun _ _ ->
       return @@
       List.map
         (Clic.map_command
            (fun (o : Client_context.full) -> (o :> Client_context.io_wallet)))
         commands)
