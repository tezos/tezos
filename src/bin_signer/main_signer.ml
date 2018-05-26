(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_signer_remote_messages

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

let log = Logging.Client.Sign.lwt_log_notice

let run_socket_daemon (cctxt : #Client_context.io_wallet) path =
  Lwt_utils_unix.Socket.bind path >>=? fun fd ->
  let rec loop () =
    Lwt_unix.accept fd >>= fun (fd, _) ->
    Lwt.async (fun () ->
        Lwt_utils_unix.Socket.recv fd Request.encoding >>=? function
        | Sign req ->
            log "Request for signing %d bytes of data for key %s, magic byte = %02X"
              (MBytes.length req.data) req.key (MBytes.get_uint8 req.data 0) >>= fun () ->
            let encoding = result_encoding Sign.Response.encoding in
            Client_keys.alias_keys cctxt req.key >>=? begin function
              | Some (_, _, Some skloc) ->
                  log "Signing data for key %s" req.key >>= fun () ->
                  Client_keys.sign cctxt skloc req.data >>=? fun signature ->
                  Lwt_utils_unix.Socket.send fd encoding (ok { Sign.Response.signature = signature })
              | _ ->
                  Lwt_utils_unix.Socket.send fd encoding (error (Unkwnon_alias_key req.key)) >>=? fun _ ->
                  log "Cannot get alias for key %s" req.key >>= fun () ->
                  return ()
            end

        | Public_key req ->
            log "Request for public key %s" req.key >>= fun () ->
            let encoding = result_encoding Public_key.Response.encoding in
            Client_keys.alias_keys cctxt req.key >>= begin function
              | Error err ->
                  Lwt_utils_unix.Socket.send fd encoding (Error err) >>=? fun _ ->
                  log "Cannot get alias for key %s" req.key >>= fun () ->
                  return ()
              | Ok value ->
                  begin match value with
                    | Some (public_key_hash, _, _) ->
                        log "Found public key hash %a for key %s"
                          Signature.Public_key_hash.pp public_key_hash req.key >>= fun () ->
                        Client_keys.get_key cctxt public_key_hash >>= begin function
                          | Error err ->
                              Lwt_utils_unix.Socket.send fd encoding (Error err) >>=? fun _ ->
                              log "Cannot get key %s" req.key >>= fun () ->
                              return ()
                          | Ok (_, public_key, _) ->
                              log "Send public key %a for key %s"
                                Signature.Public_key.pp public_key req.key >>= fun () ->
                              Lwt_utils_unix.Socket.send fd encoding (ok { Public_key.Response.public_key = public_key }) >>=? fun _ ->
                              return ()
                        end
                    | None -> begin
                        Lwt_utils_unix.Socket.send fd encoding (error (Unkwnon_alias_key req.key)) >>=? fun _ ->
                        log "Cannot find key %s" req.key >>= fun () ->
                        return ()
                      end
                  end
            end
      );
    loop ()
  in
  Lwt_unix.listen fd 10 ;
  begin match path with
    | Tcp (host, port) ->
        log "Accepting TCP requests on %s:%d" host port
    | Unix path ->
        Sys.set_signal Sys.sigint (Signal_handle (fun _ ->
            Format.printf "Removing the local socket file and quitting.@." ;
            Unix.unlink path ;
            exit 0)) ;
        log "Accepting UNIX requests on %s" path
  end >>= fun () ->
  loop ()

let run_https_daemon (cctxt : #Client_context.io_wallet) host port cert key =
  let open Client_signer_remote_services in
  log "Accepting HTTPS requests on port %d" port >>= fun () ->
  let mode : Conduit_lwt_unix.server =
    `TLS (`Crt_file_path cert, `Key_file_path key, `No_password, `Port port) in
  let dir = RPC_directory.empty in
  let dir =
    RPC_directory.register0 dir
      sign (fun () req ->
          log "Request for signing %d bytes of data for key %s, magic byte = %02X"
            (MBytes.length req.data) req.key (MBytes.get_uint8 req.data 0) >>= fun () ->
          Client_keys.alias_keys cctxt req.key >>=? function
          | Some (_, _, Some skloc) ->
              log "Signing data for key %s" req.key >>= fun () ->
              Client_keys.sign cctxt skloc req.data >>=? fun signature ->
              return { Sign.Response.signature = signature }
          | _ ->
              fail (Unkwnon_alias_key req.key)) in
  let dir =
    RPC_directory.register0 dir
      public_key (fun () req ->
          log "Request for public key %s" req.key >>= fun () ->
          Client_keys.alias_keys cctxt req.key >>= begin function
            | Error err ->
                log "Cannot get alias for key %s" req.key >>= fun () ->
                Lwt.return (Error err)
            | Ok value ->
                begin match value with
                  | Some (public_key_hash, _, _) ->
                      log "Found public key hash %a for key %s"
                        Signature.Public_key_hash.pp public_key_hash req.key >>= fun () ->
                      Client_keys.get_key cctxt public_key_hash >>= begin function
                        | Error err ->
                            log "Cannot get key %s" req.key >>= fun () ->
                            Lwt.return (Error err)
                        | Ok (_, public_key, _) ->
                            log "Send public key %a for key %s"
                              Signature.Public_key.pp public_key req.key >>= fun () ->
                            return { Public_key.Response.public_key = public_key }
                      end
                  | None -> begin
                      log "Cannot find key %s" req.key >>= fun () ->
                      fail (Unkwnon_alias_key req.key)
                    end
                end
          end) in
  Lwt.catch
    (fun () ->
       RPC_server.launch ~host mode dir
         ~media_types:Media_type.all_media_types
         ~cors: { allowed_origins = [ "*" ] ;
                  allowed_headers = [ "Content-Type" ] }
       >>= fun _server ->
       fst (Lwt.wait ()))
    (function
      | Unix.Unix_error(Unix.EADDRINUSE, "bind","") ->
          failwith "Port already in use."
      | exn -> Lwt.return (error_exn exn))

open Clic

let group =
  { Clic.name = "signer" ;
    title = "Commands specific to the signing daemon" }

let select_commands _ _ =
  return
    (List.map
       (Clic.map_command
          (fun (o : Client_context.full) -> (o :> Client_context.io_wallet))) @@
     List.flatten
       [ Client_keys_commands.commands () ;
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
                   (parameter (fun _ s -> return s))))
             (prefixes [ "launch" ; "socket" ; "signer" ] @@ stop)
             (fun (host, port) cctxt ->
                let port = int_of_string port in
                run_socket_daemon cctxt (Tcp (host, port))) ;
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
                run_socket_daemon cctxt (Unix path)) ;
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
                   (parameter (fun _ s -> return s))))
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
                let port = int_of_string port in
                run_https_daemon cctxt host port cert key) ;
         ]])

let () =
  Client_main_run.run select_commands
