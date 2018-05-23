(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_signer_remote_messages

let log = Logging.Client.Sign.lwt_log_notice

let run_daemon (cctxt : #Client_context_unix.unix_full) path =
  Connection.bind path >>=? fun (fd, display_path) ->
  let rec loop () =
    Lwt_unix.accept fd >>= fun (fd, _) ->
    Lwt.async (fun () ->
        recv fd Request.encoding >>=? function
        | Sign req ->
            log "Request for signing %d bytes of data for key %s, magic byte = %02X"
              (MBytes.length req.data) req.key (MBytes.get_uint8 req.data 0) >>= fun () ->
            Client_keys.alias_keys cctxt req.key >>=? begin function
              | Some (_, _, Some skloc) ->
                  log "Signing data for key %s" req.key >>= fun () ->
                  Client_keys.sign cctxt skloc req.data >>=? fun signature ->
                  send fd Sign.Response.encoding (ok { Sign.Response.signature = signature })
              | _ ->
                  send fd Public_key.Response.encoding (error (Unkwnon_alias_key req.key)) >>=? fun _ ->
                  log "Cannot get alias for key %s" req.key >>= fun () ->
                  return ()
            end

        | Public_key req ->
            log "Request for public key %s" req.key >>= fun () ->
            Client_keys.alias_keys cctxt req.key >>= begin function
              | Error err ->
                  send fd Public_key.Response.encoding (Error err) >>=? fun _ ->
                  log "Cannot get alias for key %s" req.key >>= fun () ->
                  return ()
              | Ok value ->
                  begin match value with
                    | Some (public_key_hash, _, _) ->
                        log "Found public key hash %a for key %s"
                          Signature.Public_key_hash.pp public_key_hash req.key >>= fun () ->
                        Client_keys.get_key cctxt public_key_hash >>= begin function
                          | Error err ->
                              send fd Public_key.Response.encoding (Error err) >>=? fun _ ->
                              log "Cannot get key %s" req.key >>= fun () ->
                              return ()
                          | Ok (_, public_key, _) ->
                              log "Send public key %a for key %s"
                                Signature.Public_key.pp public_key req.key >>= fun () ->
                              send fd Public_key.Response.encoding
                                (ok { Public_key.Response.public_key = public_key }) >>=? fun _ ->
                              return ()
                        end
                    | None -> begin
                        send fd Public_key.Response.encoding (error (Unkwnon_alias_key req.key)) >>=? fun _ ->
                        log "Cannot find key %s" req.key >>= fun () ->
                        return ()
                      end
                  end
            end
      );
    loop ()
  in
  Lwt_unix.listen fd 10 ;
  log "Accepting requests on %s" display_path >>= fun () ->
  begin match path with
    | Tcp _ -> ()
    | Unix path ->
        Sys.set_signal Sys.sigint (Signal_handle (fun _ ->
            Format.printf "Removing the local socket file and quitting.@." ;
            Unix.unlink path ;
            exit 0)) ;
  end ;
  loop ()

open Clic

let group =
  { Clic.name = "signer" ;
    title = "Commands specific to the signing daemon" }

let select_commands _ _ =
  return
    (List.flatten
       [ Client_keys_commands.commands () ;
         [ command ~group
             ~desc: "Launch a signer daemon over a TCP socket."
             (args2
                (default_arg
                   ~doc: "listening address or host name"
                   ~short: 'a'
                   ~long: "address"
                   ~placeholder: "host|address"
                   ~default: "$TEZOS_SIGNER_TCP_HOST"
                   (parameter (fun _ s -> return s)))
                (default_arg
                   ~doc: "listening TCP port"
                   ~short: 'p'
                   ~long: "port"
                   ~placeholder: "port number"
                   ~default: "$TEZOS_SIGNER_TCP_PORT"
                   (parameter (fun _ s -> return s))))
             (prefixes [ "launch" ; "socket" ; "signer" ] @@ stop)
             (fun (host, port) cctxt ->
                run_daemon cctxt (Tcp (host, port))) ;
           command ~group
             ~desc: "Launch a signer daemon over a local Unix socket."
             (args1
                (default_arg
                   ~doc: "path to the local socket file"
                   ~short: 's'
                   ~long: "socket"
                   ~placeholder: "path"
                   ~default: "TEZOS_SIGNER_UNIX_PATH"
                   (parameter (fun _ s -> return s))))
             (prefixes [ "launch" ; "local" ; "signer" ] @@ stop)
             (fun path cctxt ->
                run_daemon cctxt (Unix path))
         ]])

let () = Client_main_run.run select_commands
