(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_signer_remote_messages

let run_daemon (cctxt : #Client_context.full) _delegates =
  let uri = Uri.of_string "tezos:/localhost:9000" in
  Connection.bind uri >>= fun fd ->
  cctxt#message "Accepting request on %s" (Uri.to_string uri) >>= fun () ->
  let rec loop () =
    Lwt_unix.accept fd >>= fun (fd, _) ->
    Lwt.async (fun () ->
        cctxt#message "Receiving" >>= fun () ->
        recv fd Request.encoding >>=? function
        | Sign req ->
            cctxt#message "Signer: Request for siging data" >>= fun () ->
            Client_keys.alias_keys cctxt req.key >>=? begin function
              | Some (_, _, Some skloc) ->
                  cctxt#message "Signer: signing data" >>= fun () ->
                  Client_keys.sign cctxt skloc req.data >>=? fun signature ->
                  send fd Sign.Response.encoding (ok { Sign.Response.signature = signature })
              | _ ->
                  send fd Public_key.Response.encoding (error (Unkwnon_alias_key req.key)) >>=? fun _ ->
                  cctxt#warning "Singer: Cannot get alias for key %s" req.key >>= fun () ->
                  return ()
            end

        | Public_key req ->
            cctxt#message "Singer: Request for public key %s" req.key >>= fun () ->
            Client_keys.alias_keys cctxt req.key >>= begin function
              | Error err ->
                  send fd Public_key.Response.encoding (Error err) >>=? fun _ ->
                  cctxt#warning "Singer: Cannot get alias for key %s" req.key >>= fun () ->
                  return ()
              | Ok value ->
                  begin match value with
                    | Some (public_key_hash, _, _) ->
                        cctxt#message "Signer: Hash Public Key %a" Signature.Public_key_hash.pp public_key_hash >>= fun () ->
                        Client_keys.get_key cctxt public_key_hash >>= begin function
                          | (Error err) ->
                              send fd Public_key.Response.encoding (Error err) >>=? fun _ ->
                              cctxt#warning "Singer: cannot get key %s" req.key >>= fun () ->
                              return ()
                          | Ok (_, public_key, _) ->
                              cctxt#message "Signer: Send Public Key %a" Signature.Public_key.pp public_key >>= fun () ->
                              send fd Public_key.Response.encoding
                                (ok { Public_key.Response.public_key = public_key }) >>=? fun _ ->
                              return ()
                        end
                    | _ -> begin
                        send fd Public_key.Response.encoding (error (Unkwnon_alias_key req.key)) >>=? fun _ ->
                        cctxt#warning "Signer cannot find key %s" req.key >>= fun () ->
                        return ()
                      end
                  end
            end
      );
    loop ()
  in
  Lwt_unix.listen fd 10;
  cctxt#message "Listening" >>= fun () ->
  loop ()

open Clic

let group =
  { Clic.name = "signer" ;
    title = "Commands specific to the signing daemon" }

let select_commands _ _ =
  return
    (List.flatten
       [ Client_keys_commands.commands () ;
         [ command ~group ~desc: "Launch the signer daemon."
             no_options
             (prefixes [ "signer" ; "daemon" ]
              @@ seq_of_param Client_keys.Public_key_hash.alias_param)
             (fun () delegates cctxt ->
                run_daemon cctxt delegates) ;
         ]])

let () = Client_main_run.run select_commands
