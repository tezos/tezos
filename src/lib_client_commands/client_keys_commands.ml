(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_keys

let group =
  { Cli_entries.name = "keys" ;
    title = "Commands for managing the wallet of cryptographic keys" }

let commands () =
  let open Cli_entries in
  let show_private_switch =
    switch
      ~long:"show-secret"
      ~short:'S'
      ~doc:"show the private key" () in
  [
    command ~group
      ~desc: "List supported signing schemes.\n\
              Signing schemes are identifiers for signer modules: the \
              built-in signing routines, a hardware wallet, an \
              external agent, etc.\n\
              Each signer has its own format for describing secret \
              keys, such a raw secret key for the default \
              `unencrypted` scheme, the path on a hardware security \
              module, an alias for an external agent, etc.\n\
              This command gives the list of signer modules that this \
              version of the tezos client supports."
      no_options
      (fixed [ "list" ; "signing" ; "schemes" ])
      (fun () (cctxt : #Client_context.full_context) ->
         let signers =
           List.sort
             (fun (ka, _) (kb, _) -> String.compare ka kb)
             (registered_signers ()) in
         Lwt_list.iter_s
           (fun (n, (module S : SIGNER))  ->
              cctxt#message "@[<v 2>Scheme `%s`: %s@,@[<hov 0>%a@]@]"
                n S.title Format.pp_print_text S.description)
           signers >>= return) ;

    command ~group ~desc: "Generate a pair of (unencrypted) keys."
      (args1 (Secret_key.force_switch ()))
      (prefixes [ "gen" ; "keys" ]
       @@ Secret_key.fresh_alias_param
       @@ stop)
      (fun force name (cctxt : #Client_context.full_context) ->
         Secret_key.of_fresh cctxt force name >>=? fun name ->
         gen_keys ~force cctxt name) ;

    command ~group ~desc: "Generate (unencrypted) keys including the given string."
      (args2
         (switch
            ~long:"prefix"
            ~short:'P'
            ~doc:"the key must begin with tz1[word]"
            ())
         (force_switch ()))
      (prefixes [ "gen" ; "vanity" ; "keys" ]
       @@ Public_key_hash.fresh_alias_param
       @@ prefix "matching"
       @@ (seq_of_param @@ string ~name:"words" ~desc:"string key must contain one of these words"))
      (fun (prefix, force) name containing cctxt ->
         Public_key_hash.of_fresh cctxt force name >>=? fun name ->
         gen_keys_containing ~force ~prefix ~containing ~name cctxt) ;

    command ~group ~desc: "Add a secret key to the wallet."
      (args1 (Secret_key.force_switch ()))
      (prefix "import"
       @@ string
         ~name:"scheme"
         ~desc:"signer to use for this secret key\n\
                Use command `list signing schemes` for a list of \
                supported signers."
       @@ prefixes [ "secret" ; "key" ]
       @@ Secret_key.fresh_alias_param
       @@ seq_of_param
         (string
            ~name:"spec"
            ~desc:"secret key specification\n\
                   Varies from one scheme to the other.\n\
                   Use command `list signing schemes` for more \
                   information."))
      (fun force scheme name spec cctxt ->
         Secret_key.of_fresh cctxt force name >>=? fun name ->
         find_signer_for_key ~scheme cctxt >>=? fun signer ->
         let module Signer = (val signer : SIGNER) in
         Signer.sk_locator_of_human_input
           (cctxt :> Client_context.io_wallet) spec >>=? fun skloc ->
         Signer.sk_of_locator skloc >>=? fun sk ->
         Signer.neuterize sk >>= fun pk ->
         Signer.pk_to_locator pk >>= fun pkloc ->
         Public_key.find_opt cctxt name >>=? function
         | None ->
             Signer.public_key_hash pk >>= fun pkh ->
             Secret_key.add ~force cctxt name skloc >>=? fun () ->
             Public_key_hash.add ~force cctxt name pkh >>=? fun () ->
             Public_key.add ~force cctxt name pkloc
         | Some pk ->
             fail_unless (pkloc = pk || force)
               (failure
                  "public and secret keys '%s' don't correspond, \
                   please don't use -force" name) >>=? fun () ->
             Secret_key.add ~force cctxt name skloc) ;

    command ~group ~desc: "Add a public key to the wallet."
      (args1 (Public_key.force_switch ()))
      (prefix "import"
       @@ string
         ~name:"scheme"
         ~desc:"signer to use for this public key\n\
                Use command `list signing schemes` for a list of \
                supported signers."
       @@ prefixes [ "public" ; "key" ]
       @@ Public_key.fresh_alias_param
       @@ seq_of_param
         (string
            ~name:"spec"
            ~desc:"public key specification\n\
                   Varies from one scheme to the other.\n\
                   Use command `list signing schemes` for more \
                   information."))
      (fun force scheme name location cctxt ->
         Public_key.of_fresh cctxt force name >>=? fun name ->
         find_signer_for_key ~scheme cctxt >>=? fun signer ->
         let module Signer = (val signer : SIGNER) in
         Signer.pk_locator_of_human_input
           (cctxt :> Client_context.io_wallet) location >>=? fun pkloc ->
         Signer.pk_of_locator pkloc >>=? fun pk ->
         Signer.public_key_hash pk >>= fun pkh ->
         Public_key_hash.add ~force cctxt name pkh >>=? fun () ->
         Public_key.add ~force cctxt name pkloc) ;

    command ~group ~desc: "Add an identity to the wallet."
      (args1 (Public_key.force_switch ()))
      (prefixes [ "add" ; "identity" ]
       @@ Public_key_hash.fresh_alias_param
       @@ Public_key_hash.source_param
       @@ stop)
      (fun force name hash cctxt ->
         Public_key_hash.of_fresh cctxt force name >>=? fun name ->
         Public_key_hash.add ~force cctxt name hash) ;

    command ~group ~desc: "List all identities and associated keys."
      no_options
      (fixed [ "list" ; "known" ; "identities" ])
      (fun () (cctxt : #Client_context.full_context) ->
         list_keys cctxt >>=? fun l ->
         iter_s begin fun (name, pkh, pk, sk) ->
           Public_key_hash.to_source pkh >>=? fun v ->
           begin match pk, sk with
             | None, None ->
                 cctxt#message "%s: %s" name v
             | _, Some Sk_locator { scheme } ->
                 cctxt#message "%s: %s (%s sk known)" name v scheme
             | Some Pk_locator { scheme }, _ ->
                 cctxt#message "%s: %s (%s pk known)" name v scheme
           end >>= fun () -> return ()
         end l) ;

    command ~group ~desc: "Show the keys associated with an identity."
      (args1 show_private_switch)
      (prefixes [ "show" ; "identity"]
       @@ Public_key_hash.alias_param
       @@ stop)
      (fun show_private (name, _) (cctxt : #Client_context.full_context) ->
         let ok_lwt x = x >>= (fun x -> return x) in
         alias_keys cctxt name >>=? fun key_info ->
         match key_info with
         | None -> ok_lwt @@ cctxt#message "No keys found for identity"
         | Some (pkh, pk, skloc) ->
             ok_lwt @@ cctxt#message "Hash: %a"
               Ed25519.Public_key_hash.pp pkh >>=? fun () ->
             match pk with
             | None -> return ()
             | Some (Pk_locator { scheme } as pkloc) ->
                 find_signer_for_key ~scheme cctxt >>=? fun signer ->
                 let module Signer = (val signer : SIGNER) in
                 Signer.pk_of_locator pkloc >>=? fun pk ->
                 Signer.public_key pk >>= fun pk ->
                 ok_lwt @@ cctxt#message "Public Key: %a"
                   Ed25519.Public_key.pp pk >>=? fun () ->
                 if show_private then
                   match skloc with
                   | None -> return ()
                   | Some skloc ->
                       Secret_key.to_source skloc >>=? fun skloc ->
                       ok_lwt @@ cctxt#message "Secret Key: %s" skloc
                 else return ()) ;

    command ~group ~desc: "Forget the entire wallet of keys."
      (args1 (Cli_entries.switch
                ~long:"force" ~short:'f'
                ~doc:"you got to use the force for that" ()))
      (fixed [ "forget" ; "all" ; "keys" ])
      (fun force cctxt ->
         fail_unless force
           (failure "this can only used with option -force") >>=? fun () ->
         Public_key.set cctxt [] >>=? fun () ->
         Secret_key.set cctxt [] >>=? fun () ->
         Public_key_hash.set cctxt []) ;

  ]
