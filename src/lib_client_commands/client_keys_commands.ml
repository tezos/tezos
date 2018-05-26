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
  { Clic.name = "keys" ;
    title = "Commands for managing the wallet of cryptographic keys" }

let sig_algo_arg =
  Clic.default_arg
    ~doc:"use custom signature algorithm"
    ~long:"sig"
    ~short:'s'
    ~placeholder:"ed25519|secp256k1"
    ~default: "ed25519"
    (Signature.algo_param ())

let gen_keys_containing
    ?(prefix=false) ?(force=false)
    ~containing ~name (cctxt : #Client_context.io_wallet) =
  let unrepresentable =
    List.filter (fun s -> not @@ Base58.Alphabet.all_in_alphabet Base58.Alphabet.bitcoin s) containing in
  match unrepresentable with
  | _ :: _ ->
      cctxt#warning
        "The following can't be written in the key alphabet (%a): %a"
        Base58.Alphabet.pp Base58.Alphabet.bitcoin
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
           (fun ppf s -> Format.fprintf ppf "'%s'" s))
        unrepresentable >>= return
  | [] ->
      Public_key_hash.mem cctxt name >>=? fun name_exists ->
      if name_exists && not force
      then
        cctxt#warning
          "Key for name '%s' already exists. Use -force to update." name >>= return
      else
        begin
          cctxt#warning "This process uses a brute force search and \
                         may take a long time to find a key." >>= fun () ->
          let matches =
            if prefix then
              let containing_tz1 = List.map ((^) "tz1") containing in
              (fun key -> List.exists
                  (fun containing ->
                     String.sub key 0 (String.length containing) = containing)
                  containing_tz1)
            else
              let re = Re.Str.regexp (String.concat "\\|" containing) in
              (fun key -> try ignore (Re.Str.search_forward re key 0); true
                with Not_found -> false) in
          let rec loop attempts =
            let public_key_hash, public_key, secret_key =
              Signature.generate_key () in
            let hash = Signature.Public_key_hash.to_b58check @@
              Signature.Public_key.hash public_key in
            if matches hash
            then
              let pk_uri = Tezos_signer_backends.Unencrypted.make_pk public_key in
              let sk_uri = Tezos_signer_backends.Unencrypted.make_sk secret_key in
              register_key cctxt ~force
                (public_key_hash, pk_uri, sk_uri) name >>=? fun () ->
              return hash
            else begin if attempts mod 25_000 = 0
              then cctxt#message "Tried %d keys without finding a match" attempts
              else Lwt.return () end >>= fun () ->
              loop (attempts + 1) in
          loop 1 >>=? fun key_hash ->
          cctxt#message
            "Generated '%s' under the name '%s'." key_hash name >>= fun () ->
          return ()
        end

let commands () : Client_context.io_wallet Clic.command list =
  let open Clic in
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
      (fun () (cctxt : Client_context.io_wallet) ->
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
      (args2 (Secret_key.force_switch ()) sig_algo_arg)
      (prefixes [ "gen" ; "keys" ]
       @@ Secret_key.fresh_alias_param
       @@ stop)
      (fun (force, algo) name (cctxt : #Client_context.io_wallet) ->
         Secret_key.of_fresh cctxt force name >>=? fun name ->
         let (pkh, pk, sk) = Signature.generate_key ~algo () in
         let pk_uri = Tezos_signer_backends.Unencrypted.make_pk pk in
         let sk_uri = Tezos_signer_backends.Unencrypted.make_sk sk in
         register_key cctxt ~force (pkh, pk_uri, sk_uri) name) ;

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
      (fun (prefix, force) name containing (cctxt : #Client_context.io_wallet) ->
         Public_key_hash.of_fresh cctxt force name >>=? fun name ->
         gen_keys_containing ~force ~prefix ~containing ~name cctxt) ;

    command ~group ~desc: "Add a secret key to the wallet."
      (args1 (Secret_key.force_switch ()))
      (prefix "import"
       @@ prefixes [ "secret" ; "key" ]
       @@ Secret_key.fresh_alias_param
       @@ param
         ~name:"uri"
         ~desc:"secret key\n\
                Varies from one scheme to the other.\n\
                Use command `list signing schemes` for more \
                information."
         (parameter (fun _ s ->
              try return (Client_keys.make_sk_uri @@ Uri.of_string s)
              with Failure s -> failwith "Error while parsing uri: %s" s))
       @@ stop)
      (fun force name sk_uri (cctxt : Client_context.io_wallet) ->
         Secret_key.of_fresh cctxt force name >>=? fun name ->
         Client_keys.neuterize sk_uri >>=? fun pk_uri ->
         begin
           Public_key.find_opt cctxt name >>=? function
           | None -> return ()
           | Some pk ->
               fail_unless (pk_uri = pk || force)
                 (failure
                    "public and secret keys '%s' don't correspond, \
                     please don't use -force" name)
         end >>=? fun () ->
         Client_keys.public_key_hash pk_uri >>=? fun pkh ->
         register_key cctxt ~force (pkh, pk_uri, sk_uri) name) ;

    command ~group ~desc: "Add a public key to the wallet."
      (args1 (Public_key.force_switch ()))
      (prefix "import"
       @@ prefixes [ "public" ; "key" ]
       @@ Public_key.fresh_alias_param
       @@ param
         ~name:"uri"
         ~desc:"public key\n\
                Varies from one scheme to the other.\n\
                Use command `list signing schemes` for more \
                information."
         (parameter (fun _ s ->
              try return (Client_keys.make_pk_uri @@ Uri.of_string s)
              with Failure s -> failwith "Error while parsing uri: %s" s))
       @@ stop)
      (fun force name pk_uri (cctxt : Client_context.io_wallet) ->
         Public_key.of_fresh cctxt force name >>=? fun name ->
         Client_keys.public_key_hash pk_uri >>=? fun pkh ->
         Public_key_hash.add ~force cctxt name pkh >>=? fun () ->
         Public_key.add ~force cctxt name pk_uri) ;

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
      (fun () (cctxt : #Client_context.io_wallet) ->
         list_keys cctxt >>=? fun l ->
         iter_s begin fun (name, pkh, pk, sk) ->
           Public_key_hash.to_source pkh >>=? fun v ->
           begin match pk, sk with
             | None, None ->
                 cctxt#message "%s: %s" name v
             | _, Some uri ->
                 let scheme =
                   Option.unopt ~default:"unencrypted" @@
                   Uri.scheme (uri : sk_uri :> Uri.t) in
                 cctxt#message "%s: %s (%s sk known)" name v scheme
             | Some _, _ ->
                 cctxt#message "%s: %s (pk known)" name v
           end >>= fun () -> return ()
         end l) ;

    command ~group ~desc: "Show the keys associated with an identity."
      (args1 show_private_switch)
      (prefixes [ "show" ; "identity"]
       @@ Public_key_hash.alias_param
       @@ stop)
      (fun show_private (name, _) (cctxt : #Client_context.io_wallet) ->
         alias_keys cctxt name >>=? fun key_info ->
         match key_info with
         | None ->
             cctxt#message "No keys found for identity" >>= fun () ->
             return ()
         | Some (pkh, pk, skloc) ->
             cctxt#message "Hash: %a"
               Signature.Public_key_hash.pp pkh >>= fun () ->
             match pk with
             | None -> return ()
             | Some pk ->
                 cctxt#message "Public Key: %a"
                   Signature.Public_key.pp pk >>= fun () ->
                 if show_private then
                   match skloc with
                   | None -> return ()
                   | Some skloc ->
                       Secret_key.to_source skloc >>=? fun skloc ->
                       cctxt#message "Secret Key: %s" skloc >>= fun () ->
                       return ()
                 else
                   return ()) ;

    command ~group ~desc: "Forget the entire wallet of keys."
      (args1 (Clic.switch
                ~long:"force" ~short:'f'
                ~doc:"you got to use the force for that" ()))
      (fixed [ "forget" ; "all" ; "keys" ])
      (fun force (cctxt : Client_context.io_wallet) ->
         fail_unless force
           (failure "this can only used with option -force") >>=? fun () ->
         Public_key.set cctxt [] >>=? fun () ->
         Secret_key.set cctxt [] >>=? fun () ->
         Public_key_hash.set cctxt []) ;

  ]
