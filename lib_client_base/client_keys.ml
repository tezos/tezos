(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Public_key_hash = Client_aliases.Alias (struct
    type t = Ed25519.Public_key_hash.t
    let encoding = Ed25519.Public_key_hash.encoding
    let of_source _ s = Lwt.return (Ed25519.Public_key_hash.of_b58check s)
    let to_source _ p = return (Ed25519.Public_key_hash.to_b58check p)
    let name = "public key hash"
  end)

module Public_key = Client_aliases.Alias (struct
    type t = Ed25519.Public_key.t
    let encoding = Ed25519.Public_key.encoding
    let of_source _ s = Lwt.return (Ed25519.Public_key.of_b58check s)
    let to_source _ p = return (Ed25519.Public_key.to_b58check p)
    let name = "public key"
  end)

module Secret_key = Client_aliases.Alias (struct
    type t = Ed25519.Secret_key.t
    let encoding = Ed25519.Secret_key.encoding
    let of_source _ s = Lwt.return (Ed25519.Secret_key.of_b58check s)
    let to_source _ p = return (Ed25519.Secret_key.to_b58check p)
    let name = "secret key"
  end)

let gen_keys ?(force=false) ?seed (cctxt : #Client_commands.wallet) name =
  let seed =
    match seed with
    | None -> Ed25519.Seed.generate ()
    | Some s -> s in
  let _, public_key, secret_key = Ed25519.generate_seeded_key seed in
  Secret_key.add ~force cctxt name secret_key >>=? fun () ->
  Public_key.add ~force cctxt name public_key >>=? fun () ->
  Public_key_hash.add ~force
    cctxt name (Ed25519.Public_key.hash public_key) >>=? fun () ->
  return ()

let gen_keys_containing ?(prefix=false) ?(force=false) ~containing ~name (cctxt : Client_commands.full_context) =
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
              let re = Str.regexp (String.concat "\\|" containing) in
              (fun key -> try ignore (Str.search_forward re key 0); true
                with Not_found -> false) in
          let rec loop attempts =
            let seed = Ed25519.Seed.generate () in
            let _, public_key, secret_key = Ed25519.generate_seeded_key seed in
            let hash = Ed25519.Public_key_hash.to_b58check @@ Ed25519.Public_key.hash public_key in
            if matches hash
            then
              Secret_key.add ~force cctxt name secret_key >>=? fun () ->
              Public_key.add ~force cctxt name public_key >>=? fun () ->
              Public_key_hash.add ~force cctxt name (Ed25519.Public_key.hash public_key) >>=? fun () ->
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

let check_keys_consistency pk sk =
  let message = MBytes.of_string "Voulez-vous coucher avec moi, ce soir ?" in
  let signature = Ed25519.sign sk message in
  Ed25519.Signature.check pk signature message

let get_key (cctxt : #Client_commands.wallet) pkh =
  Public_key_hash.rev_find cctxt pkh >>=? function
  | None -> failwith "no keys for the source contract manager"
  | Some n ->
      Public_key.find cctxt n >>=? fun pk ->
      Secret_key.find cctxt n >>=? fun sk ->
      return (n, pk, sk)

let get_keys (wallet : #Client_commands.wallet) =
  Secret_key.load wallet >>=? fun sks ->
  Lwt_list.filter_map_s
    (fun (name, sk) ->
       begin
         Public_key.find wallet name >>=? fun pk ->
         Public_key_hash.find wallet name >>=? fun pkh ->
         return (name, pkh, pk, sk)
       end >>= function
       | Ok r -> Lwt.return (Some r)
       | Error _ -> Lwt.return_none)
    sks >>= fun keys ->
  return keys

let list_keys cctxt =
  Public_key_hash.load cctxt >>=? fun l ->
  map_s
    (fun (name, pkh) ->
       Public_key.mem cctxt name >>=? fun pkm ->
       Secret_key.mem cctxt name >>=? fun pks ->
       return (name, pkh, pkm, pks))
    l

let alias_keys cctxt name =
  Public_key_hash.load cctxt >>=? fun l ->
  let rec find_key = function
    | [] -> return None
    | (key_name, pkh) :: tl ->
        if key_name = name
        then
          Public_key.find_opt cctxt name >>=? fun pkm ->
          Secret_key.find_opt cctxt name >>=? fun pks ->
          return (Some (pkh, pkm, pks))
        else find_key tl
  in find_key l

let group =
  { Cli_entries.name = "keys" ;
    title = "Commands for managing cryptographic keys" }

let commands () =
  let open Cli_entries in
  let open Client_commands in
  let show_private_switch =
    switch
      ~parameter:"-show-secret"
      ~doc:"Show the private key" in
  [

    command ~group ~desc: "generate a pair of keys"
      (args1 Client_commands.force_switch)
      (prefixes [ "gen" ; "keys" ]
       @@ Secret_key.fresh_alias_param
       @@ stop)
      (fun force name (cctxt : Client_commands.full_context) ->
         Secret_key.of_fresh cctxt force name >>=? fun name ->
         gen_keys ~force cctxt name) ;

    command ~group ~desc: "Generate keys including the given string"
      (args2 (switch ~doc:"The key must begin with tz1[containing]" ~parameter:"-prefix") force_switch)
      (prefixes [ "gen" ; "vanity" ; "keys" ]
       @@ Public_key_hash.fresh_alias_param
       @@ prefix "matching"
       @@ (seq_of_param @@ string ~name:"strs" ~desc:"String key must contain"))
      (fun (prefix, force) name containing cctxt ->
         Public_key_hash.of_fresh cctxt force name >>=? fun name ->
         gen_keys_containing ~force ~prefix ~containing ~name cctxt) ;

    command ~group ~desc: "add a secret key to the wallet"
      (args1 Client_commands.force_switch)
      (prefixes [ "add" ; "secret" ; "key" ]
       @@ Secret_key.fresh_alias_param
       @@ Secret_key.source_param
       @@ stop)
      (fun force name sk cctxt ->
         Secret_key.of_fresh cctxt force name >>=? fun name ->
         Public_key.find_opt cctxt name >>=? function
         | None ->
             let pk = Ed25519.Secret_key.to_public_key sk in
             Public_key_hash.add ~force cctxt
               name (Ed25519.Public_key.hash pk) >>=? fun () ->
             Public_key.add ~force cctxt name pk >>=? fun () ->
             Secret_key.add ~force cctxt name sk
         | Some pk ->
             fail_unless
               (check_keys_consistency pk sk || force)
               (failure
                  "public and secret keys '%s' don't correspond, \
                   please don't use -force" name) >>=? fun () ->
             Secret_key.add ~force cctxt name sk) ;

    command ~group ~desc: "add a public key to the wallet"
      (args1 Client_commands.force_switch)
      (prefixes [ "add" ; "public" ; "key" ]
       @@ Public_key.fresh_alias_param
       @@ Public_key.source_param
       @@ stop)
      (fun force name key cctxt ->
         Public_key.of_fresh cctxt force name >>=? fun name ->
         Public_key_hash.add ~force cctxt
           name (Ed25519.Public_key.hash key) >>=? fun () ->
         Public_key.add ~force cctxt name key) ;

    command ~group ~desc: "add a public key to the wallet"
      (args1 Client_commands.force_switch)
      (prefixes [ "add" ; "identity" ]
       @@ Public_key_hash.fresh_alias_param
       @@ Public_key_hash.source_param
       @@ stop)
      (fun force name hash cctxt ->
         Public_key_hash.of_fresh cctxt force name >>=? fun name ->
         Public_key_hash.add ~force cctxt name hash) ;

    command ~group ~desc: "list all public key hashes and associated keys"
      no_options
      (fixed [ "list" ; "known" ; "identities" ])
      (fun () (cctxt : Client_commands.full_context) ->
         list_keys cctxt >>=? fun l ->
         iter_s
           (fun (name, pkh, pkm, pks) ->
              Public_key_hash.to_source cctxt pkh >>=? fun v ->
              cctxt#message "%s: %s%s%s" name v
                (if pkm then " (public key known)" else "")
                (if pks then " (secret key known)" else "") >>= fun () ->
              return ())
           l) ;

    command ~group ~desc: "show the keys associated with an identity"
      (args1 show_private_switch)
      (prefixes [ "show" ; "identity"]
       @@ Public_key_hash.alias_param
       @@ stop)
      (fun show_private (name, _) (cctxt : Client_commands.full_context) ->
         let ok_lwt x = x >>= (fun x -> return x) in
         alias_keys cctxt name >>=? fun key_info ->
         match key_info with
         | None -> ok_lwt @@ cctxt#message "No keys found for identity"
         | Some (hash, pub, priv) ->
             Public_key_hash.to_source cctxt hash >>=? fun hash ->
             ok_lwt @@ cctxt#message "Hash: %s" hash >>=? fun () ->
             match pub with
             | None -> return ()
             | Some pub ->
                 Public_key.to_source cctxt pub >>=? fun pub ->
                 ok_lwt @@ cctxt#message "Public Key: %s" pub >>=? fun () ->
                 if show_private then
                   match priv with
                   | None -> return ()
                   | Some priv ->
                       Secret_key.to_source cctxt priv >>=? fun priv ->
                       ok_lwt @@ cctxt#message "Secret Key: %s" priv
                 else return ()) ;

    command ~group ~desc: "forget all keys"
      (args1 Client_commands.force_switch)
      (fixed [ "forget" ; "all" ; "keys" ])
      (fun force cctxt ->
         fail_unless force
           (failure "this can only used with option -force") >>=? fun () ->
         Public_key.set cctxt [] >>=? fun () ->
         Secret_key.set cctxt [] >>=? fun () ->
         Public_key_hash.set cctxt []) ;

  ]
