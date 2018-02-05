(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type error += Unregistered_key_scheme of string
let () =
  register_error_kind `Permanent
    ~id: "cli.unregistered_key_scheme"
    ~title: "Unregistered key scheme"
    ~description: "A key has been provided with an \
                   unregistered scheme (no corresponding plugin)"
    ~pp:
      (fun ppf s ->
         Format.fprintf ppf "No matching plugin for key scheme %s" s)
    Data_encoding.(obj1 (req "value" string))
    (function Unregistered_key_scheme s -> Some s | _ -> None)
    (fun s -> Unregistered_key_scheme s)

module Public_key_hash = Client_aliases.Alias (struct
    type t = Ed25519.Public_key_hash.t
    let encoding = Ed25519.Public_key_hash.encoding
    let of_source s = Lwt.return (Ed25519.Public_key_hash.of_b58check s)
    let to_source p = return (Ed25519.Public_key_hash.to_b58check p)
    let name = "public key hash"
  end)

module type LOCATOR = sig
  val name : string
  type t

  val create : scheme:string -> location:string -> t
  val scheme : t -> string
  val location : t -> string
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

type sk_locator = Sk_locator of { scheme : string ; location : string }
type pk_locator = Pk_locator of { scheme : string ; location : string }

module Sk_locator = struct
  let name = "secret key"
  type t = sk_locator

  let create ~scheme ~location =
    Sk_locator { scheme ; location }

  let scheme (Sk_locator { scheme }) = scheme
  let location (Sk_locator { location }) = location

  let to_string (Sk_locator { scheme ; location }) =
    scheme ^ ":" ^  location

  let pp ppf (Sk_locator { scheme ; location }) =
    Format.pp_print_string ppf (scheme ^ ":" ^ location)
end

module Pk_locator = struct
  let name = "public key"
  type t = pk_locator

  let create ~scheme ~location =
    Pk_locator { scheme ; location }

  let scheme (Pk_locator { scheme }) = scheme
  let location (Pk_locator { location }) = location

  let to_string (Pk_locator { scheme ; location }) =
    scheme ^ ":" ^  location

  let pp ppf (Pk_locator { scheme ; location }) =
    Format.pp_print_string ppf (scheme ^ ":" ^ location)
end

module type KEY = sig
  type t
  val to_b58check : t -> string
  val of_b58check_exn : string -> t
end

module Locator (K : KEY) (L : LOCATOR) = struct
  include L

  let of_unencrypted k =
    L.create ~scheme:"unencrypted"
      ~location:(K.to_b58check k)

  let of_string s =
    match String.index s ':' with
    | exception Not_found ->
        of_unencrypted (K.of_b58check_exn s)
    | i ->
        let len = String.length s in
        create
          ~scheme:(String.sub s 0 i)
          ~location:(String.sub s (i+1) (len-i-1))

  let of_source s =  return (of_string s)
  let to_source t = return (to_string t)

  let encoding = Data_encoding.(conv to_string of_string string)
end

module Secret_key_locator = Locator(Ed25519.Secret_key)(Sk_locator)
module Secret_key = Client_aliases.Alias (Secret_key_locator)
module Public_key_locator = Locator(Ed25519.Public_key)(Pk_locator)
module Public_key = Client_aliases.Alias (Public_key_locator)

module type SIGNER = sig
  type secret_key
  type public_key
  val scheme : string
  val title : string
  val description : string
  val sk_locator_of_human_input :
    Client_commands.logging_wallet ->
    string list -> sk_locator tzresult Lwt.t
  val pk_locator_of_human_input :
    Client_commands.logging_wallet ->
    string list -> pk_locator tzresult Lwt.t
  val sk_of_locator : sk_locator -> secret_key tzresult Lwt.t
  val pk_of_locator : pk_locator -> public_key tzresult Lwt.t
  val sk_to_locator : secret_key -> sk_locator Lwt.t
  val pk_to_locator : public_key -> pk_locator Lwt.t
  val neuterize : secret_key -> public_key Lwt.t
  val public_key : public_key -> Ed25519.Public_key.t Lwt.t
  val public_key_hash : public_key -> Ed25519.Public_key_hash.t Lwt.t
  val sign : secret_key -> MBytes.t -> Ed25519.Signature.t tzresult Lwt.t
end

let signers_table : (string, (module SIGNER)) Hashtbl.t = Hashtbl.create 13
let register_signer signer =
  let module Signer = (val signer : SIGNER) in
  Hashtbl.replace signers_table Signer.scheme signer

let find_signer_for_key ~scheme =
  match Hashtbl.find signers_table scheme with
  | exception Not_found -> error (Unregistered_key_scheme scheme)
  | signer -> ok signer

let sign ((Sk_locator { scheme }) as skloc) buf =
  Lwt.return (find_signer_for_key ~scheme) >>=? fun signer ->
  let module Signer = (val signer : SIGNER) in
  Signer.sk_of_locator skloc >>=? fun t ->
  Signer.sign t buf

let append loc buf =
  sign loc buf >>|? fun signature ->
  MBytes.concat buf (Ed25519.Signature.to_bytes signature)

let gen_keys ?(force=false) ?seed (cctxt : #Client_commands.wallet) name =
  let seed =
    match seed with
    | None -> Ed25519.Seed.generate ()
    | Some s -> s in
  let _, public_key, secret_key = Ed25519.generate_seeded_key seed in
  Secret_key.add ~force cctxt name
    (Secret_key_locator.of_unencrypted secret_key) >>=? fun () ->
  Public_key.add ~force cctxt name
    (Public_key_locator.of_unencrypted public_key) >>=? fun () ->
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
              Secret_key.add ~force cctxt name
                (Secret_key_locator.of_unencrypted secret_key) >>=? fun () ->
              Public_key.add ~force cctxt name
                (Public_key_locator.of_unencrypted public_key) >>=? fun () ->
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

let get_key (cctxt : #Client_commands.wallet) pkh =
  Public_key_hash.rev_find cctxt pkh >>=? function
  | None -> failwith "no keys for the source contract manager"
  | Some n ->
      Public_key.find cctxt n >>=? fun pk ->
      Secret_key.find cctxt n >>=? fun sk ->
      let scheme = Secret_key_locator.scheme sk in
      Lwt.return (find_signer_for_key ~scheme) >>=? fun signer ->
      let module Signer = (val signer : SIGNER) in
      Signer.pk_of_locator pk >>=? fun pk ->
      Signer.public_key pk >>= fun pk ->
      return (n, pk, sk)

let get_keys (wallet : #Client_commands.wallet) =
  Secret_key.load wallet >>=? fun sks ->
  Lwt_list.filter_map_s begin fun (name, sk) ->
    begin
      Public_key.find wallet name >>=? fun pk ->
      Public_key_hash.find wallet name >>=? fun pkh ->
      let scheme = Public_key_locator.scheme pk in
      Lwt.return
        (find_signer_for_key ~scheme) >>=? fun signer ->
      let module Signer = (val signer : SIGNER) in
      Signer.pk_of_locator pk >>=? fun pk ->
      Signer.public_key pk >>= fun pk ->
      return (name, pkh, pk, sk)
    end >>= function
    | Ok r -> Lwt.return (Some r)
    | Error _ -> Lwt.return_none
  end sks >>= fun keys ->
  return keys

let list_keys cctxt =
  Public_key_hash.load cctxt >>=? fun l ->
  map_s
    (fun (name, pkh) ->
       Public_key.find_opt cctxt name >>=? fun pkm ->
       Secret_key.find_opt cctxt name >>=? fun pks ->
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

let force_switch =
  Client_commands.force_switch ~doc:"overwrite existing keys" ()

let group =
  { Cli_entries.name = "keys" ;
    title = "Commands for managing the wallet of cryptographic keys" }

let commands () =
  let open Cli_entries in
  let show_private_switch =
    switch
      ~parameter:"-show-secret"
      ~doc:"show the private key" in
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
      (fun () (cctxt : Client_commands.full_context) ->
         let schemes = Hashtbl.fold (fun k _ a -> k :: a) signers_table [] in
         let schemes = List.sort String.compare schemes in
         Lwt_list.iter_s
           (fun n ->
              let (module S : SIGNER) = Hashtbl.find signers_table n in
              cctxt#message "@[<v 2>Scheme `%s`: %s@,@[<hov 0>%a@]@]"
                n S.title Format.pp_print_text S.description)
           schemes >>= return) ;

    command ~group ~desc: "Generate a pair of (unencrypted) keys."
      (args1 Secret_key.force_switch)
      (prefixes [ "gen" ; "keys" ]
       @@ Secret_key.fresh_alias_param
       @@ stop)
      (fun force name (cctxt : Client_commands.full_context) ->
         Secret_key.of_fresh cctxt force name >>=? fun name ->
         gen_keys ~force cctxt name) ;

    command ~group ~desc: "Generate (unencrypted) keys including the given string."
      (args2
         (switch ~doc:"the key must begin with tz1[word]" ~parameter:"-prefix")
         force_switch)
      (prefixes [ "gen" ; "vanity" ; "keys" ]
       @@ Public_key_hash.fresh_alias_param
       @@ prefix "matching"
       @@ (seq_of_param @@ string ~name:"words" ~desc:"string key must contain one of these words"))
      (fun (prefix, force) name containing cctxt ->
         Public_key_hash.of_fresh cctxt force name >>=? fun name ->
         gen_keys_containing ~force ~prefix ~containing ~name cctxt) ;

    command ~group ~desc: "Add a secret key to the wallet."
      (args1 Secret_key.force_switch)
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
         Lwt.return (find_signer_for_key ~scheme) >>=? fun signer ->
         let module Signer = (val signer : SIGNER) in
         Signer.sk_locator_of_human_input
           (cctxt :> Client_commands.logging_wallet) spec >>=? fun skloc ->
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
      (args1 Public_key.force_switch)
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
         Lwt.return (find_signer_for_key ~scheme) >>=? fun signer ->
         let module Signer = (val signer : SIGNER) in
         Signer.pk_locator_of_human_input
           (cctxt :> Client_commands.logging_wallet) location >>=? fun pkloc ->
         Signer.pk_of_locator pkloc >>=? fun pk ->
         Signer.public_key_hash pk >>= fun pkh ->
         Public_key_hash.add ~force cctxt name pkh >>=? fun () ->
         Public_key.add ~force cctxt name pkloc) ;

    command ~group ~desc: "Add an identity to the wallet."
      (args1 Public_key.force_switch)
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
      (fun () (cctxt : Client_commands.full_context) ->
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
      (fun show_private (name, _) (cctxt : Client_commands.full_context) ->
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
                 Lwt.return (find_signer_for_key ~scheme) >>=? fun signer ->
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
      (args1 (Client_commands.force_switch ~doc:"you got to use the force for that" ()))
      (fixed [ "forget" ; "all" ; "keys" ])
      (fun force cctxt ->
         fail_unless force
           (failure "this can only used with option -force") >>=? fun () ->
         Public_key.set cctxt [] >>=? fun () ->
         Secret_key.set cctxt [] >>=? fun () ->
         Public_key_hash.set cctxt []) ;

  ]
