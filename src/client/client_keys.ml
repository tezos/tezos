(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

module Ed25519 = Environment.Ed25519

module Public_key_hash = Client_aliases.Alias (struct
    type t = Ed25519.Public_key_hash.t
    let encoding = Ed25519.Public_key_hash.encoding
    let of_source _ s = Lwt.return (Ed25519.Public_key_hash.of_b48check s)
    let to_source _ p = Lwt.return (Ed25519.Public_key_hash.to_b48check p)
    let name = "public key hash"
  end)

module Public_key = Client_aliases.Alias (struct
    type t = Ed25519.public_key
    let encoding = Ed25519.public_key_encoding
    let of_source _ s =
      Lwt.return (Sodium.Sign.Bytes.to_public_key
                (Bytes.of_string B64.(decode ~alphabet:uri_safe_alphabet s)))
    let to_source _ p =
      Lwt.return B64.(encode ~alphabet:uri_safe_alphabet
                    (Bytes.to_string (Sodium.Sign.Bytes.of_public_key p)))
    let name = "public key"
  end)

module Secret_key = Client_aliases.Alias (struct
    type t = Ed25519.secret_key
    let encoding = Ed25519.secret_key_encoding
    let of_source _ s =
      Lwt.return (Sodium.Sign.Bytes.to_secret_key
                (Bytes.of_string B64.(decode ~alphabet:uri_safe_alphabet s)))
    let to_source _ p =
      Lwt.return B64.(encode ~alphabet:uri_safe_alphabet
                    (Bytes.to_string (Sodium.Sign.Bytes.of_secret_key p)))
    let name = "secret key"
  end)

let gen_keys cctxt name =
  let secret_key, public_key = Sodium.Sign.random_keypair () in
  Secret_key.add cctxt name secret_key >>= fun () ->
  Public_key.add cctxt name public_key >>= fun () ->
  Public_key_hash.add cctxt name (Ed25519.hash public_key) >>= fun () ->
  cctxt.message "I generated a brand new pair of keys under the name '%s'." name

let check_keys_consistency pk sk =
  let message = MBytes.of_string "Voulez-vous coucher avec moi, ce soir ?" in
  let signature = Ed25519.sign sk message in
  Ed25519.check_signature pk signature message

let get_key cctxt pkh =
  Public_key_hash.rev_find cctxt pkh >>= function
  | None -> cctxt.error "no keys for the source contract manager"
  | Some n ->
      Public_key.find cctxt n >>= fun pk ->
      Secret_key.find cctxt n >>= fun sk ->
      return (n, pk, sk)

let group =
  { Cli_entries.name = "keys" ;
    title = "Commands for managing cryptographic keys" }

let commands () =
  let open Cli_entries in
  [ command ~group ~desc: "generate a pair of keys"
      (prefixes [ "gen" ; "keys" ]
       @@ Secret_key.fresh_alias_param
       @@ stop)
      (fun name cctxt -> gen_keys cctxt name) ;
    command ~group ~desc: "add a secret key to the wallet"
      (prefixes [ "add" ; "secret" ; "key" ]
       @@ Secret_key.fresh_alias_param
       @@ Secret_key.source_param
       @@ stop)
      (fun name sk cctxt ->
         Lwt.catch (fun () ->
             Public_key.find cctxt name >>= fun pk ->
             if check_keys_consistency pk sk || Client_config.force#get then
               Secret_key.add cctxt name sk
             else
               cctxt.error
                 "public and secret keys '%s' don't correspond, \
                  please don't use -force true" name)
           (function
             | Not_found ->
                 cctxt.error
                   "no public key named '%s', add it before adding the secret key" name
             | exn -> Lwt.fail exn)) ;
    command ~group ~desc: "add a public key to the wallet"
      (prefixes [ "add" ; "public" ; "key" ]
       @@ Public_key.fresh_alias_param
       @@ Public_key.source_param
       @@ stop)
      (fun name key cctxt ->
         Public_key_hash.add cctxt name (Ed25519.hash key) >>= fun () ->
         Public_key.add cctxt name key) ;
    command ~group ~desc: "add an ID a public key hash to the wallet"
      (prefixes [ "add" ; "identity" ]
       @@ Public_key_hash.fresh_alias_param
       @@ Public_key_hash.source_param
       @@ stop)
      (fun name hash cctxt ->
         Public_key_hash.add cctxt name hash) ;
    command ~group ~desc: "list all public key hashes and associated keys"
      (fixed [ "list" ; "known" ; "identities" ])
      (fun cctxt ->
         Public_key_hash.load cctxt >>= fun l ->
         Lwt_list.iter_s (fun (name, pkh) ->
             Public_key.mem cctxt name >>= fun pkm ->
             Secret_key.mem cctxt name >>= fun pks ->
             Public_key_hash.to_source cctxt pkh >>= fun v ->
             cctxt.message "%s: %s%s%s" name v
               (if pkm then " (public key known)" else "")
               (if pks then " (secret key known)" else ""))
           l) ;
    command ~group ~desc: "forget all keys"
      (fixed [ "forget" ; "all" ; "keys" ])
      (fun cctxt ->
         if not Client_config.force#get then
           cctxt.Client_commands.error "this can only used with option -force true"
         else
           Public_key.save cctxt [] >>= fun () ->
           Secret_key.save cctxt [] >>= fun () ->
           Public_key_hash.save cctxt []) ;
     ]
