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

module Seed = struct

  let to_hex s =
    Sodium.Sign.Bytes.of_seed s
    |> Bytes.to_string
    |> Hex_encode.hex_encode

  let of_hex s =
    Hex_encode.hex_decode s
    |> Bytes.of_string
    |> Sodium.Sign.Bytes.to_seed

  let generate () =
    (* Seed is 32 bytes long *)
    Sodium.Random.Bytes.generate Sodium.Sign.seed_size
    |> Sodium.Sign.Bytes.to_seed

  let extract =
    Sodium.Sign.secret_key_to_seed
end

let gen_keys ?seed cctxt name =
  let seed =
    match seed with
    | None -> Seed.generate ()
    | Some s -> s in
  let secret_key, public_key = Sodium.Sign.seed_keypair seed in
  Secret_key.add cctxt name secret_key >>=? fun () ->
  Public_key.add cctxt name public_key >>=? fun () ->
  Public_key_hash.add
    cctxt name (Ed25519.Public_key.hash public_key) >>=? fun () ->
  cctxt.message
    "I generated a brand new pair of keys under the name '%s'." name >>= fun () ->
  return ()

let check_keys_consistency pk sk =
  let message = MBytes.of_string "Voulez-vous coucher avec moi, ce soir ?" in
  let signature = Ed25519.sign sk message in
  Ed25519.Signature.check pk signature message

let get_key cctxt pkh =
  Public_key_hash.rev_find cctxt pkh >>=? function
  | None -> cctxt.error "no keys for the source contract manager"
  | Some n ->
      Public_key.find cctxt n >>=? fun pk ->
      Secret_key.find cctxt n >>=? fun sk ->
      return (n, pk, sk)

let get_keys cctxt =
  Secret_key.load cctxt >>=? fun sks ->
  Lwt_list.filter_map_s
    (fun (name, sk) ->
       begin
         Public_key.find cctxt name >>=? fun pk ->
         Public_key_hash.find cctxt name >>=? fun pkh ->
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

let group =
  { Cli_entries.name = "keys" ;
    title = "Commands for managing cryptographic keys" }

let commands () =
  let open Cli_entries in
  let open Client_commands in
  [

    command ~group ~desc: "generate a pair of keys"
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
         Public_key.find_opt cctxt name >>=? function
         | None ->
             failwith
               "no public key named '%s', add it before adding the secret key"
               name
         | Some pk ->
             fail_unless
               (check_keys_consistency pk sk || cctxt.config.force)
               (failure
                  "public and secret keys '%s' don't correspond, \
                   please don't use -force true" name) >>=? fun () ->
             Secret_key.add cctxt name sk) ;

    command ~group ~desc: "add a public key to the wallet"
      (prefixes [ "add" ; "public" ; "key" ]
       @@ Public_key.fresh_alias_param
       @@ Public_key.source_param
       @@ stop)
      (fun name key cctxt ->
         Public_key_hash.add cctxt
           name (Ed25519.Public_key.hash key) >>=? fun () ->
         Public_key.add cctxt name key) ;

    command ~group ~desc: "add an ID a public key hash to the wallet"
      (prefixes [ "add" ; "identity" ]
       @@ Public_key_hash.fresh_alias_param
       @@ Public_key_hash.source_param
       @@ stop)
      (fun name hash cctxt -> Public_key_hash.add cctxt name hash) ;

    command ~group ~desc: "list all public key hashes and associated keys"
      (fixed [ "list" ; "known" ; "identities" ])
      (fun cctxt ->
         list_keys cctxt >>=? fun l ->
         iter_s
           (fun (name, pkh, pkm, pks) ->
              Public_key_hash.to_source cctxt pkh >>=? fun v ->
              cctxt.message "%s: %s%s%s" name v
                (if pkm then " (public key known)" else "")
                (if pks then " (secret key known)" else "") >>= fun () ->
              return ())
           l) ;

    command ~group ~desc: "forget all keys"
      (fixed [ "forget" ; "all" ; "keys" ])
      (fun cctxt ->
         fail_unless cctxt.config.force
           (failure "this can only used with option -force true") >>=? fun () ->
         Public_key.save cctxt [] >>=? fun () ->
         Secret_key.save cctxt [] >>=? fun () ->
         Public_key_hash.save cctxt []) ;

  ]
