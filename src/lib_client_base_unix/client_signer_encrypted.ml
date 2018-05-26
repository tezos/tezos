(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_keys

module Encrypted_signer : SIGNER = struct
  let scheme = "encrypted"

  let title =
    "Built-in signer using encrypted keys."

  let description =
    "If you try to import a secret key without additional argument, you will \
     be asked to either generate a new key, or to import the elements \
     from your fundraiser paper wallet.\n\
     If you add an argument when importing a secret key, \
     the format is the raw Base58-encoded key (starting with 'edsk').\n\
     The format for importing public keys is the raw Base58-encoded \
     key (starting with 'edpk')."

  type secret_key = Signature.Secret_key.t
  type public_key = Signature.Public_key.t

  (* https://tools.ietf.org/html/rfc2898#section-4.1 *)
  let salt_len = 8

  (* Fixed zero nonce *)
  let nonce = Crypto_box.zero_nonce

  (* skloc -> Signature.Secret_key.t *)
  let decrypted_sks = Hashtbl.create 13

  let pbkdf ~salt ~password =
    let open Cstruct in
    let salt = of_bigarray salt in
    let password = of_bigarray password in
    to_bigarray
      (Pbkdf.pbkdf2 ~prf:`SHA512 ~count:2048 ~dk_len:32l ~salt ~password)

  let rec decrypt_sk sk salt = function
    | [] -> None
    | password :: pws ->
        let key = Crypto_box.Secretbox.unsafe_of_bytes (pbkdf ~password ~salt) in
        match Crypto_box.Secretbox.box_open key sk nonce with
        | None -> decrypt_sk sk salt pws
        | Some sk -> Some sk

  let salt_skenc_of_skloc skloc =
    let open Cstruct in
    let skloc = of_string skloc in
    let len = len skloc in
    let salt = sub skloc 0 salt_len in
    let skenc = sub skloc salt_len (len - salt_len) in
    to_bigarray salt, to_bigarray skenc

  let rec passwd_ask_loop (cctxt : #Client_context.io_wallet) ~name ~salt ~skenc =
    cctxt#prompt_password "Enter password for encrypted key %s: " name >>=? fun password ->
    let key = pbkdf ~salt ~password in
    let key = Crypto_box.Secretbox.unsafe_of_bytes key in
    match Crypto_box.Secretbox.box_open key skenc nonce with
    | None -> passwd_ask_loop cctxt ~name ~salt ~skenc
    | Some decrypted_sk ->
        return (password, (Data_encoding.Binary.of_bytes_exn
                             Signature.Secret_key.encoding
                             decrypted_sk))

  let ask_all_passwords (cctxt : #Client_context.io_wallet) sks =
    fold_left_s begin fun a (name, skloc) ->
      if Secret_key_locator.scheme skloc <> scheme then
        return a
      else
        match Secret_key_locator.location skloc with
        |location :: _ -> begin
            match Base58.safe_decode location with
            | None -> Lwt.fail Exit
            | Some payload ->
                let salt, skenc = salt_skenc_of_skloc payload in
                match decrypt_sk skenc salt a with
                | Some sk ->
                    Hashtbl.replace decrypted_sks location
                      (Data_encoding.Binary.of_bytes_exn Signature.Secret_key.encoding sk);
                    return a
                | None ->
                    passwd_ask_loop
                      cctxt ~name ~salt ~skenc >>=? fun (passwd, decrypted_sk) ->
                    Hashtbl.replace decrypted_sks location decrypted_sk ;
                    return (passwd :: a)
          end
        |_ -> Lwt.fail Exit
    end [] sks

  let init cctxt =
    Secret_key.load cctxt >>=? fun sks ->
    Lwt.try_bind
      (fun () -> ask_all_passwords cctxt sks)
      (fun _ -> return ())
      (fun _ -> failwith "Corrupted secret key database. Aborting.")

  let input_new_passphrase (cctxt : #Client_context.io_wallet) =
    cctxt#prompt_password "Enter passphrase to encrypt your key: " >>=? fun password ->
    cctxt#prompt_password "Confirm passphrase: " >>=? fun confirm ->
    if password <> confirm then
      failwith "Passphrases do not match."
    else return password

  let encrypt_sk cctxt sk =
    input_new_passphrase cctxt >>=? fun password ->
    let salt = Rand.generate salt_len in
    let key = Crypto_box.Secretbox.unsafe_of_bytes (pbkdf ~password ~salt) in
    let msg = Data_encoding.Binary.to_bytes_exn Signature.Secret_key.encoding sk in
    let encrypted_passwd = Crypto_box.Secretbox.box key msg nonce in
    let payload = MBytes.(to_string (concat "" [salt; encrypted_passwd])) in
    let location = Base58.safe_encode payload in
    Hashtbl.replace decrypted_sks location sk ;
    return (Secret_key_locator.create ~scheme ~location:[location])

  let rec get_boolean_answer (cctxt : #Client_context.io_wallet) ~default ~msg =
    let prompt = if default then "(Y/n/q)" else "(y/N/q)" in
    cctxt#prompt "%s %s: " msg prompt >>=? fun gen ->
    match default, String.lowercase_ascii gen with
    | default, "" -> return default
    | _, "y" -> return true
    | _, "n" -> return false
    | _, "q" -> failwith "Exit by user request."
    | _ -> get_boolean_answer cctxt ~msg ~default

  let rec sk_of_mnemonic (cctxt : #Client_context.io_wallet) =
    cctxt#prompt "Enter the e-mail used for the paper wallet: " >>=? fun email ->
    let rec loop_words acc i =
      if i > 14 then return (List.rev acc) else
        cctxt#prompt_password "Enter word %d: " i >>=? fun word ->
        match Bip39.index_of_word (MBytes.to_string word) with
        | None -> loop_words acc i
        | Some wordidx -> loop_words (wordidx :: acc) (succ i) in
    loop_words [] 0 >>=? fun words ->
    match Bip39.of_indices words with
    | None -> assert false
    | Some t ->
        cctxt#prompt_password
          "Enter the password used for the paper wallet: " >>=? fun password ->
        (* TODO: unicode normalization (NFKD)... *)
        let sk = Bip39.to_seed ~passphrase:(email ^ MBytes.to_string password) t in
        let sk = Cstruct.(to_bigarray (sub sk 0 32)) in
        let sk : Signature.Secret_key.t =
          Ed25519
            (Data_encoding.Binary.of_bytes_exn Ed25519.Secret_key.encoding sk) in
        let pk = Signature.Secret_key.to_public_key sk in
        let pkh = Signature.Public_key.hash pk in
        let msg = Format.asprintf
            "Your public Tezos address is %a is that correct?"
            Signature.Public_key_hash.pp pkh in
        get_boolean_answer cctxt ~msg ~default:true >>=? function
        | true -> return sk
        | false -> sk_of_mnemonic cctxt

  let sk_locator_of_human_input cctxt = function
    | sk :: _ ->
        Lwt.return (Signature.Secret_key.of_b58check sk) >>=? fun sk ->
        encrypt_sk cctxt sk
    | [] -> begin
        get_boolean_answer
          cctxt ~msg:"Generate a new key" ~default:true >>=? function
        | true ->
            let _, _, sk = Signature.generate_key () in
            encrypt_sk cctxt sk
        | false ->
            get_boolean_answer cctxt
              ~msg:"Import key from fundraiser" ~default:true >>=? function
            | false -> failwith "Goodbye."
            | true ->
                sk_of_mnemonic cctxt >>=? fun sk ->
                encrypt_sk cctxt sk
      end

  let pk_locator_of_human_input _cctxt = function
    | [] -> failwith "Missing public key argument."
    | pk :: _ -> return (Public_key_locator.create ~scheme ~location:[pk])

  let sk_of_locator = function
    | (Sk_locator { location = [location] }) -> begin
        match Hashtbl.find decrypted_sks location with
        | exception Not_found -> failwith "Unknown secret key location."
        | sk -> return sk
      end
    | (Sk_locator { location = _ }) ->
        failwith "Wrong location type."

  let pk_of_locator = function
    |(Pk_locator { location = [location] }) ->
        Lwt.return (Signature.Public_key.of_b58check location)
    |(Pk_locator { location = _ }) ->
        failwith "Wrong location type."

  let sk_to_locator sk =
    Secret_key_locator.create
      ~scheme ~location:[Signature.Secret_key.to_b58check sk] |>
    Lwt.return

  let pk_to_locator pk =
    Public_key_locator.create
      ~scheme ~location:[Signature.Public_key.to_b58check pk] |>
    Lwt.return

  let neuterize x = Lwt.return (Signature.Secret_key.to_public_key x)
  let public_key x = return x
  let public_key_hash x = return (Signature.Public_key.hash x)
  let sign ?watermark t buf = return (Signature.sign ?watermark t buf)
end

let () =
  register_signer (module Encrypted_signer)
