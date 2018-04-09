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

  type secret_key = Ed25519.Secret_key.t
  type public_key = Ed25519.Public_key.t

  (* https://tools.ietf.org/html/rfc2898#section-4.1 *)
  let salt_len = 8

  (* Fixed zero nonce *)
  let nonce = Crypto_box.zero_nonce

  (* skloc -> Ed25519.Secret_key.t *)
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
        let key = Crypto_box.Secretbox.of_bytes_exn (pbkdf ~password ~salt) in
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
    cctxt#prompt_password "Enter password for encrypted key %s: " name >>= fun password ->
    let password = MBytes.of_string password in
    let key = pbkdf ~salt ~password in
    let key = Crypto_box.Secretbox.of_bytes_exn key in
    match Crypto_box.Secretbox.box_open key skenc nonce with
    | None -> passwd_ask_loop cctxt ~name ~salt ~skenc
    | Some decrypted_sk ->
        Lwt.return (password, (Ed25519.Secret_key.of_bytes_exn decrypted_sk))

  let ask_all_passwords (cctxt : #Client_context.io_wallet) sks =
    Lwt_list.fold_left_s begin fun a (name, skloc) ->
      if Secret_key_locator.scheme skloc <> scheme then
        Lwt.return a
      else
        let location = Secret_key_locator.location skloc in
        match Base58.safe_decode location with
        | None -> Lwt.fail Exit
        | Some payload ->
            let salt, skenc = salt_skenc_of_skloc payload in
            match decrypt_sk skenc salt a with
            | Some sk ->
                Hashtbl.replace decrypted_sks location
                  (Ed25519.Secret_key.of_bytes_exn sk) ;
                Lwt.return a
            | None ->
                passwd_ask_loop
                  cctxt ~name ~salt ~skenc >>= fun (passwd, decrypted_sk) ->
                Hashtbl.replace decrypted_sks location decrypted_sk ;
                Lwt.return (passwd :: a)
    end [] sks

  let init cctxt =
    Secret_key.load cctxt >>=? fun sks ->
    Lwt.try_bind
      (fun () -> ask_all_passwords cctxt sks)
      (fun _ -> return ())
      (fun _ -> failwith "Corrupted secret key database. Aborting.")

  let input_new_passphrase (cctxt : #Client_context.io_wallet) =
    cctxt#prompt_password "Enter passphrase to encrypt your key: " >>= fun password ->
    cctxt#prompt_password "Confirm passphrase: " >>= fun confirm ->
    if password <> confirm then
      failwith "Passphrases do not match."
    else return password

  let encrypt_sk cctxt sk =
    input_new_passphrase cctxt >>=? fun password ->
    let password = MBytes.of_string password in
    let salt = Rand.generate salt_len in
    let key = Crypto_box.Secretbox.of_bytes_exn (pbkdf ~password ~salt) in
    let msg = Ed25519.Secret_key.to_bytes sk in
    let encrypted_passwd = Crypto_box.Secretbox.box key msg nonce in
    let payload = MBytes.(to_string (concat salt encrypted_passwd)) in
    let location = Base58.safe_encode payload in
    Hashtbl.replace decrypted_sks location sk ;
    return (Secret_key_locator.create ~scheme ~location)

  let rec get_boolean_answer (cctxt : #Client_context.io_wallet) ~default ~msg =
    let prompt = if default then "(Y/n/q)" else "(y/N/q)" in
    cctxt#prompt "%s %s: " msg prompt >>= fun gen ->
    match default, String.lowercase_ascii gen with
    | default, "" -> return default
    | _, "y" -> return true
    | _, "n" -> return false
    | _, "q" -> failwith "Exit by user request."
    | _ -> get_boolean_answer cctxt ~msg ~default

  let rec sk_of_mnemonic (cctxt : #Client_context.io_wallet) =
    cctxt#prompt "Enter the e-mail used for the paper wallet: " >>= fun email ->
    let rec loop_words acc i =
      if i > 14 then Lwt.return (List.rev acc) else
        cctxt#prompt_password "Enter word %d: " i >>= fun word ->
        match Bip39.index_of_word word with
        | None -> loop_words acc i
        | Some wordidx -> loop_words (wordidx :: acc) (succ i) in
    loop_words [] 0 >>= fun words ->
    match Bip39.of_indices words with
    | None -> assert false
    | Some t ->
        cctxt#prompt_password
          "Enter the password used for the paper wallet: " >>= fun password ->
        let sk = Bip39.to_seed ~passphrase:(password ^ email) t in
        let sk = Cstruct.(to_bigarray (sub sk 0 32)) in
        let sk = Ed25519.Secret_key.of_bytes_exn sk in
        let pk = Ed25519.Secret_key.to_public_key sk in
        let pkh = Ed25519.Public_key.hash pk in
        let msg = Format.asprintf
            "Your public Tezos address is %a is that correct?"
            Ed25519.Public_key_hash.pp pkh in
        get_boolean_answer cctxt ~msg ~default:true >>=? function
        | true -> return sk
        | false -> sk_of_mnemonic cctxt

  let sk_locator_of_human_input cctxt = function
    | sk :: _ ->
        Lwt.return (Ed25519.Secret_key.of_b58check sk) >>=? fun sk ->
        encrypt_sk cctxt sk
    | [] -> begin
        get_boolean_answer
          cctxt ~msg:"Generate a new key" ~default:true >>=? function
        | true ->
            let _, _, sk = Ed25519.generate_key () in
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
    | pk :: _ -> return (Public_key_locator.create ~scheme ~location:pk)

  let sk_of_locator (Sk_locator { location }) =
    match Hashtbl.find decrypted_sks location with
    | exception Not_found -> failwith "Unknown secret key location."
    | sk -> return sk

  let pk_of_locator (Pk_locator { location }) =
    Lwt.return (Ed25519.Public_key.of_b58check location)

  let sk_to_locator sk =
    Secret_key_locator.create
      ~scheme ~location:(Ed25519.Secret_key.to_b58check sk) |>
    Lwt.return

  let pk_to_locator pk =
    Public_key_locator.create
      ~scheme ~location:(Ed25519.Public_key.to_b58check pk) |>
    Lwt.return

  let neuterize x = Lwt.return (Ed25519.Secret_key.to_public_key x)
  let public_key x = Lwt.return x
  let public_key_hash x = Lwt.return (Ed25519.Public_key.hash x)
  let sign t buf = return (Ed25519.sign t buf)
end

let () =
  register_signer (module Encrypted_signer)
