(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2017.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Client_keys

let scheme = "encrypted"

module Raw = struct

  (* https://tools.ietf.org/html/rfc2898#section-4.1 *)
  let salt_len = 8

  (* Fixed zero nonce *)
  let nonce = Crypto_box.zero_nonce

  let pbkdf ~salt ~password =
    Cstruct.to_bigarray
      (Pbkdf.pbkdf2 ~prf:`SHA512 ~count:2048 ~dk_len:32l
         ~salt: (Cstruct.of_bigarray salt)
         ~password: (Cstruct.of_bigarray password))

  let encrypt ~password sk =
    let salt = Rand.generate salt_len in
    let key = Crypto_box.Secretbox.unsafe_of_bytes (pbkdf ~password ~salt) in
    let msg = Data_encoding.Binary.to_bytes_exn Signature.Secret_key.encoding sk in
    let encrypted_passwd = Crypto_box.Secretbox.box key msg nonce in
    MBytes.concat "" [ salt ; encrypted_passwd ]

  let decrypt ~password ~encrypted_sk =
    let len = MBytes.length encrypted_sk in
    let salt = MBytes.sub encrypted_sk 0 salt_len in
    let encrypted_sk = MBytes.sub encrypted_sk salt_len (len - salt_len) in
    let key = Crypto_box.Secretbox.unsafe_of_bytes (pbkdf ~salt ~password) in
    match Crypto_box.Secretbox.box_open key encrypted_sk nonce with
    | None -> return None
    | Some bytes ->
        match Data_encoding.Binary.of_bytes Signature.Secret_key.encoding bytes with
        | None -> failwith "... FIXME ... D" (* corrupted data *)
        | Some sk -> return (Some sk)

end

let decrypted = Hashtbl.create 13
let passwords = ref []

let rec interactive_decrypt_loop
    (cctxt : #Client_context.prompter)
    ?name ~encrypted_sk =
  begin
    match name with
    | None ->
        cctxt#prompt_password
          "Enter password for encrypted key: "
    | Some name ->
        cctxt#prompt_password
          "Enter password for encrypted key \"%s\": " name
  end >>=? fun password ->
  Raw.decrypt ~password ~encrypted_sk >>=? function
  | None ->
      interactive_decrypt_loop cctxt ?name ~encrypted_sk
  | Some sk ->
      passwords := password :: !passwords ;
      return sk

let rec noninteractice_decrypt_loop ~encrypted_sk = function
  | [] -> return None
  | password :: passwords ->
      Raw.decrypt ~password ~encrypted_sk >>=? function
      | None -> noninteractice_decrypt_loop ~encrypted_sk passwords
      | Some sk -> return (Some sk)

let decrypt_payload cctxt ?name encrypted_sk =
  match Base58.safe_decode encrypted_sk with
  | None -> failwith "... FIXME ... A"
  | Some encrypted_sk ->
      let encrypted_sk = MBytes.of_string encrypted_sk in
      noninteractice_decrypt_loop ~encrypted_sk !passwords >>=? function
      | Some sk -> return sk
      | None -> interactive_decrypt_loop cctxt ?name ~encrypted_sk

let decrypt cctxt ?name sk_uri =
  let payload = Uri.path (sk_uri : sk_uri :> Uri.t) in
  decrypt_payload cctxt ?name payload >>=? fun sk ->
  Hashtbl.replace decrypted sk_uri sk ;
  return sk

let decrypt_all (cctxt : #Client_context.io_wallet) =
  Secret_key.load cctxt >>=? fun sks ->
  iter_s begin fun (name, sk_uri) ->
    if Uri.scheme (sk_uri : sk_uri :> Uri.t) <> Some scheme then
      return ()
    else
      decrypt cctxt ~name sk_uri >>=? fun _ ->
      return ()
  end sks

let rec read_passphrase (cctxt : #Client_context.io) =
  cctxt#prompt_password
    "Enter passphrase to encrypt your key: " >>=? fun password ->
  cctxt#prompt_password
    "Confirm passphrase: " >>=? fun confirm ->
  if not (MBytes.equal password confirm) then
    cctxt#message "Passphrases do not match." >>= fun () ->
    read_passphrase cctxt
  else
    return password

let encrypt cctxt sk =
  read_passphrase cctxt >>=? fun password ->
  let payload = Raw.encrypt ~password sk in
  let path = Base58.safe_encode (MBytes.to_string payload) in
  let sk_uri = Client_keys.make_sk_uri (Uri.make ~scheme ~path ()) in
  Hashtbl.replace decrypted sk_uri sk ;
  return sk_uri

module Make(C : sig val cctxt: Client_context.prompter end) = struct

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

  let public_key = Unencrypted.public_key
  let public_key_hash = Unencrypted.public_key_hash
  let neuterize sk_uri =
    decrypt C.cctxt sk_uri >>=? fun sk ->
    return (Unencrypted.make_pk (Signature.Secret_key.to_public_key sk))
  let sign ?watermark sk_uri buf =
    decrypt C.cctxt sk_uri >>=? fun sk ->
    return (Signature.sign ?watermark sk buf)

end
