(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2016.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

type account = {
  public_key_hash : Ed25519.Public_key_hash.t ;
  public_key : Ed25519.public_key ;
  secret_key : Ed25519.secret_key ;
}

(* FIXME: when incresing wealth *10, the node is very slow to initialize...
   this should be investigated... *)
let wealth = Tez_repr.of_cents_exn 2_000_000_00L

let make ~raw_secret_key ~raw_public_key =
  let raw_secret_key =
    raw_secret_key
    |> Hex_encode.hex_decode
    |> MBytes.of_string in
  let raw_public_key =
    raw_public_key
    |> Hex_encode.hex_decode
    |> MBytes.of_string in
  let secret_key =
    match Data_encoding.Binary.of_bytes Ed25519.secret_key_encoding raw_secret_key with
    | None -> assert false
    | Some v -> v in
  let public_key =
    match Data_encoding.Binary.of_bytes Ed25519.public_key_encoding raw_public_key with
    | None -> assert false
    | Some v -> v in
  (* check that keys correspond *)
  let bytes = MBytes.of_string "some test text" in
  let signature = Ed25519.sign secret_key bytes in
  assert (Ed25519.check_signature public_key signature bytes) ;
  let public_key_hash = Ed25519.hash public_key in
  { public_key_hash ; public_key ; secret_key }

let accounts = [
  make
    ~raw_public_key:
      "000000204798D2CC98473D7E250C898885718AFD2E4EFBCB1A1595AB9730761ED830DE0F"
    ~raw_secret_key:
      "000000408500C86780141917FCD8AC6A54A43A9EEDA1ABA9D263CE5DEC5A1D0E5DF1E598\
       4798D2CC98473D7E250C898885718AFD2E4EFBCB1A1595AB9730761ED830DE0F" ;
  make
    ~raw_public_key:
      "000000202dc050925cf3a80c0d0fd4589e1d86e2a4e07118e29458a537ed6382cb697d97"
    ~raw_secret_key:
      "000000403f6aa02bc3cf23d7d4955f3d2708c84368372779aca1cfe013def93cf15dfcdb\
       2dc050925cf3a80c0d0fd4589e1d86e2a4e07118e29458a537ed6382cb697d97" ;
  make
    ~raw_public_key:
      "000000206b6aa000041caa65d1df72354d329beae2a782c59021f25c6f40bf4a88781c1b"
    ~raw_secret_key:
      "00000040c56dcb77f1fff00d1a1f5330a77a9f1f31cf70fa7ad691a22b5ec28cdb232350\
       6b6aa000041caa65d1df72354d329beae2a782c59021f25c6f40bf4a88781c1b" ;
  make
    ~raw_public_key:
      "0000002050e67edf7dbff2c9a45f0bfae892964c67c61472a74d3ab1e51aa009611c788f"
    ~raw_secret_key:
      "000000401fa3088f39928af52331654f0d9234787f345988a4ee46b619b94d8ad5405dc8\
       50e67edf7dbff2c9a45f0bfae892964c67c61472a74d3ab1e51aa009611c788f" ;
  make
    ~raw_public_key:
      "00000020c34b689f812ccca41c114a123aa44f55846fec7eb956b6b852d2d19003e63165"
    ~raw_secret_key:
      "00000040e4104362f6db39d47aa1a85bd0d5b54b712f6d8c603c0c81bf01b42123d0d9b9\
       c34b689f812ccca41c114a123aa44f55846fec7eb956b6b852d2d19003e63165" ;
]

let init_account ctxt account =
  Storage.Public_key.init ctxt account.public_key_hash account.public_key >>=? fun ctxt ->
  Contract_storage.credit
    ctxt
    (Contract_repr.default_contract account.public_key_hash)
    wealth >>=? fun ctxt ->
  return ctxt

let init ctxt =
  fold_left_s init_account ctxt accounts >>=? fun ctxt ->
  return ctxt

let account_encoding =
  let open Data_encoding in
  conv
    (fun {public_key_hash ; public_key ; secret_key } ->
       (public_key_hash, public_key, secret_key))
    (fun (public_key_hash, public_key, secret_key) ->
       { public_key_hash ; public_key ; secret_key })
    (obj3
       (req "publicKeyHash" Ed25519.Public_key_hash.encoding)
       (req "publicKey" Ed25519.public_key_encoding)
       (req "secretKey" Ed25519.secret_key_encoding))

let refill ctxt =
  fold_left_s
    (fun ctxt account ->
       let contract =
         Contract_repr.default_contract account.public_key_hash in
       Contract_storage.get_balance ctxt contract >>=? fun balance ->
       match Tez_repr.(wealth -? balance) with
       | Error _ -> return ctxt
       | Ok tez -> Contract_storage.credit ctxt contract tez)
    ctxt
    accounts
